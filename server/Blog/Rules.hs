{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Blog.Rules (rules) where

import Blog
  ( MetadataValue
  , ResourceId (..)
  , metadataValueString
  , renderResourceId
  , resourceIdParser
  )
import Blog.Build ((*<))
import qualified Blog.Build as Build
import Blog.Diagnostic (DiagnosticReports (..), sageErrorReport, templeTypeErrorReport, tomlResult)
import Blog.Metadata
  ( Path
  , PathItem (..)
  , lookupResourceMetadata
  , metadataValueToTempleExpr
  , parseResourceMetadata
  , pathItem
  , pathUncons
  , renderPath
  )
import Blog.Resource (getResourceType, listResource)
import Commonmark.Pandoc (Cm, unCm)
import Commonmark.Parser (commonmark)
import Control.Applicative ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Foldable (for_)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Traversable (for)
import qualified Temple
import Text.Pandoc.Builder (Blocks)
import qualified Text.Pandoc.Builder as Blocks (toList)
import qualified Text.Pandoc.Html as Html
import qualified Toml

rules :: Build.Rules
rules =
  Build.rule "template-dependency" (Build.iResourceType "template") (pure ()) templateDependency
    <> Build.rule
      "article-adjacency"
      (Build.iResourceTypeAll "article")
      (Build.oResource "adjacency" $ Build.oMatch "article-" *< Build.oAny)
      articleAdjacency
    <> Build.rule
      "article-html"
      ( (,,,)
          <$> Build.iResource "config" (Build.iMatch "base-url")
          <*> Build.iResource "template" (Build.iMatch "article.html.temple")
          <*> Build.iResource "article" (Build.iBind "name")
          <*> Build.iResource "adjacency" (Build.iMatch "article-" <> Build.iBind "name")
      )
      (Build.oResource "html" $ Build.oMatch "article-" *< Build.oBind "name")
      articleHtml

templateDependency ::
  Build.ResourceInput ->
  () ->
  Build.Action ()
templateDependency template () = do
  template' <-
    case Temple.parse
      (Build.resourceInputPath template)
      (LazyByteString.toStrict $ Build.resourceInputContent template) of
      Left err ->
        throwError $
          DiagnosticReports
            (fromString $ Build.resourceInputPath template)
            (Build.resourceInputContent template)
            (sageErrorReport err)
      Right x -> pure x
  let templateDependencies = getTemplateDependencies template'
  Build.setDependencies (Build.resourceInputId template) templateDependencies

getTemplateDependencies :: Temple.Template loc -> Set ResourceId
getTemplateDependencies template =
  case template of
    Temple.TemplateBase _ parts ->
      foldMap partDependencies parts
    Temple.TemplateChild _ parent pragmas ->
      Set.insert (ResourceId "template" $ Temple.locatedVal parent) $ foldMap pragmaDependencies pragmas
  where
    partDependencies :: Temple.Part loc -> Set ResourceId
    partDependencies part =
      case part of
        Temple.PartText{} -> mempty
        Temple.PartExpr expr -> exprDependencies $ Temple.locatedVal expr
        Temple.PartExprStream expr -> exprDependencies $ Temple.locatedVal expr
        Temple.PartInclude file bindings ->
          Set.insert (ResourceId "template" . Text.unpack $ Temple.locatedVal file) $
            (foldMap . foldMap) (\(_name, expr) -> exprDependencies $ Temple.locatedVal expr) bindings

    pragmaDependencies :: Temple.Pragma loc -> Set ResourceId
    pragmaDependencies pragma =
      case pragma of
        Temple.PragmaBlock _name parts ->
          foldMap partDependencies parts
        Temple.PragmaWith bindings ->
          foldMap (\(_name, expr) -> exprDependencies $ Temple.locatedVal expr) bindings

    exprDependencies :: Temple.Expr loc -> Set ResourceId
    exprDependencies expr =
      case expr of
        Temple.Var{} -> mempty
        Temple.Bool{} -> mempty
        Temple.String parts -> foldMap partDependencies parts
        Temple.MultilineString parts -> foldMap partDependencies parts
        Temple.Call _name args -> foldMap (exprDependencies . Temple.locatedVal) args
        Temple.Record fields -> (foldMap . foldMap) (exprDependencies . Temple.locatedVal) fields
        Temple.Field expr' field ->
          exprDependencies (Temple.locatedVal expr')
            <> case field of
              Temple.FStatic{} -> mempty
              Temple.FDynamic expr'' -> exprDependencies $ Temple.locatedVal expr''
        Temple.Constructor _name args -> foldMap (exprDependencies . Temple.locatedVal) args
        Temple.Match expr' branches ->
          exprDependencies (Temple.locatedVal expr')
            <> foldMap (\(Temple.Branch _pattern body) -> exprDependencies $ Temple.locatedVal body) branches
        Temple.IfThenElse cond th el ->
          exprDependencies (Temple.locatedVal cond)
            <> exprDependencies (Temple.locatedVal th)
            <> exprDependencies (Temple.locatedVal el)
        Temple.Array items -> foldMap (exprDependencies . Temple.locatedVal) items
        Temple.For _name items yield ->
          exprDependencies (Temple.locatedVal items)
            <> exprDependencies (Temple.locatedVal yield)

articleAdjacency ::
  Build.ResourceInputs ->
  Build.ResourceOutput String ->
  Build.Action ()
articleAdjacency iArticles oAdjacency = do
  dataDir <- Build.askDataDir
  (resTyDir, resTy) <- do
    let resTyName = Build.resourceInputsType iArticles
    mResTy <- getResourceType dataDir $ fromString resTyName
    maybe (error $ resTyName ++ " does not exist") pure mResTy
  articles' <- listResource resTyDir resTy
  articlesWithPublished <- for articles' $ \article -> do
    metadata <- do
      mMetadata <- liftIO $ lookupResourceMetadata resTyDir $ resourceName article
      case mMetadata of
        Nothing ->
          throwError . DiagnosticSimple $ renderResourceId article ++ " has no metadata"
        Just metadata ->
          parseResourceMetadata resTy (resourceName article) metadata

    published <- do
      input <- case Map.lookup (fromString "published") metadata of
        Nothing ->
          throwError . DiagnosticSimple $ renderResourceId article ++ "'s metadata has no 'published' field"
        Just x ->
          pure x
      let !input' = metadataValueString input
      let
        parseDateTime :: Text -> Maybe UTCTime
        parseDateTime = iso8601ParseM . Text.unpack

        parseDate :: Text -> Maybe UTCTime
        parseDate x = do
          day <- iso8601ParseM $ Text.unpack x
          pure $ UTCTime day 0
      case parseDateTime input' <|> parseDate input' of
        Nothing ->
          throwError . DiagnosticSimple $
            renderResourceId article ++ "'s metadata an invalid 'published' field: " ++ Text.unpack input'
        Just x -> pure (x :: UTCTime)
    pure (article, published)

  let sortedArticles = sortOn snd articlesWithPublished
  let adjacencies = makeAdjacencies sortedArticles

  for_ adjacencies $ \adjacency -> do
    let Adjacency prev (ResourceId _ currentResName) next = fmap fst adjacency
    let
      -- TODO: string escaping, move to `tomlin` library
      tomlString = (fromString "\"" <>) . (<> fromString "\"")
    Build.writeResource
      oAdjacency
      currentResName
      ( foldMap (<> fromString "\n") $
          [ fromString "previous = " <> tomlString (fromString $ renderResourceId resId) | Just resId <- [prev]
          ]
            ++ [fromString "next = " <> tomlString (fromString $ renderResourceId resId) | Just resId <- [next]]
      )

data Adjacency a
  = Adjacency
      -- | Previous
      (Maybe a)
      -- | Current
      a
      -- | Next
      (Maybe a)
  deriving (Functor)

makeAdjacencies :: [a] -> [Adjacency a]
makeAdjacencies xs =
  case xs of
    [] -> []
    [x] -> [Adjacency Nothing x Nothing]
    [x, y] -> [Adjacency Nothing x (Just y), Adjacency (Just x) y Nothing]
    x : y : z : rest -> Adjacency Nothing x (Just y) : go x y z rest
  where
    go prev current next rest =
      Adjacency (Just prev) current (Just next)
        : case rest of
          [] -> [Adjacency (Just current) next Nothing]
          next' : rest' -> go current next next' rest'

articleHtml ::
  (Build.ResourceInput, Build.ResourceInput, Build.ResourceInput, Build.ResourceInput) ->
  Build.ResourceOutput () ->
  Build.Action ()
articleHtml (iBaseUrl, iTemplate, iArticle, iAdjacency) oHtml = do
  -- TODO: process baseUrl and template only once
  -- TODO: push article/adjacency name matching into rule dependencies
  let
    baseUrl' =
      LazyText.toStrict
        . LazyText.strip
        . Text.Lazy.Encoding.decodeUtf8
        $ Build.resourceInputContent iBaseUrl

  let templatePath = Build.resourceInputPath iTemplate
  let templateContent = Build.resourceInputContent iTemplate
  template' <-
    case Temple.parse templatePath (LazyByteString.toStrict templateContent) of
      Left err -> throwError $ DiagnosticReports (fromString templatePath) templateContent (sageErrorReport err)
      Right x -> pure x
  (deps, bindings) <- do
    result <- runExceptT $ Temple.inferBindings (Build.resourceInputPath iTemplate) template'
    case result of
      Left err -> do
        reports <- liftIO $ templeTypeErrorReport err
        throwError $
          DiagnosticReports
            (fromString $ Build.resourceInputPath iTemplate)
            (Build.resourceInputContent iTemplate)
            reports
      Right x -> pure x

  html <- do
    let articleContent = Build.resourceInputContent iArticle
    articleContent' <-
      case Text.Lazy.Encoding.decodeUtf8' articleContent of
        Left err -> error $ "TODO: " ++ show err
        Right x -> pure $! LazyText.toStrict x
    markdown <-
      case commonmark ("(" ++ renderResourceId (Build.resourceInputId iArticle) ++ ")") articleContent' of
        Left err -> error $ "TODO: " ++ show err
        Right x -> pure $ unCm (x :: Cm () Blocks)
    Html.runRenderT Html.emptyNotesState . Html.renderBlocks $ Blocks.toList markdown

  (prev, next) <- do
    let adjacencyFile = fromString $ "(" ++ renderResourceId (Build.resourceInputId iAdjacency) ++ ")"
    let adjacencyContent = Build.resourceInputContent iAdjacency
    toml <-
      tomlResult adjacencyFile adjacencyContent . Toml.parse $ LazyByteString.toStrict adjacencyContent
    let
      decoder =
        (,)
          <$> Toml.optionalKey (fromString "previous") (Toml.pstring resourceIdParser)
          <*> Toml.optionalKey (fromString "next") (Toml.pstring resourceIdParser)
    (prev, next) <- tomlResult adjacencyFile adjacencyContent $ Toml.decode toml decoder

    let
      getAdjacencyFields resId@(ResourceId resTyName resName) = do
        dataDir <- Build.askDataDir
        mResTy <- getResourceType dataDir $ fromString resTyName
        (resTyDir, resTy) <- maybe (error $ "resource type " ++ resTyName ++ " does not exist") pure mResTy
        mContent <- liftIO $ lookupResourceMetadata resTyDir resName
        content <- maybe (error $ "resource " ++ renderResourceId resId ++ " has no metadata") pure mContent
        metadata <- parseResourceMetadata resTy resName content
        pure $
          [(fromString "title", prev') | Just prev' <- [Map.lookup (fromString "title") metadata]]
            ++ [(fromString "url", next') | Just next' <- [Map.lookup (fromString "url") metadata]]

    prev' <- traverse getAdjacencyFields prev
    next' <- traverse getAdjacencyFields next

    pure (prev', next')

  let
    adjacencyExpr :: Path -> Maybe [(Text, MetadataValue)] -> Temple.LExpr Path
    adjacencyExpr path adj =
      Temple.Located path $
        case adj of
          Nothing ->
            Temple.Constructor (fromString "None") []
          Just fields ->
            let path' = path <> pathItem (ConstructorArg (fromString "Some") 0)
            in Temple.Constructor
                 (fromString "Some")
                 [ Temple.Located path' $
                     Temple.Record $
                       fmap
                         ( \(key, value) ->
                             let path'' = path' <> pathItem (RecordField key)
                             in ( key
                                , Temple.Located path'' $ metadataValueToTempleExpr path'' value
                                )
                         )
                         fields
                 ]

    exprs :: Map Text (Temple.LExpr Path)
    exprs =
      let
        path = mempty
      in
        Map.fromList
          [
            ( fromString "root"
            , let path' = path <> pathItem (RecordField $ fromString "root")
              in Temple.Located path' $
                   Temple.String [Temple.PartText baseUrl']
            )
          ,
            ( fromString "self"
            , let path' = path <> pathItem (RecordField $ fromString "self")
              in Temple.Located path' $
                   Temple.Record
                     [
                       ( fromString "metadata"
                       , let path'' = path' <> pathItem (RecordField $ fromString "metadata")
                         in Temple.Located path''
                              . Temple.Record
                              . fmap
                                ( \(key, value) ->
                                    let path''' = path'' <> pathItem (RecordField key)
                                    in (key, Temple.Located path''' $ metadataValueToTempleExpr path''' value)
                                )
                              . Map.toList
                              $ Build.resourceInputMetadata iArticle
                       )
                     ,
                       ( fromString "content"
                       , let path'' = path' <> pathItem (RecordField $ fromString "content")
                         in Temple.Located path'' $
                              Temple.String [Temple.PartText . LazyText.toStrict $ Builder.toLazyText html]
                       )
                     ,
                       ( fromString "previous"
                       , let path'' = path' <> pathItem (RecordField $ fromString "previous")
                         in adjacencyExpr path'' prev
                       )
                     ,
                       ( fromString "next"
                       , let path'' = path' <> pathItem (RecordField $ fromString "next")
                         in adjacencyExpr path'' next
                       )
                     ]
            )
          ]

  bindings' <- for bindings $ \binding -> do
    let name = Temple.bindingName binding
    let tyScheme = Temple.bindingScheme binding
    expr <-
      case Map.lookup name exprs of
        Nothing ->
          throwError . DiagnosticSimple $
            renderResourceId (Build.resourceInputId iTemplate)
              ++ " has unsatisfied template parameter "
              ++ Text.unpack name
              ++ " : "
              ++ Temple.renderTypeScheme tyScheme
        Just x ->
          pure x

    result <-
      Temple.runInferT (Temple.emptyInferEnv ".") Temple.emptyInferState $ do
        ty <- Temple.instantiateTypeScheme tyScheme
        Temple.checkExpr Temple.checkPartIncludeDisabled expr ty

    case result of
      Left err ->
        case pathUncons $ Temple.typeErrorLoc err of
          Just (RecordField field, rest) | field == fromString "self" ->
            throwError . DiagnosticSimple $
              case err of
                Temple.MissingFields _ fields ->
                  unlines $
                    (renderResourceId (Build.resourceInputId iArticle) ++ " is missing fields:")
                      : fmap (\(field', ty') -> "  " ++ Text.unpack field' ++ " : " ++ Temple.renderType ty') fields
                Temple.TypeMismatch _ expected actual ->
                  unlines
                    [ renderResourceId (Build.resourceInputId iArticle)
                        ++ " has a type error in "
                        ++ renderPath rest
                        ++ ":"
                    , "  expected " ++ Temple.renderType expected ++ ", got " ++ Temple.renderType actual
                    ]
                _ ->
                  error $ "type error (TODO): " ++ show err
          _ ->
            error $ "type error (TODO): " ++ show err
      Right (_s, ()) -> do
        let env = Temple.defaultEvalEnv (Build.resourceInputPath iTemplate) mempty
        let !value = Temple.evalExpr env $ Temple.locatedVal expr
        pure (name, value)

  let
    env = Temple.defaultEvalEnv (Build.resourceInputPath iTemplate) deps
    output =
      Temple.evalTemplate
        env{Temple.eeScope = Map.fromList bindings' <> Temple.eeScope env}
        template'

  Build.writeResource oHtml () output
