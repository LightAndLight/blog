{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Blog.Build
  ( rules
  , Rules
  , evalRules
  , rule
  , Resource (..)
  , Depends
  , resource
  , resourceType
  , Action
  , setDependencies
  )
where

import Blog
  ( MetadataValue
  , ResourceId (ResourceId)
  , metadataValueString
  , readResourceId
  , renderResourceId
  , resourceIdParser
  , resourceName
  )
import Blog.Diagnostic (DiagnosticReports (..), sageErrorReport, tomlResult)
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
import Blog.Resource
  ( createResource
  , doesResourceExist
  , getResourceType
  , listResource
  , lookupResource
  , updateResource
  )
import Commonmark.Pandoc (Cm, unCm)
import Commonmark.Parser (commonmark)
import Control.Applicative ((<|>))
import Control.Exception (catch, throwIO)
import Control.Monad (guard, unless)
import Control.Monad.Error.Class (MonadError, liftEither, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Class (get, put)
import Control.Monad.State.Strict (evalStateT, modify)
import Control.Monad.Writer.CPS (WriterT, runWriterT)
import Control.Monad.Writer.Class (tell)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid (Any (..))
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
import qualified IO
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import qualified Temple
import Text.Pandoc.Builder (Blocks)
import qualified Text.Pandoc.Builder as Blocks (toList)
import qualified Text.Pandoc.Html as Html
import qualified Toml

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
    x : y : z : rest -> go x y z rest
  where
    go prev current next rest =
      Adjacency (Just prev) current (Just next)
        : case rest of
          [] -> [Adjacency (Just current) next Nothing]
          next' : rest' -> go current next next' rest'

rules :: Rules
rules =
  rule
    "template-dependency"
    (resourceType "template")
    ( \templates ->
        for_ (resourcesData templates) $ \template -> do
          template' <-
            case Temple.parse (resourcePath template) (LazyByteString.toStrict $ resourceContent template) of
              Left err -> error "TODO: " err
              Right x -> pure x
          let templateDependencies = getTemplateDependencies template'
          setDependencies (resourceId template) templateDependencies
    )
    <> rule
      "article-adjacency"
      (resourceType "article")
      ( \articles -> do
          dataDir <- askDataDir
          mResTy <- getResourceType dataDir $ fromString (resourcesType articles)
          (resTyDir, resTy) <- maybe (error $ resourcesType articles ++ " does not exist") pure mResTy
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
            let Adjacency prev current next = fmap fst adjacency
            let
              -- TODO: string escaping, move to `tomlin` library
              tomlString = (fromString "\"" <>) . (<> fromString "\"")
            putResource
              (ResourceId "adjacency" $ renderResourceId current)
              ( foldMap (<> fromString "\n") $
                  [ fromString "previous = " <> tomlString (fromString $ renderResourceId resId) | Just resId <- [prev]
                  ]
                    ++ [fromString "next = " <> tomlString (fromString $ renderResourceId resId) | Just resId <- [next]]
              )
      )
    <> rule
      "article-html"
      ( (,,,)
          <$> resource "config" "base-url"
          <*> resourceType "article"
          <*> resourceType "adjacency"
          <*> resource "template" "article.html.temple"
      )
      ( \(baseUrl, articles, adjacencies, template) -> do
          let
            -- TODO: can I make the rule dependency perform this "inner join"?
            articlesWithAdjacencies =
              [ (article, adjacency)
              | article <- resourcesData articles
              , adjacency <- resourcesData adjacencies
              , renderResourceId (resourceId article) == resourceName (resourceId adjacency)
              ]

          unless (null articlesWithAdjacencies) $ do
            let
              baseUrl' =
                LazyText.toStrict
                  . LazyText.strip
                  . Text.Lazy.Encoding.decodeUtf8
                  $ resourceContent baseUrl

            let templatePath = resourcePath template
            let templateContent = resourceContent template
            template' <-
              case Temple.parse templatePath (LazyByteString.toStrict templateContent) of
                Left err -> throwError $ DiagnosticReports (fromString templatePath) templateContent (sageErrorReport err)
                Right x -> pure x
            (deps, bindings) <- do
              result <- runExceptT $ Temple.inferBindings (resourcePath template) template'
              case result of
                Left err -> error "TODO: " err
                Right x -> pure x

            for_ articlesWithAdjacencies $ \(article, adjacency) -> do
              articleHtml <- do
                let articleContent = resourceContent article
                articleContent' <-
                  case Text.Lazy.Encoding.decodeUtf8' articleContent of
                    Left err -> error $ "TODO: " ++ show err
                    Right x -> pure $! LazyText.toStrict x
                markdown <-
                  case commonmark ("(" ++ renderResourceId (resourceId article) ++ ")") articleContent' of
                    Left err -> error $ "TODO: " ++ show err
                    Right x -> pure $ unCm (x :: Cm () Blocks)
                Html.runRenderT Html.emptyNotesState . Html.renderBlocks $ Blocks.toList markdown

              (prev, next) <- do
                let adjacencyFile = fromString $ "(" ++ renderResourceId (resourceId adjacency) ++ ")"
                let adjacencyContent = resourceContent adjacency
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
                    dataDir <- askDataDir
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
                                          $ resourceMetadata article
                                   )
                                 ,
                                   ( fromString "content"
                                   , let path'' = path' <> pathItem (RecordField $ fromString "content")
                                     in Temple.Located path'' $
                                          Temple.String [Temple.PartText . LazyText.toStrict $ Builder.toLazyText articleHtml]
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
                        renderResourceId (resourceId template)
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
                                (renderResourceId (resourceId article) ++ " is missing fields:")
                                  : fmap (\(field', ty') -> "  " ++ Text.unpack field' ++ " : " ++ Temple.renderType ty') fields
                            Temple.TypeMismatch _ expected actual ->
                              unlines
                                [ renderResourceId (resourceId article) ++ " has a type error in " ++ renderPath rest ++ ":"
                                , "  expected " ++ Temple.renderType expected ++ ", got " ++ Temple.renderType actual
                                ]
                            _ ->
                              error $ "type error (TODO): " ++ show err
                      _ ->
                        error $ "type error (TODO): " ++ show err
                  Right (_s, ()) -> do
                    let env = Temple.defaultEvalEnv (resourcePath template) mempty
                    let !value = Temple.evalExpr env $ Temple.locatedVal expr
                    pure (name, value)

              let
                env = Temple.defaultEvalEnv (resourcePath template) deps
                output =
                  Temple.evalTemplate
                    env{Temple.eeScope = Map.fromList bindings' <> Temple.eeScope env}
                    template'

              let
                htmlName =
                  let ResourceId resTyName resName = resourceId article
                  in resTyName ++ "-" ++ resName
              putResource (ResourceId "html" htmlName) output
      )

newtype Rules = Rules [Rule]
  deriving (Semigroup, Monoid)

data Rule = forall a. Rule !String (Depends a) (a -> Action ())

matchRule :: Rule -> ResourceId -> Maybe (Action ())
matchRule (Rule name deps f) resId = do
  let (Any match, action) = matchDepends deps resId
  guard match
  pure $ do
    trace $ "rule: " ++ name
    action >>= f

newtype Action a = Action (ReaderT ActionEnv (WriterT [ResourceId] (ExceptT DiagnosticReports IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError DiagnosticReports)

data ActionEnv
  = ActionEnv
  { aeTrace :: !(String -> IO ())
  , aeDataDir :: !FilePath
  }

runAction ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Trace
  (String -> IO ()) ->
  -- | Data directory
  FilePath ->
  Action a ->
  m ([ResourceId], a)
runAction fTrace dataDir (Action ma) = do
  let env = ActionEnv{aeTrace = fTrace, aeDataDir = dataDir}
  (a, pending) <- liftEither =<< liftIO (runExceptT . runWriterT . flip runReaderT env $ ma)
  pure (pending, a)

askDataDir :: Action FilePath
askDataDir = Action $ asks aeDataDir

trace :: String -> Action ()
trace s = Action $ do
  f <- asks aeTrace
  liftIO $ f s

getDependents ::
  -- | Data directory
  FilePath ->
  ResourceId ->
  IO [ResourceId]
getDependents dataDir (ResourceId resTyName resName) = do
  dependents <-
    listDirectory (dataDir </> resTyName </> (resName ++ ".d") </> "dependents")
      `catch` \err -> if isDoesNotExistError err then pure [] else throwIO err
  pure $ fmap readResourceId dependents

setDependencies :: ResourceId -> Set ResourceId -> Action ()
setDependencies a bs = do
  dataDir <- askDataDir

  for_ bs $ \b -> do
    let ResourceId resTyName resName = b
    let resTyDir = dataDir </> resTyName
    exists <- liftIO $ doesResourceExist resTyDir resName
    unless exists . throwError . DiagnosticSimple $
      "dependency " ++ renderResourceId b ++ " does not exist"

  resDependenciesPath <- do
    let ResourceId resTyName resName = a
    pure $ dataDir </> resTyName </> (resName ++ ".d") </> "dependencies"

  dependencies <-
    liftIO $
      listDirectory resDependenciesPath
        `catch` \err -> if isDoesNotExistError err then pure [] else throwIO err
  for_ dependencies $ \dependency -> do
    let b = readResourceId dependency

    liftIO . removeFile $ resDependenciesPath </> dependency

    resDependentsPath <- do
      let ResourceId resTyName resName = b
      pure $ dataDir </> resTyName </> (resName ++ ".d") </> "dependents"
    liftIO $
      removeFile (resDependentsPath </> renderResourceId a)
        `catch` \err -> unless (isDoesNotExistError err) $ throwIO err

  liftIO $ createDirectoryIfMissing True resDependenciesPath
  for_ bs $ \b -> do
    liftIO $ IO.writeFile (resDependenciesPath </> renderResourceId b) mempty

    resDependentsPath <- do
      let ResourceId resTyName resName = b
      pure $ dataDir </> resTyName </> (resName ++ ".d") </> "dependents"
    liftIO $ do
      createDirectoryIfMissing True resDependentsPath
      IO.writeFile (resDependentsPath </> renderResourceId a) mempty

putResource :: ResourceId -> LazyByteString -> Action ()
putResource resId@(ResourceId resTyName resName) content = do
  dataDir <- askDataDir
  mResTy <- getResourceType dataDir $ fromString resTyName
  (resTyDir, resTy) <-
    case mResTy of
      Nothing ->
        throwError . DiagnosticSimple $
          "resource type " ++ resTyName ++ " does not exist"
      Just x ->
        pure x
  exists <- liftIO $ doesResourceExist resTyDir resName
  if exists
    then updateResource dataDir resTy resName content
    else createResource dataDir resTy resName content
  Action $ tell [resId]

rule ::
  -- | ID
  String ->
  Depends a ->
  -- | Action
  (a -> Action ()) ->
  Rules
rule name deps f = Rules [Rule name deps f]

data Depends :: Type -> Type where
  DFmap :: (a -> b) -> Depends a -> Depends b
  DPure :: a -> Depends a
  DApply :: Depends (a -> b) -> Depends a -> Depends b
  DResourceType :: String -> Depends Resources
  DResource :: ResourceId -> Depends Resource

instance Functor Depends where
  fmap = DFmap

instance Applicative Depends where
  pure = DPure
  (<*>) = DApply

matchDepends :: Depends a -> ResourceId -> (Any, Action a)
matchDepends (DFmap f deps) resId =
  (fmap . fmap . fmap) f (matchDepends deps) resId
matchDepends (DPure a) _resId =
  (mempty, pure a)
matchDepends (DApply deps deps') resId =
  liftA2 (<*>) (matchDepends deps resId) (matchDepends deps' resId)
matchDepends (DResourceType resTyName) resId@(ResourceId resTyName' _resName) =
  if resTyName == resTyName'
    then (Any True, Resources resTyName . pure <$> makeResource resId)
    else (Any False, makeResources resTyName)
matchDepends (DResource resId) resId' =
  (Any $ resId == resId', makeResource resId)

data Resources
  = Resources
  { resourcesType :: !String
  , resourcesData :: ![Resource]
  }

data Resource
  = Resource
  { resourceId :: !ResourceId
  , resourcePath :: !FilePath
  , resourceMetadata :: !(Map Text MetadataValue)
  , resourceContent :: LazyByteString
  }

makeResource :: ResourceId -> Action Resource
makeResource resId@(ResourceId resTyName resName) = do
  dataDir <- askDataDir
  (resTyDir, resTy) <-
    maybe (throwError . DiagnosticSimple $ "resource type '" ++ resTyName ++ "' does not exist") pure
      =<< getResourceType dataDir (fromString resTyName)
  let resPath = resTyDir </> resName
  mContent <- liftIO $ lookupResource resTyDir resName
  case mContent of
    Nothing ->
      throwError . DiagnosticSimple $ "resource " ++ renderResourceId resId ++ " does not exist"
    Just content -> do
      metadata <- do
        mMetadataContent <- liftIO $ lookupResourceMetadata resTyDir resName
        maybe (pure mempty) (parseResourceMetadata resTy resName) mMetadataContent
      pure
        Resource
          { resourceId = resId
          , resourcePath = resPath
          , resourceMetadata = metadata
          , resourceContent = content
          }

makeResources ::
  -- | Resource type name
  String ->
  Action Resources
makeResources resTyName = do
  dataDir <- askDataDir
  (resTyDir, resTy) <-
    maybe (error $ "resource type " ++ resTyName ++ " does not exist") pure
      =<< getResourceType dataDir (fromString resTyName)

  resNames <- liftIO $ listDirectory resTyDir
  resources <-
    fmap catMaybes . for resNames $ \resName -> do
      let resPath = resTyDir </> resName
      exists <- liftIO $ doesFileExist resPath
      if exists
        then do
          let resId = ResourceId resTyName resName
          metadata <- do
            mMetadataContent <- liftIO $ lookupResourceMetadata resTyDir resName
            maybe (pure mempty) (parseResourceMetadata resTy resName) mMetadataContent
          content <- liftIO $ IO.readFile resPath
          pure $
            Just
              Resource
                { resourceId = resId
                , resourcePath = resPath
                , resourceMetadata = metadata
                , resourceContent = content
                }
        else
          pure Nothing
  pure $ Resources resTyName resources

resource ::
  -- | Resource type name
  String ->
  -- | Resource name
  String ->
  Depends Resource
resource resTyName = DResource . ResourceId resTyName

resourceType ::
  -- | Resource type
  String ->
  Depends Resources
resourceType = DResourceType

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

evalRules ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Trace
  (String -> IO ()) ->
  -- | Data directory
  FilePath ->
  Rules ->
  -- | The created/updated resource
  ResourceId ->
  m ()
evalRules fTrace dataDir (Rules rs) resId = do
  flip evalStateT [] $ go resId
  where
    go resId' = do
      for_ rs $ \r -> for_ (matchRule r resId') $ \action -> do
        (pending, ()) <- runAction fTrace dataDir action
        modify (++ pending)

      dependents <- liftIO $ getDependents dataDir resId'
      pending <- get

      case dependents ++ pending of
        [] -> pure ()
        resId'' : pending'' -> put pending'' *> go resId''
