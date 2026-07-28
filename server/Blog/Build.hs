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
  , Change(..)
  , renderChange
  , Status(..)
  , Reason(..)
  , evalRules
  , rule
  , Input
  , Resources(..)
  , Resource (..)
  , iResource
  , iResourceType
  , Output
  , WriteResourceNamed
  , writeResourceNamed
  , WriteResource
  , writeResource
  , oResource
  , oResourceType
  , Action
  , setDependencies
  , trace
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
import Blog.Diagnostic (DiagnosticReports (..), sageErrorReport, tomlResult, templeTypeErrorReport)
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
import Control.Monad (guard, unless, when)
import Control.Monad.Error.Class (MonadError, liftEither, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Class (get, MonadState)
import Control.Monad.State.Strict (evalStateT, modify)
import Control.Monad.Writer.CPS (WriterT, runWriterT, execWriterT)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Foldable (for_, traverse_)
import Data.Kind (Type)
import Data.List (sortOn, intercalate, nub, partition)
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
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import qualified Temple
import Text.Pandoc.Builder (Blocks)
import qualified Text.Pandoc.Builder as Blocks (toList)
import qualified Text.Pandoc.Html as Html
import qualified Toml
import Data.Graph (graphFromEdges, topSort)

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
    (iResourceType "template")
    (pure ())
    ( \template () -> do
        template' <-
          case Temple.parse (resourcePath template) (LazyByteString.toStrict $ resourceContent template) of
            Left err ->
              throwError $
              DiagnosticReports
                (fromString $ resourcePath template) (resourceContent template) (sageErrorReport err)
            Right x -> pure x
        let templateDependencies = getTemplateDependencies template'
        setDependencies (resourceId template) templateDependencies
    )
    <> rule
      "article-adjacency"
      (iResourceType "article")
      (oResourceType "adjacency")
      ( \iArticle oAdjacency -> do
          dataDir <- askDataDir
          (resTyDir, resTy) <- do
            let ResourceId resTyName _resName = resourceId iArticle
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
            let Adjacency prev current next = fmap fst adjacency
            let
              -- TODO: string escaping, move to `tomlin` library
              tomlString = (fromString "\"" <>) . (<> fromString "\"")
            writeResourceNamed
              oAdjacency
              (renderResourceId current)
              ( foldMap (<> fromString "\n") $
                  [ fromString "previous = " <> tomlString (fromString $ renderResourceId resId) | Just resId <- [prev]
                  ]
                    ++ [fromString "next = " <> tomlString (fromString $ renderResourceId resId) | Just resId <- [next]]
              )
      )
    <> rule
      "article-html"
      ( (,,,)
          <$> iResource "config" "base-url"
          <*> iResource "template" "article.html.temple"
          <*> iResourceType "article"
          <*> iResourceType "adjacency"
      )
      (oResourceType "html")
      ( \(iBaseUrl, iTemplate, iArticle, iAdjacency) oHtml ->
        when (renderResourceId (resourceId iArticle) == resourceName (resourceId iAdjacency)) $ do
            -- TODO: process baseUrl and template only once
            -- TODO: push article/adjacency name matching into rule dependencies
            let
              baseUrl' =
                LazyText.toStrict
                  . LazyText.strip
                  . Text.Lazy.Encoding.decodeUtf8
                  $ resourceContent iBaseUrl

            let templatePath = resourcePath iTemplate
            let templateContent = resourceContent iTemplate
            template' <-
              case Temple.parse templatePath (LazyByteString.toStrict templateContent) of
                Left err -> throwError $ DiagnosticReports (fromString templatePath) templateContent (sageErrorReport err)
                Right x -> pure x
            (deps, bindings) <- do
              result <- runExceptT $ Temple.inferBindings (resourcePath iTemplate) template'
              case result of
                Left err -> do
                  reports <- liftIO $ templeTypeErrorReport err
                  throwError $
                    DiagnosticReports
                      (fromString $ resourcePath iTemplate) (resourceContent iTemplate) reports
                Right x -> pure x

            articleHtml <- do
              let articleContent = resourceContent iArticle
              articleContent' <-
                case Text.Lazy.Encoding.decodeUtf8' articleContent of
                  Left err -> error $ "TODO: " ++ show err
                  Right x -> pure $! LazyText.toStrict x
              markdown <-
                case commonmark ("(" ++ renderResourceId (resourceId iArticle) ++ ")") articleContent' of
                  Left err -> error $ "TODO: " ++ show err
                  Right x -> pure $ unCm (x :: Cm () Blocks)
              Html.runRenderT Html.emptyNotesState . Html.renderBlocks $ Blocks.toList markdown

            (prev, next) <- do
              let adjacencyFile = fromString $ "(" ++ renderResourceId (resourceId iAdjacency) ++ ")"
              let adjacencyContent = resourceContent iAdjacency
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
                                        $ resourceMetadata iArticle
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
                      renderResourceId (resourceId iTemplate)
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
                              (renderResourceId (resourceId iArticle) ++ " is missing fields:")
                                : fmap (\(field', ty') -> "  " ++ Text.unpack field' ++ " : " ++ Temple.renderType ty') fields
                          Temple.TypeMismatch _ expected actual ->
                            unlines
                              [ renderResourceId (resourceId iArticle) ++ " has a type error in " ++ renderPath rest ++ ":"
                              , "  expected " ++ Temple.renderType expected ++ ", got " ++ Temple.renderType actual
                              ]
                          _ ->
                            error $ "type error (TODO): " ++ show err
                    _ ->
                      error $ "type error (TODO): " ++ show err
                Right (_s, ()) -> do
                  let env = Temple.defaultEvalEnv (resourcePath iTemplate) mempty
                  let !value = Temple.evalExpr env $ Temple.locatedVal expr
                  pure (name, value)

            let
              env = Temple.defaultEvalEnv (resourcePath iTemplate) deps
              output =
                Temple.evalTemplate
                  env{Temple.eeScope = Map.fromList bindings' <> Temple.eeScope env}
                  template'

            let
              htmlName =
                let ResourceId resTyName resName = resourceId iArticle
                in resTyName ++ "-" ++ resName
            writeResourceNamed oHtml htmlName output
      )

newtype Rules = Rules [Rule]
  deriving (Semigroup, Monoid)

data Rule = forall a b. Rule !String (Input a) (Output b) (a -> b -> Action ())

matchRule ::
  Rule ->
  -- | Changes
  Set ResourceId ->
  Maybe ([Reason], Action ())
matchRule (Rule name inputs outputs f) changes = do
  let (Any match, reasons, action) = matchInput inputs changes
  guard match
  pure
    ( reasons
    , do
        trace name
        inputs' <- action
        let outputs' = makeOutput outputs
        traverse_ (\input' -> f input' outputs') (fmap snd inputs')
    )

newtype Action a = Action (ReaderT ActionEnv (WriterT ActionSummary (ExceptT DiagnosticReports IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError DiagnosticReports)

data ActionEnv
  = ActionEnv
  { aeTrace :: !(String -> IO ())
  , aeDataDir :: !FilePath
  , aeReasons :: ![Reason]
  }

data ActionSummary
  = ActionSummary
  { asPending :: ![ResourceId]
  , asChanges :: ![Change]
  }

instance Semigroup ActionSummary where
  ActionSummary a b <> ActionSummary a' b' = ActionSummary (a <> a') (b <> b')

instance Monoid ActionSummary where
  mempty = ActionSummary mempty mempty

runAction ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Trace
  (String -> IO ()) ->
  -- | Data directory
  FilePath ->
  -- | Why the action was triggered
  [Reason] ->
  Action a ->
  m ([ResourceId], [Change], a)
runAction fTrace dataDir reasons (Action ma) = do
  let env = ActionEnv{aeTrace = fTrace, aeDataDir = dataDir, aeReasons = reasons}
  (a, ActionSummary pending changes) <- liftEither =<< liftIO (runExceptT . runWriterT . flip runReaderT env $ ma)
  pure (pending, changes, a)

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
  trace $ "putResource: " ++ renderResourceId resId

  dataDir <- askDataDir
  reasons <- Action $ asks aeReasons

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
    then do
      updateResource dataDir resTy resName content
      let changes = [Change Updated resId reasons]
      Action $ tell mempty{asChanges = changes}
    else do
      createResource dataDir resTy resName content
      let changes = [Change Created resId reasons]
      Action $ tell mempty{asChanges = changes}
  Action $ tell mempty{asPending = [resId]}

rule ::
  -- | ID
  String ->
  Input a ->
  Output b ->
  -- | Action
  (a -> b -> Action ()) ->
  Rules
rule name inputs outputs f = Rules [Rule name inputs outputs f]

data Input :: Type -> Type where
  IFmap :: (a -> b) -> Input a -> Input  b
  IPure :: a -> Input  a
  IApply :: Input  (a -> b) -> Input  a -> Input  b
  IResourceType :: String -> Input Resource
  IResource :: ResourceId -> Input Resource

instance Functor Input where
  fmap = IFmap

instance Applicative Input where
  pure = IPure
  (<*>) = IApply

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

{-
new(a * b)
=
new(a) * all(b) + old(a) * new(b)

new(a * b * c)
=
new(a * b) * all(c) + old(a * b) * new(c)
=
(new(a) * all(b) + old(a) * new(b)) * all(c) + old(a * b) * new(c)
=
new(a) * all(b) * all(c) + old(a) * new(b) * all(c) + old(a * b) * new(c)
=
new(a) * all(b) * all(c) + old(a) * new(b) * all(c) + old(a) * old(b) * new(c)
-}

data Age = Old | New
  deriving (Show, Eq)

instance Semigroup Age where
  Old <> a = a
  New <> Old = New
  New <> New = New

instance Monoid Age where
  mempty = Old

matchInput :: Input a -> Set ResourceId -> (Any, [Reason], Action [(Age, a)])
matchInput (IFmap f deps) changes =
  (fmap . fmap . fmap . fmap) f (matchInput deps changes)
matchInput (IPure a) _changes =
  (mempty, [], pure . pure $ pure a)
matchInput (IApply deps deps') changes =
  (liftA2 . liftA2)
    (\l r ->
      let
        (lOld, lNew) = partition ((== New) . fst) l
        rNew = filter ((== New) . fst) r
      in
        liftA2 (<*>) lNew r ++
        liftA2 (<*>) lOld rNew
    )
    (matchInput deps changes)
    (matchInput deps' changes)
matchInput (IResourceType resTyName) changes = do
  changes' <-
    fmap catMaybes .
    for (Set.toList changes) $ \resId@(ResourceId resTyName' _resName) ->
      if resTyName == resTyName'
        then do
          (Any True, [Reason Updated resId], ())
          pure . Just $ (,) New <$> makeResource resId
        else
          pure Nothing
  pure $ do
    (resTyDir, resTy) <- do
      dataDir <- askDataDir
      mResTy <- getResourceType dataDir $ fromString resTyName
      maybe (error $ resTyName ++ " does not exist") pure mResTy
    news <- sequence changes'
    olds <- traverse makeResource . filter (`Set.notMember` changes) =<< listResource resTyDir resTy
    pure $ news ++ fmap ((,) Old) olds
matchInput (IResource resId) changes = do
  changes' <-
    fmap catMaybes .
    for (Set.toList changes) $ \resId' ->
      if resId == resId'
        then do
          (Any True, [Reason Updated resId], ())
          pure . Just $ (,) New <$> makeResource resId
        else
          pure Nothing
  pure $ do
    let ResourceId resTyName resName = resId
    (resTyDir, _resTy) <- do
      dataDir <- askDataDir
      mResTy <- getResourceType dataDir $ fromString resTyName
      maybe (error $ resTyName ++ " does not exist") pure mResTy
    news <- sequence changes'
    olds <- do
      exists <- liftIO $ doesResourceExist resTyDir resName
      if exists && Set.notMember resId changes then pure <$> makeResource resId else pure []
    pure $ news ++ fmap ((,) Old) olds

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
      error $ "resource " ++ renderResourceId resId ++ " does not exist"
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

iResource ::
  -- | Resource type name
  String ->
  -- | Resource name
  String ->
  Input Resource
iResource resTyName = IResource . ResourceId resTyName

iResourceType ::
  -- | Resource type
  String ->
  Input Resource
iResourceType = IResourceType

data Output :: Type -> Type where
  OFmap :: (a -> b) -> Output a -> Output  b
  OPure :: a -> Output a
  OApply :: Output  (a -> b) -> Output  a -> Output  b
  OResourceType :: String -> Output WriteResourceNamed
  OResource :: ResourceId -> Output WriteResource

instance Functor Output where
  fmap = OFmap

instance Applicative Output where
  pure = OPure
  (<*>) = OApply

newtype WriteResourceNamed
  = WriteResourceNamed
  { writeResourceNamed :: String -> LazyByteString -> Action ()
  }

newtype WriteResource
  = WriteResource
  { writeResource :: LazyByteString -> Action ()
  }

oResourceType :: String -> Output WriteResourceNamed
oResourceType = OResourceType

oResource ::
  -- | Resource type name
  String ->
  -- | Resource name
  String ->
  Output WriteResource
oResource resTyName resTy = OResource $ ResourceId resTyName resTy

makeOutput :: Output a -> a
makeOutput (OFmap f a) = f (makeOutput a)
makeOutput (OPure a) = a
makeOutput (OApply a b) = makeOutput a (makeOutput b)
makeOutput (OResourceType resTyName) =
  WriteResourceNamed (\resName -> putResource $ ResourceId resTyName resName)
makeOutput (OResource resId) =
  WriteResource (putResource resId)

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

data Change
  = Change
      -- | What happened
      !Status
      -- | Target resource
      !ResourceId
      -- | Why the change occurred
      ![Reason]
  deriving Show

data Status
  = Created
  | Updated
  deriving Show

data Reason = Reason !Status !ResourceId
  deriving Show

renderChange :: Change -> String
renderChange (Change status resId reasons) =
  renderStatus status ++
  " " ++
  renderResourceId resId ++
  if null reasons
    then " (no reason)"
    else " (" ++ intercalate ", " (fmap renderReason reasons) ++ ")"

renderStatus :: Status -> String
renderStatus status =
  case status of
    Created -> "created"
    Updated -> "updated"
  
renderReason :: Reason -> String
renderReason (Reason status resId) =
  renderResourceId resId ++ " " ++ renderStatus status

data ResourceIdPattern
  = ResourceIdPattern
      -- | Resource type name
      !String
      !ResourceNamePattern
  deriving (Show, Eq)

data ResourceNamePattern
  = PAny
  | PExact !String
  deriving (Show, Eq)

inputResourceIdPatterns :: Input a -> [ResourceIdPattern]
inputResourceIdPatterns (IFmap _f a) = inputResourceIdPatterns a
inputResourceIdPatterns (IPure _a) = []
inputResourceIdPatterns (IApply a b) = inputResourceIdPatterns a ++ inputResourceIdPatterns b
inputResourceIdPatterns (IResourceType resTyName) = [ResourceIdPattern resTyName PAny]
inputResourceIdPatterns (IResource (ResourceId resTyName resName)) = [ResourceIdPattern resTyName $ PExact resName]

outputResourceIdPatterns :: Output a -> [ResourceIdPattern]
outputResourceIdPatterns (OFmap _f a) = outputResourceIdPatterns a
outputResourceIdPatterns (OPure _a) = []
outputResourceIdPatterns (OApply a b) = outputResourceIdPatterns a ++ outputResourceIdPatterns b
outputResourceIdPatterns (OResourceType resTyName) = [ResourceIdPattern resTyName PAny]
outputResourceIdPatterns (OResource (ResourceId resTyName resName)) = [ResourceIdPattern resTyName $ PExact resName]

resourceIdPatternMatches :: ResourceIdPattern -> ResourceIdPattern -> Bool
resourceIdPatternMatches (ResourceIdPattern resTyName resNamePattern) (ResourceIdPattern resTyName' resNamePattern') =
  resTyName == resTyName' &&
  case (resNamePattern, resNamePattern') of
    (PAny, _) -> True
    (_, PAny) -> True
    (PExact name, PExact name') -> name == name'

evalRules ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Trace
  (String -> IO ()) ->
  -- | Data directory
  FilePath ->
  Rules ->
  -- | The created/updated resource
  ResourceId ->
  m [Change]
evalRules fTrace dataDir (Rules rs) resId = do
  execWriterT . flip evalStateT mempty $ do
    dependents <- liftIO $ getDependents dataDir resId
    modify $ (Set.fromList dependents <>) . Set.insert resId
    go
  where
    (graph, fromVertex, _fromKey) =
      graphFromEdges
        [ ( r
          , name
          , [ name'
            | Rule name' inputs' _outputs' _f <- rs
            , let inputPatterns = nub $ inputResourceIdPatterns inputs'
            , or $ resourceIdPatternMatches <$> outputPatterns <*> inputPatterns
            ]
          )
        | r@(Rule name _inputs outputs _f) <- rs
        , let outputPatterns = nub $ outputResourceIdPatterns outputs
        ]

    vertices = topSort graph

    go :: (MonadState (Set ResourceId) m, MonadWriter [Change] m, MonadError DiagnosticReports m, MonadIO m) => m ()
    go = for_ vertices $ \vertex -> do
      let (r, _, _) = fromVertex vertex
      changedResources <- get
      for_ (matchRule r changedResources) $ \(reasons, action) -> do
        (changedResources', changes, ()) <- runAction fTrace dataDir reasons action
        dependents <- liftIO $ concat <$> traverse (getDependents dataDir) changedResources'
        modify $ (Set.fromList changedResources' <>) . (Set.fromList dependents <>)
        tell changes
