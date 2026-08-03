{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- Because `Store(..)` and `ResourceType(..)` are exported under the "Internals" heading.
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Blog.Store
  ( -- * Store
    Store
  , hoistStore

    -- ** Constructors
  , fromDirectory

    -- ** Methods
  , lookupResourceType
  , getResourceType

    -- * Resource types
  , ResourceType
  , hoistResourceType

    -- ** Methods
  , doesResourceExist
  , readResource
  , writeResource
  , listResource
  , readResourceMetadata
  , readResourceModificationTime
  , listDependencies
  , listDependents
  , createDependency
  , removeDependency

    -- * Internals
  , Store (..)
  , ResourceType (..)
  ) where

import Blog
  ( MetadataValue
  , ResourceConfig (..)
  , ResourceId (..)
  , getPropertiesDir
  , readResourceId
  , renderResourceId
  , resourceConfigDecoder
  )
import Blog.Diagnostic (DiagnosticReports (..))
import Blog.Error (tomlResult)
import Blog.Metadata (resourceMetadataDecoder)
import Commonmark.Pandoc (Cm, unCm)
import Commonmark.Parser (commonmark)
import Control.Exception (catch, throwIO)
import Control.Monad (unless, when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid (First (..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import IO (WithCallStack (..))
import qualified IO
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.Definition (Block (..))
import Text.Pandoc.Walk (query)
import qualified Toml

data Store m
  = Store
  { lookupResourceTypeImpl :: !(String -> m (Maybe (ResourceType m)))
  }

hoistStore :: Functor m => (forall a. m a -> n a) -> Store m -> Store n
hoistStore f (Store x1) = Store (fmap f (fmap (fmap (fmap (hoistResourceType f))) x1))

fromDirectory ::
  forall m.
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Store directory
  FilePath ->
  IO (Store m)
fromDirectory storeDir = pure Store{..}
  where
    lookupResourceTypeImpl :: String -> m (Maybe (ResourceType m))
    lookupResourceTypeImpl resTyName
      | resTyName == "resource" = do
          let config = ResourceConfig (fromString "text/toml") mempty
          liftIO $ Just <$> resourceTypeFromDirectory storeDir "resource" config
      | otherwise = do
          let resTyDir = storeDir </> resTyName
          exists <- liftIO $ doesDirectoryExist resTyDir
          if exists
            then do
              content <- liftIO . IO.readFile $ storeDir </> "resource" </> resTyName
              config <- parseResourceConfig resTyName content
              liftIO $ Just <$> resourceTypeFromDirectory storeDir resTyName config
            else pure Nothing

parseResourceConfig ::
  MonadError DiagnosticReports m =>
  -- | Resource type name
  String ->
  LazyByteString ->
  m ResourceConfig
parseResourceConfig resTyName body = do
  let resourceFile = fromString $ "(resource:" ++ resTyName ++ ")"
  toml <- tomlResult resourceFile body . Toml.parse $ LazyByteString.toStrict body
  tomlResult resourceFile body $ Toml.decode toml resourceConfigDecoder

lookupResourceType ::
  Store m ->
  -- | Resource type
  String ->
  m (Maybe (ResourceType m))
lookupResourceType = lookupResourceTypeImpl

getResourceType ::
  MonadError DiagnosticReports m =>
  Store m ->
  -- | Resource type
  String ->
  m (ResourceType m)
getResourceType store resTyName = do
  mResTy <- lookupResourceType store resTyName
  case mResTy of
    Just x -> pure x
    Nothing ->
      throwError . DiagnosticSimple $
        "resource type '" ++ resTyName ++ "' does not exist"

data ResourceType m
  = ResourceType
  { resourceTypeName :: !String
  , resourceTypeConfig :: !ResourceConfig
  , doesResourceExistImpl :: !(String -> m Bool)
  , readResourceImpl :: !(String -> m (Maybe LazyByteString))
  , writeResourceImpl :: !(String -> LazyByteString -> m ())
  , listResourceImpl :: !(m [ResourceId])
  , readResourceMetadataImpl :: !(String -> m (Maybe LazyByteString))
  , readResourceModificationTimeImpl :: !(String -> m (Maybe UTCTime))
  , listDependenciesImpl :: !(String -> m [ResourceId])
  , listDependentsImpl :: !(String -> m [ResourceId])
  , createDependencyImpl :: !(String -> ResourceId -> m ())
  , removeDependencyImpl :: !(String -> ResourceId -> m ())
  }

hoistResourceType :: Functor m => (forall a. m a -> n a) -> ResourceType m -> ResourceType n
hoistResourceType f (ResourceType x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) =
  ResourceType
    x1
    x2
    (fmap f x3)
    (fmap f x4)
    (fmap (fmap f) x5)
    (f x6)
    (fmap f x7)
    (fmap f x8)
    (fmap f x9)
    (fmap f x10)
    (fmap (fmap f) x11)
    (fmap (fmap f) x12)

resourceTypeFromDirectory ::
  forall m.
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Store directory
  FilePath ->
  -- | Resource type name
  String ->
  ResourceConfig ->
  IO (ResourceType m)
resourceTypeFromDirectory storeDir resTyName config =
  pure ResourceType{resourceTypeName = resTyName, resourceTypeConfig = config, ..}
  where
    resTyDir = storeDir </> resTyName

    doesResourceExistImpl :: String -> m Bool
    doesResourceExistImpl resName =
      liftIO . doesFileExist $ resTyDir </> resName

    readResourceImpl :: String -> m (Maybe LazyByteString)
    readResourceImpl resName =
      liftIO $
        fmap Just (IO.readFile $ resTyDir </> resName)
          `catch` \(WithCallStack _cs err) -> if isDoesNotExistError err then pure Nothing else throwIO err

    writeResourceImpl :: String -> LazyByteString -> m ()
    writeResourceImpl resName body
      | resTyName == "resource" = do
          _config <- parseResourceConfig resName body
          liftIO $ createDirectoryIfMissing False (storeDir </> resName)
          liftIO $ IO.writeFile (resTyDir </> resName) body
      | otherwise = do
          metadata <- extractMetadata resTyName config resName body
          updateMetadata resName metadata
          liftIO $ IO.writeFile (resTyDir </> resName) body

    updateMetadata :: String -> Metadata -> m ()
    updateMetadata resName metadata = do
      let
        propertiesDir = getPropertiesDir resTyDir resName
        metadataFile = propertiesDir </> "metadata"

      if Map.null $ metadataValues metadata
        then liftIO $ do
          IO.removeFile metadataFile
            `catch` \(WithCallStack _cs err) -> unless (isDoesNotExistError err) $ throwIO err
          mEntries <-
            fmap Just (IO.listDirectory propertiesDir)
              `catch` \(WithCallStack _cs err) -> if isDoesNotExistError err then pure Nothing else throwIO err
          case mEntries of
            Nothing -> pure ()
            Just entries -> when (null entries) $ IO.removeDirectory propertiesDir
        else liftIO $ do
          createDirectoryIfMissing False propertiesDir
          IO.writeFile metadataFile . LazyByteString.fromStrict $ metadataSource metadata

    readResourceMetadataImpl :: String -> m (Maybe LazyByteString)
    readResourceMetadataImpl resName =
      liftIO $
        fmap Just (IO.readFile $ getPropertiesDir resTyDir resName </> "metadata")
          `catch` \err@(WithCallStack _cs err') -> if isDoesNotExistError err' then pure Nothing else throwIO err

    readResourceModificationTimeImpl :: String -> m (Maybe UTCTime)
    readResourceModificationTimeImpl resName =
      liftIO $
        fmap Just (IO.getModificationTime $ resTyDir </> resName)
          `catch` \err@(WithCallStack _cs err') -> if isDoesNotExistError err' then pure Nothing else throwIO err

    listResourceImpl :: m [ResourceId]
    listResourceImpl = do
      entries <- liftIO $ IO.listDirectory resTyDir
      entries' <- fmap catMaybes . for entries $ \entry -> do
        isFile <- liftIO . doesFileExist $ resTyDir </> entry
        if isFile then pure $ Just entry else pure Nothing
      pure $ fmap (ResourceId resTyName) entries'

    listDependenciesImpl :: String -> m [ResourceId]
    listDependenciesImpl resName = do
      let dependenciesDir = getPropertiesDir resTyDir resName </> "dependencies"
      liftIO $ do
        entries <-
          IO.listDirectory dependenciesDir
            `catch` \(WithCallStack _cs err) -> if isDoesNotExistError err then pure [] else throwIO err
        pure $ fmap readResourceId entries

    listDependentsImpl :: String -> m [ResourceId]
    listDependentsImpl resName = do
      let dependenciesDir = getPropertiesDir resTyDir resName </> "dependents"
      liftIO $ do
        entries <-
          IO.listDirectory dependenciesDir
            `catch` \(WithCallStack _cs err) -> if isDoesNotExistError err then pure [] else throwIO err
        pure $ fmap readResourceId entries

    createDependencyImpl :: String -> ResourceId -> m ()
    createDependencyImpl resNameSrc dependencyTarget@(ResourceId resTyNameTgt resNameTgt) = do
      let dependencySource = ResourceId resTyName resNameSrc

      let dependenciesDir = getPropertiesDir resTyDir resNameSrc </> "dependencies"
      liftIO $ do
        createDirectoryIfMissing True dependenciesDir
        IO.writeFile (dependenciesDir </> renderResourceId dependencyTarget) mempty

      let resTyDirTgt = storeDir </> resTyNameTgt
      let dependentsDir = getPropertiesDir resTyDirTgt resNameTgt </> "dependents"
      liftIO $ do
        createDirectoryIfMissing True dependentsDir
        IO.writeFile (dependentsDir </> renderResourceId dependencySource) mempty

    removeDependencyImpl :: String -> ResourceId -> m ()
    removeDependencyImpl resNameSrc dependencyTarget@(ResourceId resTyDirTgt resNameTgt) = do
      let dependencySource = ResourceId resTyName resNameSrc
      liftIO $ do
        let dependenciesDir = getPropertiesDir resTyDir resNameSrc </> "dependencies"
        IO.removeFile (dependenciesDir </> renderResourceId dependencySource)
          `catch` \(WithCallStack _cs err) -> unless (isDoesNotExistError err) $ throwIO err

      liftIO $ do
        let dependentsDir = getPropertiesDir resTyDirTgt resNameTgt </> "dependents"
        IO.removeFile (dependentsDir </> renderResourceId dependencyTarget)
          `catch` \(WithCallStack _cs err) -> unless (isDoesNotExistError err) $ throwIO err

doesResourceExist :: ResourceType m -> String -> m Bool
doesResourceExist = doesResourceExistImpl

readResource :: ResourceType m -> String -> m (Maybe LazyByteString)
readResource = readResourceImpl

writeResource :: ResourceType m -> String -> LazyByteString -> m ()
writeResource = writeResourceImpl

listResource :: ResourceType m -> m [ResourceId]
listResource = listResourceImpl

readResourceMetadata :: ResourceType m -> String -> m (Maybe LazyByteString)
readResourceMetadata = readResourceMetadataImpl

readResourceModificationTime :: ResourceType m -> String -> m (Maybe UTCTime)
readResourceModificationTime = readResourceModificationTimeImpl

listDependencies :: ResourceType m -> String -> m [ResourceId]
listDependencies = listDependenciesImpl

listDependents :: ResourceType m -> String -> m [ResourceId]
listDependents = listDependentsImpl

createDependency ::
  ResourceType m ->
  -- | Source name
  String ->
  -- | Target ID
  ResourceId ->
  m ()
createDependency = createDependencyImpl

removeDependency :: ResourceType m -> String -> ResourceId -> m ()
removeDependency = removeDependencyImpl

data Metadata
  = Metadata
  { metadataSource :: ByteString
  -- ^ Source
  , metadataValues :: Map Text MetadataValue
  -- ^ Parsed
  }

extractMetadata ::
  MonadError DiagnosticReports m =>
  -- | Resource type
  String ->
  ResourceConfig ->
  -- | Resource name
  String ->
  LazyByteString.ByteString ->
  m Metadata
extractMetadata resTyName config resName body =
  if cfgContentType config == fromString "text/markdown"
    then extractMetadataMarkdown resTyName config resName body
    else pure $ Metadata mempty mempty

extractMetadataMarkdown ::
  MonadError DiagnosticReports m =>
  -- | Resourced type
  String ->
  ResourceConfig ->
  -- | Resource name
  String ->
  LazyByteString.ByteString ->
  m Metadata
extractMetadataMarkdown resTyName config resName body = do
  body' <-
    case Text.Lazy.Encoding.decodeUtf8' body of
      Left err -> error "TODO: " err
      Right x -> pure $! LazyText.toStrict x
  markdown <-
    case commonmark "(input)" body' of
      Left err -> error "TODO: " err
      Right x -> pure $ unCm (x :: Cm () Blocks)

  let
    metadataBlock (CodeBlock (_ident, classes, _kvs) content)
      | fromString "toml+blog-metadata" `elem` classes =
          Just . Text.Lazy.Encoding.encodeUtf8 $ LazyText.fromStrict content
      | otherwise = Nothing
    metadataBlock _ = Nothing

  case getFirst $ query @Block (First . metadataBlock) markdown of
    Nothing -> pure $ Metadata mempty mempty
    Just content -> do
      let decoder = resourceMetadataDecoder config
      let
        resourceFile =
          fromString $
            "(" ++ renderResourceId (ResourceId resTyName resName) ++ ":metadata)"
      let content' = LazyByteString.toStrict content
      toml <- tomlResult resourceFile content $ Toml.parse content'
      values <- tomlResult resourceFile content $ Toml.decode toml decoder
      pure $ Metadata content' values
