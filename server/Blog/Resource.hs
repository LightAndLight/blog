{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Blog.Resource
  ( getResourceType
  , createResource
  , updateResource
  , doesResourceExist
  , lookupResource
  , listResource
  )
where

import Blog
  ( MetadataValue
  , ResourceConfig (..)
  , ResourceId (..)
  , ResourceType (..)
  , renderResourceId
  , resourceConfigDecoder
  )
import Blog.Diagnostic (DiagnosticReports, tomlResult)
import Blog.Metadata (resourceMetadataDecoder)
import Commonmark.Pandoc (Cm, unCm)
import Commonmark.Parser (commonmark)
import Control.Exception (catch, throwIO)
import Control.Monad (unless, when)
import Control.Monad.Error.Class (MonadError)
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
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import Data.Traversable (for)
import IO (WithCallStack (..))
import qualified IO
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removeDirectory
  , removeFile
  )
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.Definition (Block (..))
import Text.Pandoc.Walk (query)
import qualified Toml

getResourceType ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Data directory
  FilePath ->
  -- | Resource type name
  Text ->
  m (Maybe (FilePath, ResourceType))
getResourceType data_ resTy
  | resTy == fromString "resource" = do
      let dir = data_ </> Text.unpack resTy
      let config = ResourceConfig (fromString "text/toml") mempty
      pure $ Just (dir, ResourceType resTy config)
  | otherwise = do
      let dir = data_ </> Text.unpack resTy
      exists <- liftIO $ doesDirectoryExist dir
      if exists
        then do
          content <- liftIO $ IO.readFile $ data_ </> "resource" </> Text.unpack resTy
          config <- parseResourceConfig (Text.unpack resTy) content
          pure $ Just (dir, ResourceType resTy config)
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

doesResourceExist ::
  -- | Resource type directory
  FilePath ->
  -- | Resource name
  String ->
  IO Bool
doesResourceExist resTyDir resName =
  doesFileExist $ resTyDir </> resName

lookupResource ::
  -- | Resource type directory
  FilePath ->
  -- | Resource name
  String ->
  IO (Maybe LazyByteString)
lookupResource resTy resName =
  fmap Just (IO.readFile $ resTy </> resName)
    `catch` \(WithCallStack _cs err) -> if isDoesNotExistError err then pure Nothing else throwIO err

listResource ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Resource type directory
  FilePath ->
  -- | Resource type
  ResourceType ->
  m [ResourceId]
listResource resTyDir resTy = do
  entries <- liftIO $ IO.listDirectory resTyDir
  entries' <- fmap catMaybes . for entries $ \entry -> do
    isFile <- liftIO . doesFileExist $ resTyDir </> entry
    if isFile then pure $ Just entry else pure Nothing
  pure $ fmap (ResourceId . Text.unpack $ resourceTypeName resTy) entries'

createResource ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Data directory
  FilePath ->
  ResourceType ->
  -- | Resource name
  String ->
  -- | Contents
  LazyByteString ->
  m ()
createResource data_ resTy resName body
  | resourceTypeName resTy == fromString "resource" = do
      _config <- parseResourceConfig resName body
      liftIO $ createDirectory (data_ </> resName)
      liftIO $ IO.writeFile (data_ </> Text.unpack (resourceTypeName resTy) </> resName) body
  | otherwise = do
      metadata <- extractMetadata resTy resName body
      updateMetadata data_ resTy resName metadata
      liftIO $ IO.writeFile (data_ </> Text.unpack (resourceTypeName resTy) </> resName) body

updateResource ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Data directory
  FilePath ->
  -- | Resource type
  ResourceType ->
  -- | Resource name
  String ->
  -- | Contents
  LazyByteString ->
  m ()
updateResource data_ resTy resName body
  | resourceTypeName resTy == fromString "resource" = do
      _config <- parseResourceConfig resName body
      liftIO $ IO.writeFile (data_ </> Text.unpack (resourceTypeName resTy) </> resName) body
  | otherwise = do
      metadata <- extractMetadata resTy resName body
      updateMetadata data_ resTy resName metadata
      liftIO $ IO.writeFile (data_ </> Text.unpack (resourceTypeName resTy) </> resName) body

data Metadata
  = Metadata
  { metadataSource :: ByteString
  -- ^ Source
  , metadataValues :: Map Text MetadataValue
  -- ^ Parsed
  }

extractMetadata ::
  MonadError DiagnosticReports m =>
  ResourceType ->
  -- | Resource name
  String ->
  LazyByteString.ByteString ->
  m Metadata
extractMetadata resTy resName body =
  if cfgContentType (resourceTypeConfig resTy) == fromString "text/markdown"
    then extractMetadataMarkdown resTy resName body
    else pure $ Metadata mempty mempty

extractMetadataMarkdown ::
  MonadError DiagnosticReports m =>
  ResourceType ->
  -- | Resource name
  String ->
  LazyByteString.ByteString ->
  m Metadata
extractMetadataMarkdown resTy resName body = do
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
      let decoder = resourceMetadataDecoder resTy
      let
        resourceFile =
          fromString $
            "(" ++ renderResourceId (ResourceId (Text.unpack $ resourceTypeName resTy) resName) ++ "/metadata)"
      let content' = LazyByteString.toStrict content
      toml <- tomlResult resourceFile content $ Toml.parse content'
      values <- tomlResult resourceFile content $ Toml.decode toml decoder
      pure $ Metadata content' values

updateMetadata ::
  MonadIO m =>
  -- | Data directory
  FilePath ->
  ResourceType ->
  -- | Resource name
  String ->
  Metadata ->
  m ()
updateMetadata data_ resTy resName metadata = do
  let
    metadataDir = data_ </> Text.unpack (resourceTypeName resTy) </> (resName ++ ".d")
    metadataFile = metadataDir </> "metadata"
  if Map.null $ metadataValues metadata
    then liftIO $ do
      removeFile metadataFile `catch` \err -> unless (isDoesNotExistError err) $ throwIO err
      mEntries <-
        fmap Just (listDirectory metadataDir)
          `catch` \err -> if isDoesNotExistError err then pure Nothing else throwIO err
      case mEntries of
        Nothing -> pure ()
        Just entries -> when (null entries) $ removeDirectory metadataDir
    else liftIO $ do
      createDirectoryIfMissing False metadataDir
      IO.writeFile metadataFile . LazyByteString.fromStrict $ metadataSource metadata
