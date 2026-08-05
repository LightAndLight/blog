{-# LANGUAGE BangPatterns #-}
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
  , TransactionId
  , renderTransactionId
  , parseTransactionId
  , withTransaction
  , beginTransaction
  , commitTransaction
  , rollbackTransaction

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
  , propertiesPart
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
import Control.Monad.Catch (ExitCase (..), MonadMask, generalBracket)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.List ((\\))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (First (..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import GHC.Stack (HasCallStack)
import ID (ID)
import qualified ID
import IO (WithCallStack (..))
import qualified IO
import System.Directory
  ( copyFile
  , createDirectory
  , doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  )
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.Definition (Block (..))
import Text.Pandoc.Walk (query)
import qualified Toml

data Store m
  = Store
  { lookupResourceTypeImpl :: !(TransactionId -> String -> m (Maybe (ResourceType m)))
  , beginTransactionImpl :: m TransactionId
  , commitTransactionImpl :: TransactionId -> m ()
  , rollbackTransactionImpl :: TransactionId -> m ()
  }

hoistStore :: Functor m => (forall a. m a -> n a) -> Store m -> Store n
hoistStore f (Store x1 x2 x3 x4) =
  Store
    (fmap (fmap (f . fmap (fmap (hoistResourceType f)))) x1)
    (f x2)
    (fmap f x3)
    (fmap f x4)

newtype TransactionId = TransactionId ID

renderTransactionId :: TransactionId -> String
renderTransactionId (TransactionId xactId) = ID.toString xactId

parseTransactionId :: String -> Maybe TransactionId
parseTransactionId = fmap TransactionId . ID.fromString

getSystemDir ::
  -- | Store directory
  FilePath ->
  FilePath
getSystemDir storeDir = storeDir </> ":system"

getTransactionDir ::
  -- | Store directory
  FilePath ->
  FilePath
getTransactionDir storeDir = getSystemDir storeDir </> "transaction"

getTransactionIdDir ::
  -- | Store directory
  FilePath ->
  TransactionId ->
  FilePath
getTransactionIdDir storeDir (TransactionId xactId) =
  getTransactionDir storeDir </> ID.toString xactId

data Change = Create | Update | Delete

changePart :: Change -> String
changePart Create = ":create"
changePart Update = ":update"
changePart Delete = ":delete"

fromDirectory ::
  forall m.
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Store directory
  FilePath ->
  IO (Store m)
fromDirectory storeDir = do
  IO.createDirectoryIfMissing True $ getSystemDir storeDir
  IO.createDirectoryIfMissing True $ getTransactionDir storeDir
  pure Store{..}
  where
    lookupResourceTypeImpl :: TransactionId -> String -> m (Maybe (ResourceType m))
    lookupResourceTypeImpl xactId resTyName
      | resTyName == "resource" = do
          let config = ResourceConfig (fromString "text/toml") mempty
          liftIO $ Just <$> resourceTypeFromDirectory storeDir xactId "resource" config
      | otherwise = do
          let resTyDir = storeDir </> resTyName
          exists <- liftIO $ doesDirectoryExist resTyDir
          if exists
            then do
              content <-
                liftIO $
                  IO.readFile (getTransactionIdDir storeDir xactId </> "resource" </> resTyName)
                    `catch` \err@(WithCallStack _cs err') ->
                      if isDoesNotExistError err'
                        then IO.readFile (storeDir </> "resource" </> resTyName)
                        else throwIO err
              config <- parseResourceConfig resTyName content
              liftIO $ Just <$> resourceTypeFromDirectory storeDir xactId resTyName config
            else pure Nothing

    beginTransactionImpl :: m TransactionId
    beginTransactionImpl = do
      xactId <- liftIO $ TransactionId <$> ID.generate
      liftIO $ do
        let xactDir = getTransactionIdDir storeDir xactId
        createDirectory xactDir
        traverse_ (\change -> createDirectory $ xactDir </> changePart change) [Create, Update, Delete]
      pure xactId

    commitTransactionImpl :: TransactionId -> m ()
    commitTransactionImpl xactId = do
      -- TODO: make this atomic
      --
      -- \* Clients can see the transaction in progress, e.g. the transaction
      --   creates 2 files but the first is visible and the second is not.
      -- \* If the server crashes mid-commit, then the store is in an invalid state.
      liftIO $ do
        let xactDir = getTransactionIdDir storeDir xactId
        mergeDirectory (xactDir </> changePart Create) storeDir
        mergeDirectory (xactDir </> changePart Update) storeDir
        subtractDirectory (xactDir </> changePart Delete) storeDir
        removeDirectoryRecursive xactDir

    -- \| Recursively copy the contents of the source directory into the target directory.
    --
    -- Afterward, the target directory's tree is a superset of the source directory.
    mergeDirectory ::
      HasCallStack =>
      -- \| Source directory
      FilePath ->
      -- \| Target directory
      FilePath ->
      IO ()
    mergeDirectory srcDir tgtDir = do
      entries <- IO.listDirectory srcDir
      for_ entries $ \entry -> do
        let srcPath = srcDir </> entry
        let tgtPath = tgtDir </> entry
        isDir <- doesDirectoryExist srcPath
        if isDir
          then do
            IO.createDirectoryIfMissing False tgtPath
            mergeDirectory srcPath tgtPath
          else copyFile srcPath tgtPath

    -- \| Recursively remove the contents of the source directory from the target directory.
    --
    -- An empty directory in the source directory causes removal of the entire
    -- corresponding directory in the target directory.
    subtractDirectory ::
      -- \| Source directory
      FilePath ->
      -- \| Target directory
      FilePath ->
      IO ()
    subtractDirectory src tgt = void $ go src tgt
      where
        go srcDir tgtDir = do
          entries <- IO.listDirectory srcDir
          let !isEmpty = null entries
          for_ entries $ \entry -> do
            let srcPath = srcDir </> entry
            let tgtPath = tgtDir </> entry
            isDir <- doesDirectoryExist srcPath
            if isDir
              then do
                wasEmpty <- go srcPath tgtPath
                when wasEmpty $ IO.removeDirectory tgtPath
              else IO.removeFile tgtPath
          pure isEmpty

    rollbackTransactionImpl :: TransactionId -> m ()
    rollbackTransactionImpl (TransactionId xactId) = do
      liftIO $
        removeDirectoryRecursive (getTransactionDir storeDir </> ID.toString xactId)
          `catch` \err@(WithCallStack _cs err') -> unless (isDoesNotExistError err') $ throwIO err

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
  TransactionId ->
  -- | Resource type
  String ->
  m (Maybe (ResourceType m))
lookupResourceType = lookupResourceTypeImpl

getResourceType ::
  MonadError DiagnosticReports m =>
  Store m ->
  TransactionId ->
  -- | Resource type
  String ->
  m (ResourceType m)
getResourceType store xactId resTyName = do
  mResTy <- lookupResourceType store xactId resTyName
  case mResTy of
    Just x -> pure x
    Nothing ->
      throwError . DiagnosticSimple $
        "resource type '" ++ resTyName ++ "' does not exist"

withTransaction :: MonadMask m => Store m -> (TransactionId -> m a) -> m a
withTransaction store f = do
  (a, ()) <- generalBracket (beginTransaction store) exit f
  pure a
  where
    exit xactId (ExitCaseSuccess _a) = commitTransaction store xactId
    exit xactId (ExitCaseException _err) = rollbackTransaction store xactId
    exit xactId ExitCaseAbort = rollbackTransaction store xactId

beginTransaction :: Store m -> m TransactionId
beginTransaction = beginTransactionImpl

commitTransaction :: Store m -> TransactionId -> m ()
commitTransaction = commitTransactionImpl

rollbackTransaction :: Store m -> TransactionId -> m ()
rollbackTransaction = rollbackTransactionImpl

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
  TransactionId ->
  -- | Resource type name
  String ->
  ResourceConfig ->
  IO (ResourceType m)
resourceTypeFromDirectory storeDir xactId resTyName config =
  pure ResourceType{resourceTypeName = resTyName, resourceTypeConfig = config, ..}
  where
    baseResTyDir = storeDir </> resTyName
    xactResTyDir change = getTransactionIdDir storeDir xactId </> changePart change </> resTyName

    orIO :: [IO Bool] -> IO Bool
    orIO [] = pure False
    orIO (mb : mbs) = do
      b <- mb
      if b then pure True else orIO mbs

    orElseIO :: [IO (Maybe a)] -> IO (Maybe a)
    orElseIO [] = pure Nothing
    orElseIO (mma : mmas) = do
      ma <- mma
      case ma of
        Just{} -> pure ma
        Nothing -> orElseIO mmas

    doesResourceExistImpl :: String -> m Bool
    doesResourceExistImpl resName =
      liftIO $ do
        removed <- doesFileExist $ xactResTyDir Delete </> resName
        if removed
          then pure False
          else
            orIO
              [ doesFileExist $ xactResTyDir Create </> resName
              , doesFileExist $ xactResTyDir Update </> resName
              , doesFileExist $ baseResTyDir </> resName
              ]

    readResourceImpl :: String -> m (Maybe LazyByteString)
    readResourceImpl resName =
      liftIO $ do
        removed <- doesFileExist $ xactResTyDir Delete </> resName
        if removed
          then pure Nothing
          else
            orElseIO
              [ doRead $ xactResTyDir Create </> resName
              , doRead $ xactResTyDir Update </> resName
              , doRead $ baseResTyDir </> resName
              ]
      where
        doRead path =
          fmap Just (IO.readFile path)
            `catch` \(WithCallStack _cs err) ->
              if isDoesNotExistError err
                then pure Nothing
                else throwIO err

    xactWriteFile ::
      HasCallStack =>
      -- \| File directory, relative to store directory
      FilePath ->
      -- \| File name
      String ->
      LazyByteString ->
      IO ()
    xactWriteFile dir name body = do
      let xactDir = getTransactionIdDir storeDir xactId
      let path = dir </> name

      removed <- doesFileExist $ xactDir </> changePart Delete </> path
      dir' <-
        if removed
          then do
            IO.removeFile $ xactDir </> changePart Delete </> path
            pure $ xactDir </> changePart Update </> dir
          else do
            updated <- doesFileExist $ storeDir </> path
            if updated
              then pure $ xactDir </> changePart Update </> dir
              else pure $ xactDir </> changePart Create </> dir

      IO.createDirectoryIfMissing True dir'
      IO.writeFile (dir' </> name) body

    writeResourceImpl :: String -> LazyByteString -> m ()
    writeResourceImpl resName body = do
      removed <- liftIO . doesFileExist $ xactResTyDir Delete </> resName
      if removed
        then do
          liftIO . IO.removeFile $ xactResTyDir Delete </> resName
          doWrite Update
        else do
          updated <- liftIO . doesFileExist $ baseResTyDir </> resName
          if updated
            then
              doWrite Update
            else
              doWrite Create
      where
        doWrite change
          | resTyName == "resource" = do
              _config <- parseResourceConfig resName body
              liftIO $ do
                IO.createDirectoryIfMissing False (getTransactionIdDir storeDir xactId </> resName)
                xactWriteFile (xactResTyDir change) resName body
          | otherwise = do
              liftIO $ IO.createDirectoryIfMissing False (xactResTyDir change)
              metadata <- extractMetadata resTyName config resName body
              updateMetadata change resName metadata
              liftIO $ xactWriteFile (xactResTyDir change) resName body

    updateMetadata :: Change -> String -> Metadata -> m ()
    updateMetadata change resName metadata = do
      let propertiesDir = xactResTyDir change </> propertiesPart resName
      liftIO $ do
        xactWriteFile propertiesDir "metadata" (LazyByteString.fromStrict $ metadataSource metadata)

    readResourceMetadataImpl :: String -> m (Maybe LazyByteString)
    readResourceMetadataImpl resName =
      liftIO $ do
        removed <- doesFileExist $ xactResTyDir Delete </> resName
        if removed
          then pure Nothing
          else
            orElseIO
              [ doRead $ xactResTyDir Create </> propertiesPart resName </> "metadata"
              , doRead $ xactResTyDir Update </> propertiesPart resName </> "metadata"
              , doRead $ baseResTyDir </> propertiesPart resName </> "metadata"
              ]
      where
        doRead path =
          fmap Just (IO.readFile path)
            `catch` \err@(WithCallStack _cs err') -> if isDoesNotExistError err' then pure Nothing else throwIO err

    readResourceModificationTimeImpl :: String -> m (Maybe UTCTime)
    readResourceModificationTimeImpl resName =
      liftIO $ do
        removed <- doesFileExist $ xactResTyDir Delete </> resName
        if removed
          then pure Nothing
          else
            orElseIO
              [ doTime $ xactResTyDir Create </> resName
              , doTime $ xactResTyDir Update </> resName
              , doTime $ baseResTyDir </> resName
              ]
      where
        doTime path =
          fmap Just (IO.getModificationTime path)
            `catch` \err@(WithCallStack _cs err') -> if isDoesNotExistError err' then pure Nothing else throwIO err

    listResourceImpl :: m [ResourceId]
    listResourceImpl =
      liftIO $ do
        created <- doList $ xactResTyDir Create
        removed <- doList $ xactResTyDir Delete
        current <- doList baseResTyDir
        pure $
          fmap (ResourceId resTyName) created
            ++ fmap (ResourceId resTyName) (current \\ removed)
      where
        doList path = do
          entries <-
            IO.listDirectory path
              `catch` \err@(WithCallStack _cs err') -> if isDoesNotExistError err' then pure [] else throwIO err
          fmap catMaybes . for entries $ \entry -> do
            isFile <- doesFileExist $ path </> entry
            if isFile then pure $ Just entry else pure Nothing

    listDependenciesImpl :: String -> m [ResourceId]
    listDependenciesImpl resName = do
      entries <- liftIO $ do
        removed <- doesFileExist $ xactResTyDir Delete </> resName
        if removed
          then pure []
          else
            fromMaybe []
              <$> orElseIO
                [ doList $ xactResTyDir Create </> propertiesPart resName </> "dependencies"
                , doList $ xactResTyDir Update </> propertiesPart resName </> "dependencies"
                , doList $ baseResTyDir </> propertiesPart resName </> "dependencies"
                ]
      pure $ fmap readResourceId entries
      where
        doList path =
          fmap Just (IO.listDirectory path)
            `catch` \(WithCallStack _cs err) -> if isDoesNotExistError err then pure Nothing else throwIO err

    listDependentsImpl :: String -> m [ResourceId]
    listDependentsImpl resName = do
      entries <- liftIO $ do
        removed <- doesFileExist $ xactResTyDir Delete </> resName
        if removed
          then pure []
          else
            fromMaybe []
              <$> orElseIO
                [ doList $ xactResTyDir Create </> propertiesPart resName </> "dependents"
                , doList $ xactResTyDir Update </> propertiesPart resName </> "dependents"
                , doList $ baseResTyDir </> propertiesPart resName </> "dependents"
                ]
      pure $ fmap readResourceId entries
      where
        doList path =
          fmap Just (IO.listDirectory path)
            `catch` \(WithCallStack _cs err) -> if isDoesNotExistError err then pure Nothing else throwIO err

    xactRemoveFile ::
      HasCallStack =>
      -- \| File path, relative to store directory
      FilePath ->
      IO ()
    xactRemoveFile path = do
      missing <-
        orIO
          [ doesFileExist $ getTransactionIdDir storeDir xactId </> changePart Delete </> path
          , fmap not . doesFileExist $ storeDir </> path
          ]
      if missing
        then pure ()
        else do
          created <- doesFileExist $ getTransactionIdDir storeDir xactId </> changePart Create </> path
          if created
            then doRemove (getTransactionIdDir storeDir xactId) (changePart Create </> path)
            else do
              updated <- doesFileExist $ getTransactionIdDir storeDir xactId </> changePart Update </> path
              when updated $ doRemove (getTransactionIdDir storeDir xactId) (changePart Update </> path)
              IO.writeFile (getTransactionIdDir storeDir xactId </> changePart Delete </> path) mempty
      where
        doRemove base path' = do
          isDir <- doesDirectoryExist $ base </> path'
          if isDir
            then IO.removeDirectory $ base </> path'
            else IO.removeFile $ base </> path'
          let path'' = takeDirectory path'
          unless (null path'') $ do
            entries <- IO.listDirectory $ base </> path''
            when (null entries) $ doRemove base path''

    createDependencyImpl :: String -> ResourceId -> m ()
    createDependencyImpl resNameSrc dependencyTarget@(ResourceId resTyNameTgt resNameTgt) = do
      let dependencySource = ResourceId resTyName resNameSrc

      srcRemoved <-
        liftIO $
          orIO
            [ doesFileExist $ xactResTyDir Delete </> resNameSrc
            , fmap not . doesFileExist $ baseResTyDir </> resNameSrc
            ]
      if srcRemoved
        then
          throwError . DiagnosticSimple $
            "dependency source " ++ renderResourceId dependencySource ++ " does not exist"
        else
          liftIO $
            xactWriteFile
              (resTyName </> propertiesPart resNameSrc </> "dependencies")
              (renderResourceId dependencyTarget)
              mempty

      tgtRemoved <-
        liftIO $
          orIO
            [ doesFileExist $ xactResTyDir Delete </> resNameSrc
            , fmap not . doesFileExist $ baseResTyDir </> resNameSrc
            ]
      if tgtRemoved
        then
          throwError . DiagnosticSimple $
            "dependency target " ++ renderResourceId dependencyTarget ++ " does not exist"
        else
          liftIO $
            xactWriteFile
              (resTyNameTgt </> propertiesPart resNameTgt </> "dependents")
              (renderResourceId dependencySource)
              mempty

    removeDependencyImpl :: String -> ResourceId -> m ()
    removeDependencyImpl resNameSrc dependencyTarget@(ResourceId resTyNameTgt resNameTgt) = do
      let dependencySource = ResourceId resTyName resNameSrc

      liftIO $ do
        let dependenciesDir = resTyName </> propertiesPart resNameSrc </> "dependencies"
        xactRemoveFile $ dependenciesDir </> renderResourceId dependencySource

      liftIO $ do
        let dependentsDir = resTyNameTgt </> propertiesPart resNameTgt </> "dependents"
        xactRemoveFile $ dependentsDir </> renderResourceId dependencyTarget

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
