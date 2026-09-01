{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
  , Transaction (..)
  , Change (..)
  , TransactionChange (..)
  , renderTransactionId
  , parseTransactionId
  , withTransaction
  , bracketTransaction
  , beginTransaction
  , commitTransaction
  , rollbackTransaction
  , listTransactions
  , lookupTransaction
  , saveDeferred
  , restoreDeferred
  , export
  , import_

    -- * Resource types
  , ResourceType
  , hoistResourceType

    -- ** Methods
  , doesResourceExist
  , readResource
  , writeResource
  , lookupProperty
  , setProperty
  , readProperty
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
  ( MetadataValue (..)
  , ResourceConfig (..)
  , ResourceId (..)
  , propertiesPart
  , readResourceId
  , renderResourceId
  , resourceConfigDecoder
  )
import Blog.Diagnostic (DiagnosticReports (..))
import Blog.Error (sageErrorReport, tomlResult)
import Blog.ID (ID)
import qualified Blog.ID as ID
import Blog.Metadata (metadataValueFromToml, renderMetadataValueToml, resourceMetadataDecoder)
import Blog.Pandoc (markdownReaderOptions)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar (entryTarPath, fileEntry)
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Control.Monad.Catch (ExitCase (..), MonadCatch, MonadMask, catch, generalBracket, throwM)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Fix (mfix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.Hash.SHA256 as Sha256
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.List (union, (\\))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (First (..))
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import GHC.Stack (HasCallStack)
import IO (WithCallStack (..))
import qualified IO
import System.Directory
  ( createDirectory
  , doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  , renameDirectory
  )
import System.FilePath (splitDirectories, takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Definition (Block (..))
import Text.Pandoc.Walk (query)
import qualified Text.Sage as Sage
import qualified Toml

data Store m
  = Store
  { lookupResourceTypeImpl :: !(TransactionId -> String -> m (Maybe (ResourceType m)))
  , beginTransactionImpl :: Bool -> m TransactionId
  , commitTransactionImpl :: TransactionId -> m ()
  , rollbackTransactionImpl :: TransactionId -> m ()
  , listTransactionsImpl :: m [TransactionId]
  , lookupTransactionImpl :: TransactionId -> m (Maybe Transaction)
  , saveDeferredImpl :: TransactionId -> m ()
  , restoreDeferredImpl :: TransactionId -> m ()
  }

hoistStore :: Functor m => (forall a. m a -> n a) -> Store m -> Store n
hoistStore f (Store x1 x2 x3 x4 x5 x6 x7 x8) =
  Store
    (fmap (fmap (f . fmap (fmap (hoistResourceType f)))) x1)
    (fmap f x2)
    (fmap f x3)
    (fmap f x4)
    (f x5)
    (fmap f x6)
    (fmap f x7)
    (fmap f x8)

newtype TransactionId = TransactionId ID
  deriving (Eq, Ord)

renderTransactionId :: TransactionId -> String
renderTransactionId (TransactionId xactId) = ID.toString xactId

parseTransactionId :: String -> Maybe TransactionId
parseTransactionId = fmap TransactionId . ID.fromString

data Transaction
  = Transaction
  { xactDefer :: Bool
  -- ^ Defer rules until commit
  , xactChanges :: [TransactionChange]
  }

data TransactionChange
  = TransactionChange
  { xactChange :: Change
  , xactChangeId :: ResourceId
  }

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

deferPart :: String
deferPart = ":defer"

fromDirectory ::
  forall m.
  (MonadError DiagnosticReports m, MonadCatch m, MonadIO m) =>
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
                  IO.readFile (getTransactionIdDir storeDir xactId </> changePart Update </> "resource" </> resTyName)
                    `catch` \err@(WithCallStack _cs err') ->
                      if isDoesNotExistError err'
                        then IO.readFile (storeDir </> "resource" </> resTyName)
                        else throwIO err
              config <- parseResourceConfig resTyName content
              liftIO $ Just <$> resourceTypeFromDirectory storeDir xactId resTyName config
            else do
              let xactDir = getTransactionIdDir storeDir xactId </> changePart Create
              let xactResTyDir = xactDir </> resTyName
              inCreated <- liftIO $ doesDirectoryExist xactResTyDir
              if inCreated
                then do
                  content <- liftIO $ IO.readFile (xactDir </> "resource" </> resTyName)
                  config <- parseResourceConfig resTyName content
                  liftIO $ Just <$> resourceTypeFromDirectory storeDir xactId resTyName config
                else pure Nothing

    beginTransactionImpl :: Bool -> m TransactionId
    beginTransactionImpl defer = do
      xactId <- liftIO $ TransactionId <$> ID.generate
      liftIO $ do
        let xactDir = getTransactionIdDir storeDir xactId
        createDirectory xactDir
        traverse_ (\change -> createDirectory $ xactDir </> changePart change) [Create, Update, Delete]

        when defer $ do
          let deferDir = xactDir </> deferPart
          createDirectory deferDir

      pure xactId

    commitTransactionImpl :: TransactionId -> m ()
    commitTransactionImpl xactId = do
      -- TODO: make this atomic
      --
      -- \* Clients can see the transaction in progress, e.g. the transaction
      --   creates 2 files but the first is visible and the second is not.
      -- \* If the server crashes mid-commit, then the store is in an invalid state.
      let xactDir = getTransactionIdDir storeDir xactId
      exists <- liftIO $ doesDirectoryExist xactDir
      if exists
        then liftIO $ do
          mergeDirectoryCreate (xactDir </> changePart Create) storeDir
          mergeDirectoryUpdate (xactDir </> changePart Update) storeDir
          subtractDirectory (xactDir </> changePart Delete) storeDir
          removeDirectoryRecursive xactDir
        else
          throwError . DiagnosticSimple $ "transaction not found: " ++ renderTransactionId xactId

    -- \| Recursively copy the contents of the source directory into the target directory.
    --
    -- Afterward, the target directory's tree is a superset of the source directory.
    --
    -- An empty directory within the source directory results in a corresponding empty
    -- directory in the target directory. So if that empty directory already exists in
    -- the target directory, then the contents will be deleted.
    mergeDirectoryCreate ::
      HasCallStack =>
      -- \| Source directory
      FilePath ->
      -- \| Target directory
      FilePath ->
      IO ()
    mergeDirectoryCreate srcDir' tgtDir' = do
      entries <- IO.listDirectory srcDir'
      go entries srcDir' tgtDir'
      where
        go entries srcDir tgtDir =
          for_ entries $ \entry -> do
            let srcPath = srcDir </> entry
            let tgtPath = tgtDir </> entry
            isDir <- doesDirectoryExist srcPath
            if isDir
              then do
                entries' <- IO.listDirectory srcPath
                if null entries'
                  then do
                    IO.removeDirectory tgtPath `catch` \err'@(WithCallStack _cs err) -> unless (isDoesNotExistError err) $ throwIO err'
                    IO.createDirectoryIfMissing False tgtPath
                  else do
                    IO.createDirectoryIfMissing False tgtPath
                    go entries' srcPath tgtPath
              else do
                putStrLn $ "create: copy " ++ srcPath ++ " to " ++ tgtPath
                IO.copyFile srcPath tgtPath

    -- \| Recursively copy the contents of the source directory into the target directory.
    --
    -- Afterward, the target directory's tree is a superset of the source directory.
    mergeDirectoryUpdate ::
      HasCallStack =>
      -- \| Source directory
      FilePath ->
      -- \| Target directory
      FilePath ->
      IO ()
    mergeDirectoryUpdate srcDir tgtDir = do
      entries <- IO.listDirectory srcDir
      for_ entries $ \entry -> do
        let srcPath = srcDir </> entry
        let tgtPath = tgtDir </> entry
        isDir <- doesDirectoryExist srcPath
        if isDir
          then do
            IO.createDirectoryIfMissing False tgtPath
            mergeDirectoryUpdate srcPath tgtPath
          else do
            putStrLn $ "update: copy " ++ srcPath ++ " to " ++ tgtPath
            IO.copyFile srcPath tgtPath

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

    listTransactionsImpl :: m [TransactionId]
    listTransactionsImpl = do
      liftIO $ do
        entries <-
          IO.listDirectory (getTransactionDir storeDir)
            `catch` \err@(WithCallStack _cs err') -> if isDoesNotExistError err' then pure [] else throwIO err
        pure $
          fmap
            (\entry -> fromMaybe (error $ "invalid transaction ID: " ++ show entry) $ parseTransactionId entry)
            entries

    lookupTransactionImpl :: TransactionId -> m (Maybe Transaction)
    lookupTransactionImpl xactId =
      liftIO $ do
        let xactDir = getTransactionIdDir storeDir xactId
        exists <- doesDirectoryExist xactDir
        if exists
          then do
            defer <- doesDirectoryExist $ xactDir </> deferPart
            let
              getResourceIds change = do
                resTyNames <- IO.listDirectory $ xactDir </> changePart change
                fmap (foldMap Set.toList) . for resTyNames $ \resTyName -> do
                  let resTyDir = xactDir </> changePart change </> resTyName
                  entries <- IO.listDirectory resTyDir
                  fmap Set.fromList . for entries $ \entry -> do
                    isDir <- doesDirectoryExist entry
                    if isDir
                      then do
                        -- `name:properties` directory
                        let (prefix, suffix) = break (== ':') entry
                        when (suffix /= ":properties") . error $
                          "unexpected resource directory: " ++ show (resTyDir </> entry)
                        pure $ ResourceId resTyName prefix
                      else pure $ ResourceId resTyName entry

            creates <- getResourceIds Create
            updates <- getResourceIds Update
            deletes <- getResourceIds Delete
            pure $
              Just
                Transaction
                  { xactDefer = defer
                  , xactChanges =
                      fmap (TransactionChange Create) creates
                        ++ fmap (TransactionChange Update) updates
                        ++ fmap (TransactionChange Delete) deletes
                  }
          else pure Nothing

    copyDirectory :: FilePath -> FilePath -> IO ()
    copyDirectory src tgt = do
      createDirectory tgt
      entries <- IO.listDirectory src
      for_ entries $ \entry -> do
        let srcPath = src </> entry
        let dstPath = tgt </> entry
        isDir <- doesDirectoryExist srcPath
        if isDir
          then copyDirectory srcPath dstPath
          else IO.copyFile srcPath dstPath

    saveDeferredImpl :: TransactionId -> m ()
    saveDeferredImpl xactId = do
      let xactDir = getTransactionIdDir storeDir xactId
      liftIO . for_ [Create, Update, Delete] $ \change -> do
        removeDirectoryRecursive (xactDir </> deferPart </> changePart change)
          `catch` \err -> unless (isDoesNotExistError err) $ throwIO err
        copyDirectory (xactDir </> changePart change) (xactDir </> deferPart </> changePart change)

    restoreDeferredImpl :: TransactionId -> m ()
    restoreDeferredImpl xactId = do
      let xactDir = getTransactionIdDir storeDir xactId
      liftIO . for_ [Create, Update, Delete] $ \change -> do
        removeDirectoryRecursive (xactDir </> changePart change)
        renameDirectory (xactDir </> deferPart </> changePart change) (xactDir </> changePart change)

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

bracketTransaction ::
  MonadMask m =>
  -- | On begin
  (TransactionId -> m ()) ->
  -- | On commit
  (TransactionId -> m ()) ->
  -- | On rollback
  (TransactionId -> m ()) ->
  Store m ->
  -- | Defer rules until commit
  Bool ->
  (TransactionId -> m a) ->
  m a
bracketTransaction onBegin onCommit onRollback store defer f = do
  (a, ()) <-
    generalBracket
      ( do
          xactId <- beginTransaction store defer
          xactId <$ onBegin xactId
      )
      exit
      f
  pure a
  where
    exit xactId (ExitCaseSuccess _a) = do
      commitTransaction store xactId
      onCommit xactId
    exit xactId (ExitCaseException _err) = do
      rollbackTransaction store xactId
      onRollback xactId
    exit xactId ExitCaseAbort = do
      rollbackTransaction store xactId
      onRollback xactId

withTransaction ::
  MonadMask m =>
  Store m ->
  -- | Defer rules until commit
  Bool ->
  (TransactionId -> m a) ->
  m a
withTransaction = bracketTransaction (const $ pure ()) (const $ pure ()) (const $ pure ())

beginTransaction ::
  Store m ->
  -- | Defer rules until commit
  Bool ->
  m TransactionId
beginTransaction = beginTransactionImpl

commitTransaction :: Store m -> TransactionId -> m ()
commitTransaction = commitTransactionImpl

rollbackTransaction :: Store m -> TransactionId -> m ()
rollbackTransaction = rollbackTransactionImpl

listTransactions :: Store m -> m [TransactionId]
listTransactions = listTransactionsImpl

lookupTransaction :: Store m -> TransactionId -> m (Maybe Transaction)
lookupTransaction = lookupTransactionImpl

saveDeferred :: Store m -> TransactionId -> m ()
saveDeferred = saveDeferredImpl

restoreDeferred :: Store m -> TransactionId -> m ()
restoreDeferred = restoreDeferredImpl

export :: MonadError DiagnosticReports m => Store m -> TransactionId -> m LazyByteString
export store xactId = do
  resourceTy <- getResourceType store xactId "resource"
  tyIds <- listResource resourceTy

  resourceEntries <- for tyIds $ \tyId@(ResourceId resTy resName) -> do
    content <-
      fromMaybe (error $ renderResourceId tyId ++ " does not exist")
        <$> readResource resourceTy resName
    pure $ Tar.fileEntry (resTy </> resName) content

  entries <- for tyIds $ \(ResourceId _resTyName tyName) -> do
    resTy <- getResourceType store xactId tyName
    resources <- listResource resTy
    fmap concat . for resources $ \resId@(ResourceId resTyName resName) -> do
      content <-
        fromMaybe (error $ renderResourceId resId ++ " does not exist")
          <$> readResource resTy resName

      let contentEntry = Tar.fileEntry (resTyName </> resName) content

      properties <- listProperties resTy resName
      propertyEntries <- fmap catMaybes . for properties $ \propName -> do
        if propName == "metadata"
          then
            -- metadata is set on resource creation
            pure Nothing
          else do
            propValue <-
              fromMaybe (error $ renderResourceId resId ++ ":" ++ propName ++ " does not exist")
                <$> readProperty resTy resName propName
            pure . Just $ Tar.fileEntry (resTyName </> propertiesPart resName </> propName) propValue

      dependencies <- listDependencies resTy resName
      dependencyEntries <- for dependencies $ \dependency -> do
        pure $
          Tar.fileEntry
            (resTyName </> propertiesPart resName </> "dependencies" </> renderResourceId dependency)
            mempty

      pure $ contentEntry : propertyEntries ++ dependencyEntries

  pure $ Tar.write $ foldMap Tar.encodeLongNames (resourceEntries ++ concat entries)

import_ ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  Store m ->
  TransactionId ->
  LazyByteString ->
  m [ResourceId]
import_ store xactId archive = do
  let entries = Tar.decodeLongNames $ Tar.read archive
  Tar.foldEntries
    ( \entry rest -> do
        let path = Tar.entryTarPath entry
        content <-
          case Tar.entryContent entry of
            Tar.NormalFile content' _fileSize ->
              pure content'
            _ ->
              throwError . DiagnosticSimple $
                "archive entry "
                  ++ path
                  ++ " should be a file, got "
                  ++ case Tar.entryContent entry of
                    Tar.Directory -> "a directory"
                    Tar.SymbolicLink{} -> "a symbolic link"
                    Tar.HardLink{} -> "a hard link"
                    Tar.CharacterDevice{} -> "a character device"
                    Tar.BlockDevice{} -> "a block device"
                    Tar.NamedPipe -> "a named pipe"
                    Tar.OtherEntryType{} -> "an unknown entry type"

        mResourceId <-
          case splitDirectories path of
            [] -> undefined
            [resTyName, resName] -> do
              resTy <- getResourceType store xactId resTyName
              _changed <- writeResource resTy resName content
              pure . Just $ ResourceId resTyName resName
            [resTyName, part, propName] | (resName, ":properties") <- break (== ':') part -> do
              resTy <- getResourceType store xactId resTyName

              -- metadata is set on resource creation
              unless (propName == "metadata") $
                case parsePropertyValue content of
                  Left err ->
                    throwError $
                      DiagnosticReports
                        ( fromString $
                            "(" ++ renderResourceId (ResourceId (resourceTypeName resTy) resName) ++ ":" ++ propName ++ ")"
                        )
                        content
                        (sageErrorReport err)
                  Right value -> do
                    setProperty resTy resName propName $ metadataValueFromToml value

              pure Nothing
            [resTyName, part, "dependencies", subKey] | (resName, ":properties") <- break (== ':') part -> do
              resTy <- getResourceType store xactId resTyName
              let resId = readResourceId subKey
              createDependency resTy resName resId
              pure Nothing
            _ ->
              throwError . DiagnosticSimple $ "unrecognised archive path: " ++ path

        case mResourceId of
          Nothing -> rest
          Just resId -> (resId :) <$> rest
    )
    (pure [])
    ( \err -> do
        case err of
          Left err' -> do
            liftIO . putStrLn $ "TAR format error: " ++ show err'
          Right err' -> do
            liftIO . putStrLn $ "decode long names error: " ++ show err'
        throwError $ DiagnosticSimple "TAR format error"
    )
    entries

data ResourceType m
  = ResourceType
  { resourceTypeName :: !String
  , resourceTypeConfig :: !ResourceConfig
  , doesResourceExistImpl :: !(String -> m Bool)
  , readResourceImpl :: !(String -> m (Maybe LazyByteString))
  , writeResourceImpl :: !(String -> LazyByteString -> m Bool)
  , readPropertyImpl :: !(String -> String -> m (Maybe LazyByteString))
  , setPropertyImpl :: !(String -> String -> MetadataValue -> m ())
  , listPropertiesImpl :: !(String -> m [String])
  , listResourceImpl :: !(m [ResourceId])
  , readResourceMetadataImpl :: !(String -> m (Maybe LazyByteString))
  , readResourceModificationTimeImpl :: !(String -> m (Maybe UTCTime))
  , listDependenciesImpl :: !(String -> m [ResourceId])
  , listDependentsImpl :: !(String -> m [ResourceId])
  , createDependencyImpl :: !(String -> ResourceId -> m ())
  , removeDependencyImpl :: !(String -> ResourceId -> m ())
  }

hoistResourceType :: Functor m => (forall a. m a -> n a) -> ResourceType m -> ResourceType n
hoistResourceType f (ResourceType x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) =
  ResourceType
    x1
    x2
    (fmap f x3)
    (fmap f x4)
    (fmap (fmap f) x5)
    (fmap (fmap f) x6)
    (fmap (fmap (fmap f)) x7)
    (fmap f x8)
    (f x9)
    (fmap f x10)
    (fmap f x11)
    (fmap f x12)
    (fmap f x13)
    (fmap (fmap f) x14)
    (fmap (fmap f) x15)

orElseM :: Monad m => [m (Maybe a)] -> m (Maybe a)
orElseM [] = pure Nothing
orElseM (mma : mmas) = do
  ma <- mma
  case ma of
    Just{} -> pure ma
    Nothing -> orElseM mmas

resourceTypeFromDirectory ::
  forall m.
  (MonadError DiagnosticReports m, MonadCatch m, MonadIO m) =>
  -- | Store directory
  FilePath ->
  TransactionId ->
  -- | Resource type name
  String ->
  ResourceConfig ->
  IO (ResourceType m)
resourceTypeFromDirectory storeDir xactId resTyName config =
  mfix $ \self ->
    pure
      ResourceType
        { resourceTypeName = resTyName
        , resourceTypeConfig = config
        , writeResourceImpl = writeResourceImpl self
        , ..
        }
  where
    baseResTyDir = storeDir </> resTyName
    xactResTyDir change = getTransactionIdDir storeDir xactId </> changePart change </> resTyName

    orIO :: [IO Bool] -> IO Bool
    orIO [] = pure False
    orIO (mb : mbs) = do
      b <- mb
      if b then pure True else orIO mbs

    andIO :: [IO Bool] -> IO Bool
    andIO [] = pure True
    andIO (mb : mbs) = do
      b <- mb
      if b then andIO mbs else pure False

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
            orElseM
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

    xactCreateDir ::
      HasCallStack =>
      -- \| Directory, relative to store directory
      FilePath ->
      IO ()
    xactCreateDir dir = do
      let xactDir = getTransactionIdDir storeDir xactId

      inRemoved <- doesDirectoryExist $ xactDir </> changePart Delete </> dir
      if inRemoved
        then do
          isEmpty <- null <$> IO.listDirectory (xactDir </> changePart Delete </> dir)
          if isEmpty
            then do
              IO.removeDirectory $ xactDir </> changePart Delete </> dir
              IO.createDirectoryIfMissing True $ xactDir </> changePart Update </> dir
            else
              pure ()
        else do
          exists <- doesDirectoryExist $ storeDir </> dir
          if exists
            then pure ()
            else IO.createDirectoryIfMissing True $ xactDir </> changePart Create </> dir

    writeResourceImpl :: ResourceType m -> String -> LazyByteString -> m Bool
    writeResourceImpl self resName body = do
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
        newHash = Base16.encode $ Sha256.hashlazy body

        getOldHash = do
          mValue <- lookupProperty self resName "sha256"
          case mValue of
            Nothing ->
              pure Nothing
            Just (VString s) ->
              pure . Just $ Text.Encoding.encodeUtf8 s
            Just value ->
              error $
                renderResourceId (ResourceId resTyName resName)
                  ++ ":sha256 is not a string (got "
                  ++ show value
                  ++ ")"

        doWrite change
          | resTyName == "resource" = do
              _config <- parseResourceConfig resName body

              let
                onChange = do
                  liftIO $ xactWriteFile "resource" resName body
                  setProperty self resName "sha256" $ VString (Text.Encoding.decodeUtf8 newHash)
                  liftIO $ xactCreateDir resName

              case change of
                Delete ->
                  pure True
                Create -> do
                  onChange
                  pure True
                Update -> do
                  mOldHash <- getOldHash
                  let changed = mOldHash /= Just newHash
                  when changed onChange
                  pure changed
          | otherwise = do
              let
                onChange = do
                  liftIO $ IO.createDirectoryIfMissing False (xactResTyDir change)
                  metadata <- extractMetadata resTyName config resName body
                  updateMetadata resName metadata
                  liftIO $ xactWriteFile resTyName resName body
                  setProperty self resName "sha256" $ VString (Text.Encoding.decodeUtf8 newHash)

              case change of
                Delete ->
                  pure True
                Create -> do
                  onChange
                  pure True
                Update -> do
                  mOldHash <- getOldHash
                  let changed = mOldHash /= Just newHash
                  when changed onChange
                  pure changed

    updateMetadata :: String -> Metadata -> m ()
    updateMetadata resName metadata = do
      liftIO $ do
        xactWriteFile
          (resTyName </> propertiesPart resName)
          "metadata"
          (LazyByteString.fromStrict $ metadataSource metadata)

    readResourceMetadataImpl :: String -> m (Maybe LazyByteString)
    readResourceMetadataImpl resName =
      liftIO $ do
        removed <- doesFileExist $ xactResTyDir Delete </> resName
        if removed
          then pure Nothing
          else
            orElseM
              [ doRead $ xactResTyDir Create </> propertiesPart resName </> "metadata"
              , doRead $ xactResTyDir Update </> propertiesPart resName </> "metadata"
              , doRead $ baseResTyDir </> propertiesPart resName </> "metadata"
              ]
      where
        doRead path =
          fmap Just (IO.readFile path)
            `catch` \err@(WithCallStack _cs err') -> if isDoesNotExistError err' then pure Nothing else throwIO err

    readPropertyImpl :: String -> String -> m (Maybe LazyByteString)
    readPropertyImpl resName propName = do
      removed <- liftIO . doesFileExist $ xactResTyDir Delete </> propertiesPart resName </> propName
      if removed
        then pure Nothing
        else
          liftIO $
            orElseM
              [ doRead $ xactResTyDir Create </> propertiesPart resName </> propName
              , doRead $ xactResTyDir Update </> propertiesPart resName </> propName
              , doRead $ baseResTyDir </> propertiesPart resName </> propName
              ]
      where
        doRead path =
          fmap Just (IO.readFile path)
            `catch` \(WithCallStack _cs err) ->
              if isDoesNotExistError err
                then pure Nothing
                else throwM err

    setPropertyImpl :: String -> String -> MetadataValue -> m ()
    setPropertyImpl resName key value = do
      removed <- liftIO . doesFileExist $ xactResTyDir Delete </> propertiesPart resName </> key
      if removed
        then do
          liftIO . IO.removeFile $ xactResTyDir Delete </> propertiesPart resName </> key
          doWrite Update
        else do
          updated <- liftIO . doesFileExist $ baseResTyDir </> propertiesPart resName </> key
          if updated
            then
              doWrite Update
            else
              doWrite Create
      where
        doWrite change = do
          liftIO $ IO.createDirectoryIfMissing True (xactResTyDir change </> propertiesPart resName)
          let content = renderMetadataValueToml value
          liftIO $ xactWriteFile (resTyName </> propertiesPart resName) key content

    listPropertiesImpl :: String -> m [String]
    listPropertiesImpl resName = do
      removed <- liftIO . doesFileExist $ xactResTyDir Delete </> resName
      if removed
        then pure []
        else liftIO $ do
          deleted <- doList $ xactResTyDir Delete </> propertiesPart resName
          created <- doList $ xactResTyDir Create </> propertiesPart resName
          existing <- doList $ baseResTyDir </> propertiesPart resName
          pure $ (existing \\ (["dependencies", "dependents"] ++ deleted)) `union` created
      where
        doList path =
          IO.listDirectory path
            `catch` \(WithCallStack _cs err) ->
              if isDoesNotExistError err
                then pure []
                else throwM err

    readResourceModificationTimeImpl :: String -> m (Maybe UTCTime)
    readResourceModificationTimeImpl resName =
      liftIO $ do
        removed <- doesFileExist $ xactResTyDir Delete </> resName
        if removed
          then pure Nothing
          else
            orElseM
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
              <$> orElseM
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
              <$> orElseM
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
            , andIO
                [ fmap not . doesFileExist $ xactResTyDir Create </> resNameSrc
                , fmap not . doesFileExist $ baseResTyDir </> resNameSrc
                ]
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
            , andIO
                [ fmap not . doesFileExist $ xactResTyDir Create </> resNameSrc
                , fmap not . doesFileExist $ baseResTyDir </> resNameSrc
                ]
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

writeResource ::
  ResourceType m ->
  String ->
  LazyByteString ->
  -- | The resource's contents changed
  m Bool
writeResource = writeResourceImpl

readProperty :: ResourceType m -> String -> String -> m (Maybe LazyByteString)
readProperty = readPropertyImpl

setProperty :: ResourceType m -> String -> String -> MetadataValue -> m ()
setProperty = setPropertyImpl

listProperties :: ResourceType m -> String -> m [String]
listProperties = listPropertiesImpl

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

parsePropertyValue :: LazyByteString -> Either Toml.ParseError Toml.TomlValue
parsePropertyValue content =
  Sage.parse
    (Toml.valueParser Toml.TopLevel <* Sage.skipMany (Sage.satisfy Char.isSpace) <* Sage.eof)
    (LazyByteString.toStrict content)

lookupProperty ::
  MonadError DiagnosticReports m => ResourceType m -> String -> String -> m (Maybe MetadataValue)
lookupProperty resTy resName propName = do
  mContent <- readProperty resTy resName propName
  case mContent of
    Nothing -> pure Nothing
    Just content ->
      case parsePropertyValue content of
        Right x -> pure . Just $ metadataValueFromToml x
        Left err ->
          throwError $
            DiagnosticReports
              (fromString $ "(" ++ renderResourceId (ResourceId (resourceTypeName resTy) resName) ++ ")")
              content
              (sageErrorReport err)

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
    case Pandoc.runPure $ Pandoc.readMarkdown markdownReaderOptions body' of
      Left err -> error "TODO: " err
      Right x -> pure x

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
