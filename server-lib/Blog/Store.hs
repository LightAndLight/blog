{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  , removeResource
  , lookupProperty
  , setProperty
  , readProperty
  , listResource
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
  , Name
  , ResourceConfig (..)
  , ResourceId (..)
  , nameToPath
  , pathToName
  , pathToResourceId
  , propertiesPart
  , renderName
  , renderResourceId
  , resourceConfigDecoder
  , resourceIdToPath
  , unsafeName
  )
import Blog.Diagnostic (DiagnosticReports (..))
import Blog.Error (sageErrorReport, tomlResult)
import Blog.ID (ID)
import qualified Blog.ID as ID
import Blog.Metadata (metadataValueFromToml, renderMetadataValueToml, resourceMetadataDecoder)
import Blog.Pandoc (markdownReaderOptions)
import Blog.Store.Overlay
  ( Overlay (..)
  , overlayCommit
  , overlayCreateDir
  , overlayDoesDirectoryExist
  , overlayDoesFileExist
  , overlayGetModificationTime
  , overlayListDir
  , overlayReadFile
  , overlayRemoveDir
  , overlayRemoveFile
  , overlayWriteFile
  )
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar (entryTarPath, fileEntry)
import Control.Exception (throwIO)
import Control.Monad (guard, unless, when)
import Control.Monad.Catch (ExitCase (..), MonadCatch, MonadMask, catch, generalBracket)
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
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid (First (..))
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import GHC.Stack (HasCallStack, callStack, getCallStack, prettySrcLoc)
import IO (WithCallStack (..))
import qualified IO
import System.Directory
  ( createDirectory
  , doesDirectoryExist
  , removeDirectoryRecursive
  , renameDirectory
  )
import System.FilePath (splitDirectories, (</>))
import System.IO.Error (isDoesNotExistError)
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Definition (Block (..))
import Text.Pandoc.Walk (query)
import qualified Text.Sage as Sage
import qualified Toml

data Store m
  = Store
  { lookupResourceTypeImpl :: !(TransactionId -> Name -> m (Maybe (ResourceType m)))
  , beginTransactionImpl :: Bool -> m TransactionId
  , commitTransactionImpl :: TransactionId -> m ()
  , rollbackTransactionImpl :: TransactionId -> m ()
  , listTransactionsImpl :: m [TransactionId]
  , lookupTransactionImpl :: TransactionId -> m (Maybe Transaction)
  , saveDeferredImpl :: TransactionId -> m ()
  , restoreDeferredImpl :: TransactionId -> m ()
  }

hoistStore :: Functor m => (forall a. m a -> n a) -> Store m -> Store n
hoistStore f Store{..} =
  Store
    { lookupResourceTypeImpl = \xactId resTyName ->
        f $ fmap (hoistResourceType f) <$> lookupResourceTypeImpl xactId resTyName
    , beginTransactionImpl = f . beginTransactionImpl
    , commitTransactionImpl = f . commitTransactionImpl
    , rollbackTransactionImpl = f . rollbackTransactionImpl
    , listTransactionsImpl = f listTransactionsImpl
    , lookupTransactionImpl = f . lookupTransactionImpl
    , saveDeferredImpl = f . saveDeferredImpl
    , restoreDeferredImpl = f . restoreDeferredImpl
    }

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
    getOverlay xactId =
      Overlay
        { overlayCreate = getTransactionIdDir storeDir xactId </> changePart Create
        , overlayUpdate = getTransactionIdDir storeDir xactId </> changePart Update
        , overlayDelete = getTransactionIdDir storeDir xactId </> changePart Delete
        , overlayBase = storeDir
        }

    lookupResourceTypeImpl :: TransactionId -> Name -> m (Maybe (ResourceType m))
    lookupResourceTypeImpl xactId resTyName
      | renderName resTyName == "resource" = do
          let config = ResourceConfig (fromString "text/toml") mempty
          liftIO $ Just <$> resourceTypeFromDirectory storeDir xactId resTyName config
      | otherwise = do
          let overlay = getOverlay xactId
          let resourceConfigPath = "resource" </> nameToPath resTyName
          exists <- liftIO $ overlayDoesFileExist overlay resourceConfigPath
          if exists
            then do
              content <-
                liftIO $
                  fromMaybe (error $ resourceConfigPath ++ " not found")
                    <$> overlayReadFile overlay resourceConfigPath
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
          overlayCommit $ getOverlay xactId
          removeDirectoryRecursive xactDir
        else
          throwError . DiagnosticSimple $ "transaction not found: " ++ renderTransactionId xactId

    rollbackTransactionImpl :: TransactionId -> m ()
    rollbackTransactionImpl xactId = do
      liftIO $
        removeDirectoryRecursive (getTransactionIdDir storeDir xactId)
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
                  let resTyName' = pathToName resTyName
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
                        pure $ ResourceId resTyName' (pathToName prefix)
                      else pure $ ResourceId resTyName' (pathToName entry)

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
  Name ->
  ByteString ->
  m ResourceConfig
parseResourceConfig resTyName body = do
  let resourceFile = fromString $ "(resource:" ++ renderName resTyName ++ ")"
  toml <- tomlResult resourceFile body $ Toml.parse body
  tomlResult resourceFile body $ Toml.decode toml resourceConfigDecoder

lookupResourceType ::
  Store m ->
  TransactionId ->
  -- | Resource type
  Name ->
  m (Maybe (ResourceType m))
lookupResourceType = lookupResourceTypeImpl

getResourceType ::
  HasCallStack =>
  (MonadError DiagnosticReports m, MonadIO m) =>
  Store m ->
  TransactionId ->
  -- | Resource type
  Name ->
  m (ResourceType m)
getResourceType store xactId resTyName = do
  mResTy <- lookupResourceType store xactId resTyName
  case mResTy of
    Just x -> pure x
    Nothing -> do
      liftIO . putStrLn $
        "error: getResourceType failed "
          ++ maybe
            "(location unknown)"
            (\(_fn, loc) -> "(" ++ prettySrcLoc loc ++ ")")
            (listToMaybe $ getCallStack callStack)
      throwError . DiagnosticSimple $
        "resource type '" ++ renderName resTyName ++ "' does not exist"

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

export ::
  (MonadError DiagnosticReports m, MonadIO m) => Store m -> TransactionId -> m LazyByteString
export store xactId = do
  resourceTy <- getResourceType store xactId (unsafeName "resource")
  tyIds <- listResource resourceTy

  resourceEntries <- for tyIds $ \tyId@(ResourceId resTy resName) -> do
    content <-
      fromMaybe (error $ renderResourceId tyId ++ " does not exist")
        <$> readResource resourceTy resName
    pure $ Tar.fileEntry (nameToPath resTy </> nameToPath resName) (LazyByteString.fromStrict content)

  entries <- for tyIds $ \(ResourceId _resTyName tyName) -> do
    resTy <- getResourceType store xactId tyName
    resources <- listResource resTy
    fmap concat . for resources $ \resId@(ResourceId resTyName resName) -> do
      content <-
        fromMaybe (error $ renderResourceId resId ++ " does not exist")
          <$> readResource resTy resName

      let contentEntry = Tar.fileEntry (nameToPath resTyName </> nameToPath resName) (LazyByteString.fromStrict content)

      properties <- listProperties resTy resName
      propertyEntries <- fmap catMaybes . for properties $ \propName -> do
        if renderName propName == "metadata"
          then
            -- metadata is set on resource creation
            pure Nothing
          else do
            propValue <-
              fromMaybe (error $ renderResourceId resId ++ ":" ++ renderName propName ++ " does not exist")
                <$> readProperty resTy resName propName
            pure . Just $
              Tar.fileEntry
                (nameToPath resTyName </> propertiesPart resName </> nameToPath propName)
                (LazyByteString.fromStrict propValue)

      dependencies <- listDependencies resTy resName
      dependencyEntries <- for dependencies $ \dependency -> do
        pure $
          Tar.fileEntry
            ( nameToPath resTyName
                </> propertiesPart resName
                </> "dependencies"
                </> resourceIdToPath dependency
            )
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
              let resTyName' = pathToName resTyName
              let resName' = pathToName resName
              resTy <- getResourceType store xactId resTyName'
              _changed <- writeResource resTy resName' content
              pure . Just $ ResourceId resTyName' resName'
            [resTyName, part, propName] | (resName, ":properties") <- break (== ':') part -> do
              let resTyName' = pathToName resTyName
              let resName' = pathToName resName
              let propName' = pathToName propName
              resTy <- getResourceType store xactId resTyName'

              -- metadata is set on resource creation
              unless (propName == "metadata") $
                case parsePropertyValue $ LazyByteString.toStrict content of
                  Left err ->
                    throwError $
                      DiagnosticReports
                        ( fromString $
                            "(" ++ renderResourceId (ResourceId (resourceTypeName resTy) resName') ++ ":" ++ propName ++ ")"
                        )
                        content
                        (sageErrorReport err)
                  Right value -> do
                    setProperty resTy resName' propName' $ metadataValueFromToml value

              pure Nothing
            [resTyName, part, "dependencies", subKey] | (resName, ":properties") <- break (== ':') part -> do
              let resTyName' = pathToName resTyName
              let resName' = pathToName resName
              resTy <- getResourceType store xactId resTyName'
              let resId = pathToResourceId subKey
              createDependency resTy resName' resId
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
  { resourceTypeName :: !Name
  , resourceTypeConfig :: !ResourceConfig
  , doesResourceExistImpl :: !(Name -> m Bool)
  , readResourceImpl :: !(Name -> m (Maybe ByteString))
  , writeResourceImpl :: !(Name -> LazyByteString -> m Bool)
  , removeResourceImpl :: !(Name -> m ())
  , readPropertyImpl :: !(Name -> Name -> m (Maybe ByteString))
  , setPropertyImpl :: !(Name -> Name -> MetadataValue -> m ())
  , listPropertiesImpl :: !(Name -> m [Name])
  , listResourceImpl :: !(m [ResourceId])
  , readResourceModificationTimeImpl :: !(Name -> m (Maybe UTCTime))
  , listDependenciesImpl :: !(Name -> m [ResourceId])
  , listDependentsImpl :: !(Name -> m [ResourceId])
  , createDependencyImpl :: !(Name -> ResourceId -> m ())
  , removeDependencyImpl :: !(Name -> ResourceId -> m ())
  }

hoistResourceType :: (forall a. m a -> n a) -> ResourceType m -> ResourceType n
hoistResourceType f ResourceType{..} =
  ResourceType
    { resourceTypeName
    , resourceTypeConfig
    , doesResourceExistImpl = f . doesResourceExistImpl
    , readResourceImpl = f . readResourceImpl
    , writeResourceImpl = \resName content -> f $ writeResourceImpl resName content
    , removeResourceImpl = \resName -> f $ removeResourceImpl resName
    , readPropertyImpl = \resName propName -> f $ readPropertyImpl resName propName
    , setPropertyImpl = \resName key value -> f $ setPropertyImpl resName key value
    , listPropertiesImpl = f . listPropertiesImpl
    , listResourceImpl = f listResourceImpl
    , readResourceModificationTimeImpl = f . readResourceModificationTimeImpl
    , listDependenciesImpl = f . listDependenciesImpl
    , listDependentsImpl = f . listDependentsImpl
    , createDependencyImpl = \resNameSrc dependencyTarget -> f $ createDependencyImpl resNameSrc dependencyTarget
    , removeDependencyImpl = \resNameSrc dependencyTarget -> f $ removeDependencyImpl resNameSrc dependencyTarget
    }

resourceTypeFromDirectory ::
  forall m.
  (MonadError DiagnosticReports m, MonadCatch m, MonadIO m) =>
  -- | Store directory
  FilePath ->
  TransactionId ->
  -- | Resource type name
  Name ->
  ResourceConfig ->
  IO (ResourceType m)
resourceTypeFromDirectory storeDir xactId resTyName config =
  mfix $ \self ->
    pure
      ResourceType
        { resourceTypeName = resTyName
        , resourceTypeConfig = config
        , writeResourceImpl = writeResourceImpl self
        , removeResourceImpl = removeResourceImpl self
        , ..
        }
  where
    getOverlay resTyName' =
      Overlay
        { overlayCreate = getTransactionIdDir storeDir xactId </> changePart Create </> resTyName'
        , overlayUpdate = getTransactionIdDir storeDir xactId </> changePart Update </> resTyName'
        , overlayDelete = getTransactionIdDir storeDir xactId </> changePart Delete </> resTyName'
        , overlayBase = storeDir </> resTyName'
        }

    overlay = getOverlay $ nameToPath resTyName

    doesResourceExistImpl :: Name -> m Bool
    doesResourceExistImpl resName = liftIO $ overlayDoesFileExist overlay (nameToPath resName)

    readResourceImpl :: Name -> m (Maybe ByteString)
    readResourceImpl resName = liftIO $ overlayReadFile overlay (nameToPath resName)

    writeResourceImpl :: ResourceType m -> Name -> LazyByteString -> m Bool
    writeResourceImpl self resName body = do
      exists <- doesResourceExist self resName
      mOldHash <-
        if exists
          then do
            mValue <- lookupProperty self resName (unsafeName "sha256")
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
          else do
            liftIO $ overlayCreateDir overlay (propertiesPart resName)
            pure Nothing

      let changed = mOldHash /= Just newHash
      when changed $ do
        liftIO $ overlayWriteFile overlay (nameToPath resName) body
        setProperty self resName (unsafeName "sha256") $ VString (Text.Encoding.decodeUtf8 newHash)
        mMetadata <- extractMetadata resTyName config resName body
        for_ mMetadata $ updateMetadata resName

      pure changed
      where
        newHash = Base16.encode $ Sha256.hashlazy body

    updateMetadata :: Name -> ByteString -> m ()
    updateMetadata resName metadata = do
      liftIO $
        overlayWriteFile
          overlay
          (propertiesPart resName </> "metadata")
          (LazyByteString.fromStrict metadata)

    removeResourceImpl :: ResourceType m -> Name -> m ()
    removeResourceImpl self resName = do
      exists <- doesResourceExist self resName
      if exists
        then do
          dependents <- listDependents self resName
          unless (null dependents) $
            throwError . DiagnosticSimple $
              "resource "
                ++ renderResourceId (ResourceId resTyName resName)
                ++ " still has dependents: "
                ++ intercalate ", " (fmap renderResourceId dependents)
          liftIO $ do
            overlayRemoveFile overlay (nameToPath resName)
            overlayRemoveDir overlay (propertiesPart resName)
        else
          throwError . DiagnosticSimple $
            "resource " ++ renderResourceId (ResourceId resTyName resName) ++ " does not exist"

    readPropertyImpl :: Name -> Name -> m (Maybe ByteString)
    readPropertyImpl resName propName =
      liftIO $ overlayReadFile overlay (propertiesPart resName </> nameToPath propName)

    setPropertyImpl :: Name -> Name -> MetadataValue -> m ()
    setPropertyImpl resName key value =
      liftIO $
        overlayWriteFile
          overlay
          (propertiesPart resName </> nameToPath key)
          (renderMetadataValueToml value)

    listPropertiesImpl :: Name -> m [Name]
    listPropertiesImpl resName = liftIO $ do
      entries <- overlayListDir overlay (Just $ propertiesPart resName)
      pure $ fmap pathToName entries

    readResourceModificationTimeImpl :: Name -> m (Maybe UTCTime)
    readResourceModificationTimeImpl resName =
      liftIO $ overlayGetModificationTime overlay (nameToPath resName)

    listResourceImpl :: m [ResourceId]
    listResourceImpl =
      liftIO $ do
        entries <- overlayListDir overlay Nothing
        pure $
          mapMaybe
            ( \entry -> do
                let (_prefix, suffix) = break (== ':') entry
                guard $ suffix /= ":properties"
                pure $ ResourceId resTyName (pathToName entry)
            )
            entries

    listDependenciesImpl :: Name -> m [ResourceId]
    listDependenciesImpl resName = do
      liftIO $ do
        entries <- overlayListDir overlay . Just $ propertiesPart resName </> "dependencies"
        pure $ fmap pathToResourceId entries

    listDependentsImpl :: Name -> m [ResourceId]
    listDependentsImpl resName = do
      liftIO $ do
        entries <- overlayListDir overlay . Just $ propertiesPart resName </> "dependents"
        pure $ fmap pathToResourceId entries

    createDependencyImpl :: Name -> ResourceId -> m ()
    createDependencyImpl resNameSrc dependencyTarget@(ResourceId resTyNameTgt resNameTgt) = do
      let dependencySource = ResourceId resTyName resNameSrc

      srcExists <- liftIO $ overlayDoesFileExist overlay (nameToPath resNameSrc)
      if srcExists
        then liftIO $ do
          let dir = propertiesPart resNameSrc </> "dependencies"
          dirExists <- overlayDoesDirectoryExist overlay dir
          unless dirExists $ overlayCreateDir overlay dir
          overlayWriteFile
            overlay
            (dir </> resourceIdToPath dependencyTarget)
            mempty
        else
          throwError . DiagnosticSimple $
            "dependency source " ++ renderResourceId dependencySource ++ " does not exist"

      let tgtOverlay = getOverlay $ nameToPath resTyNameTgt
      tgtExists <- liftIO $ overlayDoesFileExist tgtOverlay (nameToPath resNameTgt)
      if tgtExists
        then liftIO $ do
          let dir = propertiesPart resNameTgt </> "dependents"
          dirExists <- overlayDoesDirectoryExist tgtOverlay dir
          unless dirExists $ overlayCreateDir tgtOverlay dir
          overlayWriteFile
            tgtOverlay
            (dir </> resourceIdToPath dependencySource)
            mempty
        else
          throwError . DiagnosticSimple $
            "dependency target " ++ renderResourceId dependencyTarget ++ " does not exist"

    removeDependencyImpl :: Name -> ResourceId -> m ()
    removeDependencyImpl resNameSrc dependencyTarget@(ResourceId resTyNameTgt resNameTgt) = do
      let dependencySource = ResourceId resTyName resNameSrc

      liftIO $
        overlayRemoveFile
          overlay
          (propertiesPart resNameSrc </> "dependencies" </> resourceIdToPath dependencyTarget)

      let tgtOverlay = getOverlay (nameToPath resTyNameTgt)
      liftIO $
        overlayRemoveFile
          tgtOverlay
          (propertiesPart resNameTgt </> "dependents" </> resourceIdToPath dependencySource)

doesResourceExist :: ResourceType m -> Name -> m Bool
doesResourceExist = doesResourceExistImpl

readResource :: ResourceType m -> Name -> m (Maybe ByteString)
readResource = readResourceImpl

writeResource ::
  ResourceType m ->
  Name ->
  LazyByteString ->
  -- | The resource's contents changed
  m Bool
writeResource = writeResourceImpl

removeResource :: ResourceType m -> Name -> m ()
removeResource = removeResourceImpl

readProperty :: ResourceType m -> Name -> Name -> m (Maybe ByteString)
readProperty = readPropertyImpl

setProperty :: ResourceType m -> Name -> Name -> MetadataValue -> m ()
setProperty = setPropertyImpl

listProperties :: ResourceType m -> Name -> m [Name]
listProperties = listPropertiesImpl

listResource :: ResourceType m -> m [ResourceId]
listResource = listResourceImpl

readResourceModificationTime :: ResourceType m -> Name -> m (Maybe UTCTime)
readResourceModificationTime = readResourceModificationTimeImpl

listDependencies :: ResourceType m -> Name -> m [ResourceId]
listDependencies = listDependenciesImpl

listDependents :: ResourceType m -> Name -> m [ResourceId]
listDependents = listDependentsImpl

createDependency ::
  ResourceType m ->
  -- | Source name
  Name ->
  -- | Target ID
  ResourceId ->
  m ()
createDependency = createDependencyImpl

removeDependency :: ResourceType m -> Name -> ResourceId -> m ()
removeDependency = removeDependencyImpl

parsePropertyValue :: ByteString -> Either Toml.ParseError Toml.TomlValue
parsePropertyValue =
  Sage.parse
    (Toml.valueParser Toml.TopLevel <* Sage.skipMany (Sage.satisfy Char.isSpace) <* Sage.eof)

lookupProperty ::
  MonadError DiagnosticReports m => ResourceType m -> Name -> Name -> m (Maybe MetadataValue)
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
              (LazyByteString.fromStrict content)
              (sageErrorReport err)

extractMetadata ::
  MonadError DiagnosticReports m =>
  -- | Resource type
  Name ->
  ResourceConfig ->
  -- | Resource name
  Name ->
  LazyByteString.ByteString ->
  m (Maybe ByteString)
extractMetadata resTyName config resName body =
  if cfgContentType config == fromString "text/markdown"
    then extractMetadataMarkdown resTyName config resName body
    else pure Nothing

extractMetadataMarkdown ::
  MonadError DiagnosticReports m =>
  -- | Resourced type
  Name ->
  ResourceConfig ->
  -- | Resource name
  Name ->
  LazyByteString.ByteString ->
  m (Maybe ByteString)
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
          Just $ Text.Encoding.encodeUtf8 content
      | otherwise = Nothing
    metadataBlock _ = Nothing

  case getFirst $ query @Block (First . metadataBlock) markdown of
    Nothing ->
      pure Nothing
    Just content -> do
      -- validate the metadata before returning
      let decoder = resourceMetadataDecoder config
      let
        resourceFile =
          fromString $
            "(" ++ renderResourceId (ResourceId resTyName resName) ++ ":metadata)"
      toml <- tomlResult resourceFile content $ Toml.parse content
      _values <- tomlResult resourceFile content $ Toml.decode toml decoder

      pure $ Just content
