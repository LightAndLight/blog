module Blog.Store.Overlay
  ( Overlay (..)
  , OverlayChanges (..)
  , overlayReadFile
  , overlayDoesFileExist
  , overlayGetModificationTime
  , overlayWriteFile
  , overlayRemoveFile
  , overlayListDir
  , overlayDoesDirectoryExist
  , overlayCreateDir
  , overlayRemoveDir
  , overlayCommit
  )
where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (LazyByteString)
import Data.Foldable (for_)
import Data.List (inits, isPrefixOf, union)
import Data.Maybe (fromMaybe, isNothing)
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified IO
import System.Directory (doesDirectoryExist, doesFileExist, doesPathExist)
import System.FilePath (splitDirectories, takeDirectory, (</>))

data Overlay
  = Overlay
  { overlayBase :: !FilePath
  , overlayChanges :: !(Maybe OverlayChanges)
  {- ^ 'Nothing' means the overlay is read-only.

  Mutation operations will fail with an error.
  -}
  }
  deriving (Show)

data OverlayChanges
  = OverlayChanges
  { overlayCreate :: !FilePath
  , overlayUpdate :: !FilePath
  , overlayDelete :: !FilePath
  }
  deriving (Show)

orIO :: [IO Bool] -> IO Bool
orIO [] = pure False
orIO (mb : mbs) = do
  b <- mb
  if b then pure True else orIO mbs

orElseM :: Monad m => [m (Maybe a)] -> m (Maybe a)
orElseM [] = pure Nothing
orElseM (mma : mmas) = do
  ma <- mma
  case ma of
    Just{} -> pure ma
    Nothing -> orElseM mmas

overlayDeleted :: Overlay -> FilePath -> IO Bool
overlayDeleted overlay path =
  case overlayChanges overlay of
    Nothing -> pure False
    Just changes -> do
      let prefixes = fmap (foldr1 (</>)) . drop 1 . inits $ splitDirectories path
      orIO $
        doesFileExist (overlayDelete changes </> path)
          : fmap
            ( \prefix -> do
                let prefix' = overlayDelete changes </> prefix
                isDir <- doesDirectoryExist prefix'
                if isDir
                  then null <$> IO.listDirectory prefix'
                  else pure False
            )
            prefixes

ifChanges :: Overlay -> a -> (OverlayChanges -> a) -> a
ifChanges overlay def f = maybe def f (overlayChanges overlay)

overlayResolve :: Overlay -> FilePath -> IO (Maybe FilePath)
overlayResolve overlay path = do
  created <- ifChanges overlay (pure Nothing) $ \changes -> tryPath $ overlayCreate changes </> path
  case created of
    Just resolved -> pure $ Just resolved
    Nothing -> do
      removed <- overlayDeleted overlay path
      if removed
        then pure Nothing
        else
          orElseM
            [ ifChanges overlay (pure Nothing) $ \changes -> tryPath $ overlayUpdate changes </> path
            , tryPath $ overlayBase overlay </> path
            ]
  where
    tryPath path' = do
      exists <- doesPathExist path'
      if exists
        then pure $ Just path'
        else pure Nothing

overlayListDir :: Overlay -> Maybe FilePath -> IO [FilePath]
overlayListDir overlay mPath = do
  removed <- maybe (pure False) (overlayDeleted overlay) mPath
  if removed
    then do
      ifChanges overlay (pure []) $ \changes -> fmap (fromMaybe []) . list $ appendPath (overlayCreate changes)
    else do
      created <- ifChanges overlay (pure []) $ \changes -> fmap (fromMaybe []) . list $ appendPath (overlayCreate changes)
      existing <- fmap (fromMaybe []) . list $ appendPath (overlayBase overlay)
      deleted <- ifChanges overlay (pure []) $ \changes -> fmap (fromMaybe []) . listRecursive $ appendPath (overlayDelete changes)
      pure $
        created
          `union` filter
            ( \existingPath ->
                not $
                  any
                    ( \deletedPath ->
                        splitDirectories deletedPath `isPrefixOf` splitDirectories existingPath
                    )
                    deleted
            )
            existing
  where
    appendPath base =
      case mPath of
        Nothing -> base
        Just path -> base </> path

overlayDoesDirectoryExist :: Overlay -> FilePath -> IO Bool
overlayDoesDirectoryExist overlay path = do
  created <- ifChanges overlay (pure False) $ \changes -> doesDirectoryExist $ overlayCreate changes </> path
  if created
    then pure True
    else do
      deleted <- overlayDeleted overlay path
      if deleted
        then pure False
        else doesDirectoryExist $ overlayBase overlay </> path

list :: HasCallStack => FilePath -> IO (Maybe [FilePath])
list base =
  withFrozenCallStack
    ( do
        exists <- doesDirectoryExist base
        if exists
          then Just <$> IO.listDirectory base
          else pure Nothing
    )

listRecursive :: HasCallStack => FilePath -> IO (Maybe [FilePath])
listRecursive base =
  withFrozenCallStack
    ( do
        exists <- doesDirectoryExist base
        if exists
          then Just <$> go Nothing
          else pure Nothing
    )
  where
    go :: HasCallStack => Maybe FilePath -> IO [FilePath]
    go mPath = do
      let path' = maybe base (base </>) mPath
      entries <- IO.listDirectory path'
      fmap concat . for entries $ \entry -> do
        isDir <- doesDirectoryExist $ path' </> entry
        if isDir
          then do
            entries' <- go . Just $ maybe entry (</> entry) mPath
            if null entries'
              then pure [maybe entry (</> entry) mPath]
              else pure entries'
          else pure [maybe entry (</> entry) mPath]

overlayDoesFileExist :: Overlay -> FilePath -> IO Bool
overlayDoesFileExist overlay path = do
  mPath <- overlayResolve overlay path
  case mPath of
    Nothing -> pure False
    Just path' -> doesFileExist path'

overlayGetModificationTime :: Overlay -> FilePath -> IO (Maybe UTCTime)
overlayGetModificationTime overlay path = do
  mPath <- overlayResolve overlay path
  case mPath of
    Nothing -> pure Nothing
    Just path' -> Just <$> IO.getModificationTime path'

overlayReadFile ::
  HasCallStack =>
  Overlay ->
  FilePath ->
  IO (Maybe ByteString)
overlayReadFile overlay path = do
  mPath <- overlayResolve overlay path
  case mPath of
    Nothing -> pure Nothing
    Just path' -> Just <$> ByteString.readFile path'

-- | Precondition: overlay is writeable
overlayWriteFile ::
  HasCallStack =>
  Overlay ->
  FilePath ->
  LazyByteString ->
  IO ()
overlayWriteFile overlay path content =
  case overlayChanges overlay of
    Nothing ->
      error "overlay is read-only"
    Just changes -> do
      removed <- doesFileExist $ overlayDelete changes </> path
      path' <-
        if removed
          then pure $ overlayCreate changes </> path
          else do
            do
              dirExists <-
                orIO
                  [ doesDirectoryExist $ overlayCreate changes </> takeDirectory path
                  , doesDirectoryExist $ overlayBase overlay </> takeDirectory path
                  ]
              unless dirExists $
                error $
                  "directory of " ++ path ++ " does not exist"
            created <- doesFileExist $ overlayCreate changes </> path
            if created
              then pure $ overlayCreate changes </> path
              else do
                updated <- doesFileExist $ overlayUpdate changes </> path
                if updated
                  then pure $ overlayUpdate changes </> path
                  else do
                    exists <- doesFileExist $ overlayBase overlay </> path
                    if exists
                      then pure $ overlayUpdate changes </> path
                      else pure $ overlayCreate changes </> path
      IO.createDirectoryIfMissing True $ takeDirectory path'
      IO.writeFile path' content

-- | Precondition: overlay is writeable
overlayCreateDir :: HasCallStack => Overlay -> FilePath -> IO ()
overlayCreateDir overlay path =
  case overlayChanges overlay of
    Nothing ->
      error "overlay is read-only"
    Just changes -> do
      missing <- isNothing <$> overlayResolve overlay path
      when missing $ do
        let path' = overlayCreate changes </> path
        IO.createDirectoryIfMissing True $ takeDirectory path'
        IO.createDirectory path'

-- | Precondition: overlay is writeable
overlayRemoveDir :: HasCallStack => Overlay -> FilePath -> IO ()
overlayRemoveDir overlay path =
  case overlayChanges overlay of
    Nothing ->
      error "overlay is read-only"
    Just changes -> do
      missing <- isNothing <$> overlayResolve overlay path
      unless missing $ do
        doRemove $ overlayCreate changes
        doRemove $ overlayUpdate changes

        inBase <- doesDirectoryExist $ overlayBase overlay </> path
        when inBase $ do
          let path' = overlayDelete changes </> path
          exists <- doesDirectoryExist path'
          if exists
            then clearDirectory path'
            else do
              IO.createDirectoryIfMissing True $ takeDirectory path'
              IO.createDirectory path'
  where
    doRemove dir = do
      exists <- doesDirectoryExist $ dir </> path
      when exists . IO.removeDirectoryRecursive $ dir </> path

-- | Precondition: overlay is writeable
overlayRemoveFile ::
  HasCallStack =>
  Overlay ->
  FilePath ->
  IO ()
overlayRemoveFile overlay path =
  case overlayChanges overlay of
    Nothing ->
      error "overlay is read-only"
    Just changes -> do
      missing <- isNothing <$> overlayResolve overlay path
      unless missing $ do
        doRemove $ overlayCreate changes
        doRemove $ overlayUpdate changes

        inBase <- doesFileExist $ overlayBase overlay </> path
        when inBase $ do
          IO.createDirectoryIfMissing True $ overlayDelete changes </> takeDirectory path
          IO.writeFile (overlayDelete changes </> path) mempty
  where
    doRemove dir = do
      exists <- doesFileExist $ dir </> path
      when exists . IO.removeFile $ dir </> path

overlayCommit :: Overlay -> IO ()
overlayCommit overlay =
  for_ (overlayChanges overlay) $ \changes -> do
    deleted <- fromMaybe [] <$> listRecursive (overlayDelete changes)
    for_ deleted $ \path -> do
      isDir <- doesDirectoryExist $ overlayDelete changes </> path
      if isDir
        then do
          IO.removeDirectoryRecursive $ overlayBase overlay </> path
          IO.removeDirectory $ overlayDelete changes </> path
        else do
          IO.removeFile $ overlayBase overlay </> path
          IO.removeFile $ overlayDelete changes </> path

    clearDirectory $ overlayDelete changes
    created <- fromMaybe [] <$> listRecursive (overlayCreate changes)
    for_ created $ \path -> do
      let source = overlayCreate changes </> path
      let target = overlayBase overlay </> path
      IO.createDirectoryIfMissing True $ takeDirectory target
      isDir <- doesDirectoryExist source
      if isDir
        then IO.createDirectoryIfMissing False target
        else IO.renamePath source target
    clearDirectory $ overlayCreate changes

    updated <- fromMaybe [] <$> listRecursive (overlayUpdate changes)
    for_ updated $ \path -> do
      let source = overlayUpdate changes </> path
      let target = overlayBase overlay </> path
      IO.createDirectoryIfMissing True $ takeDirectory target
      isDir <- doesDirectoryExist source
      if isDir
        then IO.createDirectoryIfMissing False target
        else IO.renamePath source target
    clearDirectory $ overlayUpdate changes

clearDirectory :: FilePath -> IO ()
clearDirectory path = do
  entries <- IO.listDirectory path
  for_ entries $ \entry -> do
    isDir <- doesDirectoryExist $ path </> entry
    if isDir
      then IO.removeDirectoryRecursive $ path </> entry
      else IO.removeFile $ path </> entry
