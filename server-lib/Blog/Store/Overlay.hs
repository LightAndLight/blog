module Blog.Store.Overlay
  ( Overlay (..)
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
  , overlayCreate :: !FilePath
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
overlayDeleted overlay path = do
  let prefixes = fmap (foldr1 (</>)) . drop 1 . inits $ splitDirectories path
  orIO $
    doesFileExist (overlayDelete overlay </> path)
      : fmap
        ( \prefix -> do
            let prefix' = overlayDelete overlay </> prefix
            isDir <- doesDirectoryExist prefix'
            if isDir
              then null <$> IO.listDirectory prefix'
              else pure False
        )
        prefixes

overlayResolve :: Overlay -> FilePath -> IO (Maybe FilePath)
overlayResolve overlay path = do
  created <- tryPath $ overlayCreate overlay </> path
  case created of
    Just resolved -> pure $ Just resolved
    Nothing -> do
      removed <- overlayDeleted overlay path
      if removed
        then pure Nothing
        else
          orElseM
            [ tryPath $ overlayUpdate overlay </> path
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
      fmap (fromMaybe []) . list $ appendPath (overlayCreate overlay)
    else do
      created <- fmap (fromMaybe []) . list $ appendPath (overlayCreate overlay)
      existing <- fmap (fromMaybe []) . list $ appendPath (overlayBase overlay)
      deleted <- fmap (fromMaybe []) . listRecursive $ appendPath (overlayDelete overlay)
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
  created <- doesDirectoryExist $ overlayCreate overlay </> path
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

overlayWriteFile ::
  HasCallStack =>
  Overlay ->
  FilePath ->
  LazyByteString ->
  IO ()
overlayWriteFile overlay path content = do
  removed <- doesFileExist $ overlayDelete overlay </> path
  path' <-
    if removed
      then pure $ overlayCreate overlay </> path
      else do
        do
          dirExists <-
            orIO
              [ doesDirectoryExist $ overlayCreate overlay </> takeDirectory path
              , doesDirectoryExist $ overlayBase overlay </> takeDirectory path
              ]
          unless dirExists $
            error $
              "directory of " ++ path ++ " does not exist"
        created <- doesFileExist $ overlayCreate overlay </> path
        if created
          then pure $ overlayCreate overlay </> path
          else do
            updated <- doesFileExist $ overlayUpdate overlay </> path
            if updated
              then pure $ overlayUpdate overlay </> path
              else do
                exists <- doesFileExist $ overlayBase overlay </> path
                if exists
                  then pure $ overlayUpdate overlay </> path
                  else pure $ overlayCreate overlay </> path
  IO.createDirectoryIfMissing True $ takeDirectory path'
  IO.writeFile path' content

overlayCreateDir :: HasCallStack => Overlay -> FilePath -> IO ()
overlayCreateDir overlay path = do
  missing <- isNothing <$> overlayResolve overlay path
  when missing $ do
    let path' = overlayCreate overlay </> path
    IO.createDirectoryIfMissing True $ takeDirectory path'
    IO.createDirectory path'

overlayRemoveDir :: HasCallStack => Overlay -> FilePath -> IO ()
overlayRemoveDir overlay path = do
  missing <- isNothing <$> overlayResolve overlay path
  unless missing $ do
    doRemove $ overlayCreate overlay
    doRemove $ overlayUpdate overlay

    inBase <- doesDirectoryExist $ overlayBase overlay </> path
    when inBase $ do
      let path' = overlayDelete overlay </> path
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

overlayRemoveFile ::
  HasCallStack =>
  Overlay ->
  FilePath ->
  IO ()
overlayRemoveFile overlay path = do
  missing <- isNothing <$> overlayResolve overlay path
  unless missing $ do
    doRemove $ overlayCreate overlay
    doRemove $ overlayUpdate overlay

    inBase <- doesFileExist $ overlayBase overlay </> path
    when inBase $ do
      IO.createDirectoryIfMissing True $ overlayDelete overlay </> takeDirectory path
      IO.writeFile (overlayDelete overlay </> path) mempty
  where
    doRemove dir = do
      exists <- doesFileExist $ dir </> path
      when exists . IO.removeFile $ dir </> path

overlayCommit :: Overlay -> IO ()
overlayCommit overlay = do
  deleted <- fromMaybe [] <$> listRecursive (overlayDelete overlay)
  for_ deleted $ \path -> do
    isDir <- doesDirectoryExist $ overlayDelete overlay </> path
    if isDir
      then do
        IO.removeDirectoryRecursive $ overlayBase overlay </> path
        IO.removeDirectory $ overlayDelete overlay </> path
      else do
        IO.removeFile $ overlayBase overlay </> path
        IO.removeFile $ overlayDelete overlay </> path

  clearDirectory $ overlayDelete overlay
  created <- fromMaybe [] <$> listRecursive (overlayCreate overlay)
  for_ created $ \path -> do
    let source = overlayCreate overlay </> path
    let target = overlayBase overlay </> path
    IO.createDirectoryIfMissing True $ takeDirectory target
    isDir <- doesDirectoryExist source
    if isDir
      then IO.createDirectoryIfMissing False target
      else IO.renamePath source target
  clearDirectory $ overlayCreate overlay

  updated <- fromMaybe [] <$> listRecursive (overlayUpdate overlay)
  for_ updated $ \path -> do
    let source = overlayUpdate overlay </> path
    let target = overlayBase overlay </> path
    IO.createDirectoryIfMissing True $ takeDirectory target
    isDir <- doesDirectoryExist source
    if isDir
      then IO.createDirectoryIfMissing False target
      else IO.renamePath source target
  clearDirectory $ overlayUpdate overlay

clearDirectory :: FilePath -> IO ()
clearDirectory path = do
  entries <- IO.listDirectory path
  for_ entries $ \entry -> do
    isDir <- doesDirectoryExist $ path </> entry
    if isDir
      then IO.removeDirectoryRecursive $ path </> entry
      else IO.removeFile $ path </> entry
