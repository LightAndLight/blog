{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Test.Blog.Store.Overlay (spec) where

import Blog.Store.Overlay
  ( Overlay (..)
  , OverlayChanges (..)
  , overlayCommit
  , overlayCreateDir
  , overlayDoesDirectoryExist
  , overlayDoesFileExist
  , overlayListDir
  , overlayReadFile
  , overlayRemoveDir
  , overlayRemoveFile
  , overlayWriteFile
  )
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Kind (Type)
import Data.List (isPrefixOf, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec =
  describe "Blog.Store.Overlay" $ do
    it "state machine test" . hedgehog $ do
      cs <- forAll $ Gen.sequential (Range.constant 0 200) initialState commands
      tmpDir <-
        liftIO $ getCanonicalTemporaryDirectory >>= \tmp -> createTempDirectory tmp "blog-server-tests"
      footnote $ "test data: " ++ tmpDir

      let mkDir path = liftIO $ path <$ createDirectory path
      base <- mkDir $ tmpDir </> "base"
      create <- mkDir $ tmpDir </> "create"
      update <- mkDir $ tmpDir </> "update"
      delete <- mkDir $ tmpDir </> "delete"

      let
        overlay =
          Overlay
            { overlayBase = base
            , overlayChanges =
                Just
                  OverlayChanges
                    { overlayCreate = create
                    , overlayUpdate = update
                    , overlayDelete = delete
                    }
            }
      runReaderT (executeSequential initialState cs) overlay

      liftIO $ removeDirectoryRecursive tmpDir

data State (v :: Type -> Type)
  = State
  { stateBase :: Map String Node
  , stateCreate :: Map String Node
  , stateUpdate :: Map String Node
  , stateDelete :: Set Path
  }
  deriving (Show)

data Node
  = File ByteString
  | Dir (Map String Node)
  deriving (Show, Eq)

type Path = NonEmpty String

initialState :: State v
initialState = State{stateBase = mempty, stateCreate = mempty, stateUpdate = mempty, stateDelete = mempty}

stateOverlayCommit :: State v -> State v
stateOverlayCommit state =
  state
    { stateBase =
        flip
          (foldl' $ \acc (path, node) -> insertPathWith combine path node acc)
          (flatten $ stateCreate state)
          $ flip
            (foldl' $ \acc (path, node) -> insertPathWith combine path node acc)
            (flatten $ stateUpdate state)
          $ flip (foldl' $ \acc path -> deletePath path acc) (stateDelete state)
          $ stateBase state
    , stateCreate = mempty
    , stateUpdate = mempty
    , stateDelete = mempty
    }
  where
    combine new old =
      case (new, old) of
        (Dir m, Dir{})
          | Map.null m -> old
          | otherwise -> clash
        (Dir{}, File{}) -> clash
        (File{}, File{}) -> new
        (File{}, Dir{}) -> clash
      where
        clash = error $ "combine: new = " ++ show new ++ ", old = " ++ show old ++ "\nstate = " ++ show state

    flatten :: Map String Node -> [(Path, Node)]
    flatten m =
      [ item
      | (name, node) <- Map.toList m
      , item <-
          case node of
            File{} -> [(pure name, node)]
            Dir m' ->
              if Map.null m'
                then [(pure name, node)]
                else first (NonEmpty.cons name) <$> flatten m'
      ]

stateFilePaths :: State v -> [Path]
stateFilePaths = filePaths . stateBase . stateOverlayCommit

filePaths :: Map String Node -> [Path]
filePaths m =
  [ path
  | (name, node) <- Map.toList m
  , path <-
      case node of
        File{} ->
          [pure name]
        Dir m' ->
          fmap (NonEmpty.cons name) (filePaths m')
  ]

stateDeletedFilePaths :: State v -> [Path]
stateDeletedFilePaths state =
  filter
    ( \basePath ->
        any
          ( \deletedPath ->
              NonEmpty.toList deletedPath `NonEmpty.isPrefixOf` basePath
          )
          (stateDelete state)
    )
    (filePaths (stateBase state))

stateDirPaths :: State v -> [Path]
stateDirPaths = dirPaths . stateBase . stateOverlayCommit

dirPaths :: Map String Node -> [Path]
dirPaths m =
  [ path
  | (name, node) <- Map.toList m
  , path <-
      case node of
        File{} -> []
        Dir m' ->
          pure name : fmap (NonEmpty.cons name) (dirPaths m')
  ]

stateOverlayRead :: Path -> State v -> Maybe Node
stateOverlayRead path = lookupPath path . stateBase . stateOverlayCommit

stateOverlayWriteFile :: Path -> ByteString -> State v -> State v
stateOverlayWriteFile path content state =
  case lookupPath path (stateCreate state) of
    Just File{} -> state{stateCreate = insertPath path (File content) (stateCreate state)}
    Just Dir{} -> undefined
    Nothing ->
      case lookupPath path (stateUpdate state) of
        Just File{} -> state{stateUpdate = insertPath path (File content) (stateUpdate state)}
        Just Dir{} -> undefined
        Nothing ->
          if Set.member path (stateDelete state)
            then state{stateCreate = insertPath path (File content) (stateCreate state)}
            else case lookupPath path (stateBase state) of
              Just{} -> state{stateUpdate = insertPath path (File content) (stateUpdate state)}
              Nothing -> state{stateCreate = insertPath path (File content) (stateCreate state)}

stateOverlayCreateDir :: Path -> State v -> State v
stateOverlayCreateDir path state =
  case lookupPath path (stateCreate state) of
    Just File{} -> undefined
    Just Dir{} -> state
    Nothing ->
      case lookupPath path (stateUpdate state) of
        Just File{} -> undefined
        Just Dir{} -> state
        Nothing ->
          if Set.member path (stateDelete state)
            then state{stateCreate = insertPath path (Dir mempty) (stateCreate state)}
            else case lookupPath path (stateBase state) of
              Just File{} -> undefined
              Just Dir{} -> state
              Nothing -> state{stateCreate = insertPath path (Dir mempty) (stateCreate state)}

lookupPath :: Path -> Map String Node -> Maybe Node
lookupPath (p :| []) m = Map.lookup p m
lookupPath (p :| (p' : ps')) m = do
  node <- Map.lookup p m
  case node of
    File{} -> Nothing
    Dir m' -> lookupPath (p' :| ps') m'

deletePath :: Path -> Map String Node -> Map String Node
deletePath (p :| []) m = Map.delete p m
deletePath (p :| (p' : ps')) m = Map.adjust f p m
  where
    -- Nothing to remove beneath a file.
    f node@File{} = node
    f (Dir m') = Dir $ deletePath (p' :| ps') m'

insertPathWith ::
  HasCallStack =>
  {-| Arguments:

  * New value
  * Old value
  -}
  (Node -> Node -> Node) ->
  Path ->
  Node ->
  Map String Node ->
  Map String Node
insertPathWith combine path node tree = go path tree
  where
    go :: Path -> Map String Node -> Map String Node
    go (p :| []) m = Map.insertWith combine p node m
    go (p :| (p' : ps')) m = Map.alter f p m
      where
        f Nothing = Just . Dir $ go (p' :| ps') Map.empty
        f (Just File{}) =
          error $
            "some prefix of " ++ foldr1 (</>) path ++ " is a file:\n" ++ unlines (show <$> Map.toList tree)
        f (Just (Dir m')) = Just . Dir $ go (p' :| ps') m'

insertPath :: HasCallStack => Path -> Node -> Map String Node -> Map String Node
insertPath = insertPathWith const

stateOverlayRemoveFile :: Path -> State v -> State v
stateOverlayRemoveFile path state =
  case lookupPath path (stateCreate state) of
    Just Dir{} -> state
    Just File{} -> state{stateCreate = deletePath path (stateCreate state)}
    Nothing ->
      case lookupPath path (stateUpdate state) of
        Just Dir{} -> state
        Just File{} ->
          state
            { stateUpdate = deletePath path (stateUpdate state)
            , stateDelete = Set.insert path (stateDelete state)
            }
        Nothing ->
          case lookupPath path (stateBase state) of
            Just Dir{} -> state
            Nothing -> state
            Just File{} -> state{stateDelete = Set.insert path (stateDelete state)}

stateOverlayRemoveDir :: Path -> State v -> State v
stateOverlayRemoveDir path state =
  state
    { stateCreate = deletePath path (stateCreate state)
    , stateUpdate = deletePath path (stateUpdate state)
    , stateDelete =
        case lookupPath path (stateBase state) of
          Just Dir{} -> Set.insert path (stateDelete state)
          _ -> stateDelete state
    }

stateOverlayDoesDirectoryExist :: Path -> State v -> Bool
stateOverlayDoesDirectoryExist path state =
  case lookupPath path . stateBase $ stateOverlayCommit state of
    Nothing -> False
    Just File{} -> False
    Just Dir{} -> True

stateOverlayListDir :: Maybe Path -> State v -> [Path]
stateOverlayListDir mPath state =
  stateOverlayListBase mPath $ stateOverlayCommit state

stateOverlayListBase :: Maybe Path -> State v -> [Path]
stateOverlayListBase mPath old =
  fmap pure . Map.keys $
    case mPath of
      Nothing -> stateBase old
      Just path ->
        case lookupPath path $ stateBase old of
          Nothing -> undefined
          Just File{} -> undefined
          Just (Dir m) -> m

commands :: (MonadGen gen, MonadReader Overlay m, MonadIO m) => [Command gen m State]
commands =
  [ cWriteFile
  , cDoesFileExist
  , cReadFileExists
  , cReadFileDeleted
  , cRemoveFileExists
  , cRemoveFileDeleted
  , cCreateDir
  , cListDir
  , cDoesDirectoryExist
  , cRemoveDir
  , cCommit
  ]

genContent :: MonadGen m => m ByteString
genContent = Gen.bytes (Range.linear 0 32)

genName :: MonadGen m => m String
genName = Gen.string (Range.linear 1 8) Gen.alphaNum

genPath :: MonadGen m => m Path
genPath = Gen.nonEmpty (Range.constant 1 5) genName

data ReadFile (v :: Type -> Type) = ReadFile Path
  deriving (Show, Generic, FunctorB, TraversableB)

cReadFileExists :: (MonadGen gen, MonadReader Overlay m, MonadIO m) => Command gen m State
cReadFileExists =
  Command
    ( \state -> do
        let paths = stateFilePaths state
        guard . not $ null paths
        pure $ ReadFile <$> Gen.element paths
    )
    ( \(ReadFile path) -> do
        overlay <- ask
        liftIO $ overlayReadFile overlay (foldr1 (</>) path)
    )
    [ Require $ \state (ReadFile path) ->
        case stateOverlayRead path state of
          Nothing -> False
          Just File{} -> True
          Just Dir{} -> False
    , Ensure $ \old _new (ReadFile path) mOutput -> do
        label $ fromString "read file"
        annotateShow old
        node <- evalMaybe $ stateOverlayRead path old
        output <- evalMaybe mOutput
        node === File output
    ]

cReadFileDeleted :: (MonadGen gen, MonadReader Overlay m, MonadIO m) => Command gen m State
cReadFileDeleted =
  Command
    ( \state -> do
        let paths = stateDeletedFilePaths state
        guard . not $ null paths
        pure $ ReadFile <$> Gen.element paths
    )
    ( \(ReadFile path) -> do
        overlay <- ask
        liftIO $ overlayReadFile overlay (foldr1 (</>) path)
    )
    [ Require $ \state (ReadFile path) ->
        isNothing $ stateOverlayRead path state
    , Ensure $ \old _new (ReadFile path) mOutput -> do
        label $ fromString "read file (deleted)"
        let node = stateOverlayRead path old
        case node of
          Nothing -> mOutput === Nothing
          Just{} -> failure
    ]

data RemoveFile (v :: Type -> Type) = RemoveFile Path
  deriving (Show, Generic, FunctorB, TraversableB)

cRemoveFileExists :: (MonadGen gen, MonadReader Overlay m, MonadIO m) => Command gen m State
cRemoveFileExists =
  Command
    ( \state -> do
        let paths = stateFilePaths state
        guard . not $ null paths
        pure $ RemoveFile <$> Gen.element paths
    )
    ( \(RemoveFile path) -> do
        overlay <- ask
        liftIO $ overlayRemoveFile overlay (foldr1 (</>) path)
    )
    [ Require $ \state (RemoveFile path) ->
        case stateOverlayRead path state of
          Nothing -> False
          Just File{} -> True
          Just Dir{} -> False
    , Update $ \state (RemoveFile path) _output ->
        stateOverlayRemoveFile path state
    , Ensure $ \_old _new (RemoveFile _path) _output -> do
        label $ fromString "remove file"
    ]

cRemoveFileDeleted :: (MonadGen gen, MonadReader Overlay m, MonadIO m) => Command gen m State
cRemoveFileDeleted =
  Command
    ( \state -> do
        let paths = stateDeletedFilePaths state
        guard . not $ null paths
        pure $ RemoveFile <$> Gen.element paths
    )
    ( \(RemoveFile path) -> do
        overlay <- ask
        liftIO $ overlayRemoveFile overlay (foldr1 (</>) path)
    )
    [ Require $ \state (RemoveFile path) ->
        isNothing $ stateOverlayRead path state
    , Update $ \state (RemoveFile path) _output ->
        stateOverlayRemoveFile path state
    , Ensure $ \_old _new (RemoveFile _path) _output -> do
        label $ fromString "remove file (deleted)"
    ]

data WriteFile (v :: Type -> Type) = WriteFile [String] String ByteString
  deriving (Show, Generic, FunctorB, TraversableB)

cWriteFile :: (MonadGen gen, MonadReader Overlay m, MonadIO m) => Command gen m State
cWriteFile =
  Command
    ( \state -> do
        let
          paths = stateDirPaths state
        let
          toplevel = do
            let names = [name | (name, File{}) <- Map.toList . stateBase $ stateOverlayCommit state]
            name <- Gen.choice $ genName : [Gen.element names | not $ null names]
            content <- genContent
            pure $ WriteFile [] name content
        if null paths
          then
            Just toplevel
          else Just $ do
            mPath <- Gen.maybe $ Gen.element paths
            case mPath of
              Nothing -> toplevel
              Just path -> do
                let names =
                      [name | Just (Dir entries) <- [stateOverlayRead path state], (name, File{}) <- Map.toList entries]
                name <- Gen.choice $ genName : [Gen.element names | not $ null names]

                content <- genContent
                pure $ WriteFile (NonEmpty.toList path) name content
    )
    ( \(WriteFile dir name content) -> do
        overlay <- ask
        liftIO $ overlayWriteFile overlay (foldr (</>) name dir) (LazyByteString.fromStrict content)
    )
    [ Require $ \state (WriteFile dir name _content) ->
        let path = foldr NonEmpty.cons (pure name) dir
        in ( case NonEmpty.nonEmpty dir of
               Nothing -> True
               Just dir' ->
                 case stateOverlayRead dir' state of
                   Just Dir{} -> True
                   Just File{} -> False
                   Nothing -> False
           )
             && ( case stateOverlayRead path state of
                    Nothing -> True
                    Just File{} -> True
                    Just Dir{} -> False
                )
             &&
             -- No existing files overlap with `path`'s directory part
             not
               ( any
                   (\existingPath -> NonEmpty.toList existingPath `isPrefixOf` NonEmpty.init path)
                   (stateFilePaths state)
               )
             &&
             -- `path` does not overlap with any existing file's directory part
             not
               ( any
                   (\existingPath -> NonEmpty.toList path `isPrefixOf` NonEmpty.init existingPath)
                   (stateFilePaths state)
               )
    , Update $ \state (WriteFile dir name content) _output ->
        stateOverlayWriteFile (foldr NonEmpty.cons (pure name) dir) content state
    , Ensure $ \old _new (WriteFile dir name _content) _output -> do
        let path = foldr NonEmpty.cons (pure name) dir
        label . fromString $
          "write file "
            ++ case stateOverlayRead path old of
              Nothing -> "(create)"
              Just{} -> "(update)"
    ]

data DoesFileExist (v :: Type -> Type) = DoesFileExist Path
  deriving (Show, Generic, FunctorB, TraversableB)

cDoesFileExist :: (MonadGen gen, MonadReader Overlay m, MonadIO m) => Command gen m State
cDoesFileExist =
  Command
    ( \state -> do
        let paths = stateFilePaths state
        pure $
          DoesFileExist
            <$> Gen.choice (genPath : [Gen.element paths | not $ null paths])
    )
    ( \(DoesFileExist path) -> do
        overlay <- ask
        liftIO $ overlayDoesFileExist overlay (foldr1 (</>) path)
    )
    [ Ensure $ \old _new (DoesFileExist path) _output -> do
        label . fromString $
          "does file exist "
            ++ case stateOverlayRead path old of
              Nothing -> "(path missing)"
              Just{} -> "(path exists)"
    ]

data CreateDir (v :: Type -> Type) = CreateDir [String] String
  deriving (Show, Generic, FunctorB, TraversableB)

cCreateDir :: (MonadGen gen, MonadReader Overlay m, MonadIO m) => Command gen m State
cCreateDir =
  Command
    ( \state -> do
        let paths = stateDirPaths state
        let
          toplevel = do
            name <- genName
            pure $ CreateDir [] name
        if null paths
          then
            Just toplevel
          else Just $ do
            mPath <- Gen.maybe $ Gen.element paths
            case mPath of
              Nothing -> toplevel
              Just path -> do
                name <- genName
                pure $ CreateDir (NonEmpty.toList path) name
    )
    ( \(CreateDir dir name) -> do
        overlay <- ask
        liftIO $ overlayCreateDir overlay (foldr (</>) name dir)
    )
    [ Require $ \state (CreateDir dir name) ->
        let path = foldr NonEmpty.cons (pure name) dir
        in ( case NonEmpty.nonEmpty dir of
               Nothing -> True
               Just dir' ->
                 case stateOverlayRead dir' state of
                   Just Dir{} -> True
                   Just File{} -> False
                   Nothing -> False
           )
             && ( case stateOverlayRead path state of
                    Nothing -> True
                    Just Dir{} -> True
                    Just File{} -> False
                )
             &&
             -- No existing files overlap with `path`'s directory part
             not
               ( any
                   (\existingPath -> NonEmpty.toList existingPath `isPrefixOf` NonEmpty.init path)
                   (stateFilePaths state)
               )
             &&
             -- `path` does not overlap with any existing file's directory part
             not
               ( any
                   (\existingPath -> NonEmpty.toList path `isPrefixOf` NonEmpty.init existingPath)
                   (stateFilePaths state)
               )
    , Update $ \state (CreateDir dir name) _output ->
        stateOverlayCreateDir (foldr NonEmpty.cons (pure name) dir) state
    , Ensure $ \_old _new (CreateDir _dir _name) _output -> do
        label $ fromString "create directory"
    ]

data RemoveDir (v :: Type -> Type) = RemoveDir Path
  deriving (Show, Generic, FunctorB, TraversableB)

cRemoveDir :: (MonadGen gen, MonadReader Overlay m, MonadIO m) => Command gen m State
cRemoveDir =
  Command
    ( \state -> do
        let paths = stateDirPaths state
        guard . not $ null paths
        pure $ RemoveDir <$> Gen.element paths
    )
    ( \(RemoveDir path) -> do
        overlay <- ask
        liftIO $ overlayRemoveDir overlay (foldr1 (</>) path)
    )
    [ Require $ \state (RemoveDir path) ->
        case stateOverlayRead path state of
          Nothing -> False
          Just File{} -> False
          Just Dir{} -> True
    , Update $ \state (RemoveDir path) _output ->
        stateOverlayRemoveDir path state
    , Ensure $ \_old _new (RemoveDir _path) _output -> do
        label $ fromString "remove directory"
    ]

data ListDir (v :: Type -> Type) = ListDir (Maybe Path)
  deriving (Show, Generic, FunctorB, TraversableB)

cListDir :: (MonadGen gen, MonadReader Overlay m, MonadIO m) => Command gen m State
cListDir =
  Command
    ( \state -> do
        let paths = stateDirPaths state
        if null paths
          then pure $ pure (ListDir Nothing)
          else pure $ ListDir <$> Gen.maybe (Gen.element paths)
    )
    ( \(ListDir mPath) -> do
        overlay <- ask
        liftIO $ overlayListDir overlay (foldr1 (</>) <$> mPath)
    )
    [ Require $ \state (ListDir mPath) ->
        case mPath of
          Nothing -> True
          Just path ->
            case stateOverlayRead path state of
              Nothing -> False
              Just File{} -> False
              Just Dir{} -> True
    , Ensure $ \old _new (ListDir mPath) output -> do
        label . fromString $ "list directory" ++ maybe " (path = .)" (const $ " (path = not .)") mPath
        let expected = foldr1 (</>) <$> stateOverlayListDir mPath old
        sort expected === sort output
    ]

data DoesDirectoryExist (v :: Type -> Type) = DoesDirectoryExist Path
  deriving (Show, Generic, FunctorB, TraversableB)

cDoesDirectoryExist :: (MonadGen gen, MonadReader Overlay m, MonadIO m) => Command gen m State
cDoesDirectoryExist =
  Command
    ( \state -> do
        let paths = stateDirPaths state
        pure $ DoesDirectoryExist <$> Gen.choice (genPath : [Gen.element paths | not $ null paths])
    )
    ( \(DoesDirectoryExist path) -> do
        overlay <- ask
        liftIO $ overlayDoesDirectoryExist overlay (foldr1 (</>) path)
    )
    [ Ensure $ \old _new (DoesDirectoryExist path) output -> do
        label . fromString $
          "does directory exist"
            ++ maybe
              " (path missing)"
              (const $ " (path present)")
              (lookupPath path $ stateBase $ stateOverlayCommit old)
        stateOverlayDoesDirectoryExist path old === output
    ]

data Commit (v :: Type -> Type) = Commit
  deriving (Show, Generic, FunctorB, TraversableB)

cCommit :: (MonadGen gen, MonadReader Overlay m, MonadIO m) => Command gen m State
cCommit =
  Command
    (\_state -> Just $ pure Commit)
    ( \Commit -> do
        overlay <- ask
        liftIO $ overlayCommit overlay
    )
    [ Update $ \state Commit _output ->
        stateOverlayCommit state
    , Ensure $ \_old _new Commit _output -> do
        label $ fromString "commit"
    ]
