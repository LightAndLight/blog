{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Blog.Build
  ( Rules
  , Change (..)
  , renderChange
  , Status (..)
  , Reason (..)
  , evalRules
  , rule
  , Input
  , ResourceInput (..)
  , ResourceInputs (..)
  , iResource
  , iResourceAll
  , ResourceNamePattern
  , iMatch
  , iBind
  , iAny
  , Output
  , ResourceOutput
  , writeResource
  , oResource
  , OutputResourceNamePattern
  , (>*<)
  , (*<)
  , oMatch
  , oBind
  , oAny
  , oResourceType
  , Action
  , askDataDir
  , setDependencies
  , trace

    -- * Internals
  , resourcePatternsOverlap
  )
where

import Blog
  ( MetadataValue
  , ResourceId (ResourceId)
  , readResourceId
  , renderResourceId
  )
import Blog.Diagnostic (DiagnosticReports (..))
import Blog.Metadata
  ( lookupResourceMetadata
  , parseResourceMetadata
  )
import Blog.Resource
  ( createResource
  , doesResourceExist
  , getResourceType
  , listResource
  , lookupResource
  , updateResource
  )
import Control.Exception (catch, throwIO)
import Control.Monad (guard, unless)
import Control.Monad.Error.Class (MonadError, liftEither, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks, local)
import Control.Monad.State.Class (MonadState, get)
import Control.Monad.State.Strict (evalStateT, modify)
import Control.Monad.Writer.CPS (WriterT, execWriterT, runWriterT)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.ByteString.Lazy (LazyByteString)
import Data.Foldable (foldlM, for_, traverse_)
import Data.Graph (graphFromEdges, topSort)
import Data.Kind (Type)
import Data.List (intercalate, nub, partition, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid (Any (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Traversable (for)
import qualified IO
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Prelude hiding (any)

newtype Rules = Rules [Rule]
  deriving (Semigroup, Monoid)

data Rule = forall a b. Rule !String (Input a) (Output b) (a -> b -> Action ())

matchRule ::
  Rule ->
  -- | Changes
  Set ResourceId ->
  Maybe (Action ())
matchRule (Rule name inputs outputs f) changes = do
  let (Any matched, action) = matchInput inputs changes
  guard matched
  pure $ do
    trace $ "begin " ++ name
    inputs' <- action
    traverse_
      ( \(age, reasons, tuple, bindings, input') -> do
          trace $ show (age, reasons, tuple, bindings)
          let outputs' = makeOutput bindings outputs
          withReasons reasons $ f input' outputs'
      )
      inputs'
    trace $ "end " ++ name

withReasons :: [Reason] -> Action a -> Action a
withReasons rs (Action ma) = Action $ local (\env -> env{aeReasons = rs}) ma

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
  (a, ActionSummary pending changes) <-
    liftEither =<< liftIO (runExceptT . runWriterT . flip runReaderT env $ ma)
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
    let b = Blog.readResourceId dependency

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
  IFmap :: (a -> b) -> Input a -> Input b
  IPure :: a -> Input a
  IApply :: Input (a -> b) -> Input a -> Input b
  IResource :: InputQuantifier a -> String -> ResourceNamePattern -> Input a

instance Functor Input where
  fmap = IFmap

instance Applicative Input where
  pure = IPure
  (<*>) = IApply

data InputQuantifier a where
  IAny :: InputQuantifier ResourceInput
  IAll :: InputQuantifier ResourceInputs

data ResourceInput
  = ResourceInput
  { resourceInputId :: !ResourceId
  , resourceInputPath :: !FilePath
  , resourceInputMetadata :: !(Map Text MetadataValue)
  , resourceInputContent :: LazyByteString
  }

data ResourceInputs
  = ResourceInputs
  { resourceInputsType :: !String
  , resourceInputs :: ![ResourceInput]
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

merge ::
  (Age, [Reason], [ResourceId], Map String String, a -> b) ->
  (Age, [Reason], [ResourceId], Map String String, a) ->
  Maybe (Age, [Reason], [ResourceId], Map String String, b)
merge (age1, reasons1, tuples1, bindings1, f) (age2, reasons2, tuples2, bindings2, a) = do
  let
    common :: Map String (Maybe String)
    common = Map.intersectionWith (\x y -> x <$ guard (x == y)) bindings1 bindings2

    left = Map.difference bindings1 common
    right = Map.difference bindings2 common

  common' <- sequence common
  pure (age1 <> age2, reasons1 <> reasons2, tuples1 <> tuples2, left <> right <> common', f a)

matchInput ::
  Input a ->
  Set ResourceId ->
  (Any, Action [(Age, [Reason], [ResourceId], Map String String, a)])
matchInput i = go i
  where
    go ::
      MonadWriter Any m =>
      Input a ->
      Set ResourceId ->
      m (Action [(Age, [Reason], [ResourceId], Map String String, a)])
    go (IFmap f deps) changes =
      (fmap . fmap . fmap . fmap) f (go deps changes)
    go (IPure a) _changes =
      pure $ pure [(Old, [], [], mempty, a)]
    go (IApply deps deps') changes =
      (liftA2 . liftA2)
        ( \lAll rAll ->
            let
              (lOld, lNew) = partition (\(x, _, _, _, _) -> x == New) lAll
              rNew = filter (\(x, _, _, _, _) -> x == New) rAll
            in
              [z | x <- lNew, y <- rAll, Just z <- [merge x y]]
                ++ [z | x <- lOld, y <- rNew, Just z <- [merge x y]]
        )
        (go deps changes)
        (go deps' changes)
    go (IResource quant resTyName resNamePat) changes = do
      changes' <-
        fmap catMaybes
          . for (Set.toAscList changes)
          $ \resId'@(ResourceId resTyName' resName') -> do
            let
              matchSuccess bindings resId = do
                tell $ Any True
                pure . Just $ (,,,,) New [Reason Updated resId] [resId] bindings <$> makeResource resId

              matchFailure = pure Nothing

            if resTyName == resTyName'
              then case matchResourceName resNamePat resName' of
                Nothing -> matchFailure
                Just bindings -> matchSuccess bindings resId'
              else matchFailure
      pure $ do
        (resTyDir, resTy) <- do
          dataDir <- askDataDir
          mResTy <- getResourceType dataDir $ fromString resTyName
          maybe (error $ resTyName ++ " does not exist") pure mResTy

        news <- sequence changes'
        olds <- do
          listed <- listResource resTyDir resTy
          let
            olds =
              mapMaybe
                ( \old@(ResourceId _resTyName resName) -> do
                    guard $ old `Set.notMember` changes
                    bindings <- matchResourceName resNamePat resName
                    pure (bindings, old)
                )
                listed
          pure olds
        case quant of
          IAny -> do
            olds' <-
              for olds $ \(bindings, old) -> do
                old' <- makeResource old
                pure (Old, [], [old], bindings, old')
            pure $ news ++ olds'
          IAll -> do
            let reasons = nub [reason' | (_, reasons', _, _, _) <- news, reason' <- reasons']
            olds' <-
              for olds $ \(bindings, old) -> do
                old' <- makeResource old
                pure (Old, [], [old], bindings, old')
            pure
              [
                ( New
                , reasons
                , [ResourceId resTyName "*"]
                , mempty
                , ResourceInputs resTyName . fmap (\(_, _, _, _, x) -> x) $ news ++ olds'
                )
              ]

makeResource :: ResourceId -> Action ResourceInput
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
        ResourceInput
          { resourceInputId = resId
          , resourceInputPath = resPath
          , resourceInputMetadata = metadata
          , resourceInputContent = content
          }

newtype ResourceNamePattern
  = ResourceNamePattern [ResourceNamePatternPart]
  deriving (Show, Eq, Semigroup)

data ResourceNamePatternPart
  = PAny
  | PExact !String
  | PBind !String
  deriving (Show, Eq)

iAny :: ResourceNamePattern
iAny = ResourceNamePattern $ pure PAny

iMatch :: String -> ResourceNamePattern
iMatch = ResourceNamePattern . pure . PExact

iBind :: String -> ResourceNamePattern
iBind = ResourceNamePattern . pure . PBind

matchResourceName :: ResourceNamePattern -> String -> Maybe (Map String String)
matchResourceName (ResourceNamePattern ps) resName =
  fst
    <$> foldlM
      ( \(bindings, remaining) part ->
          case part of
            PAny ->
              pure (bindings, "")
            PExact namePart -> do
              remaining' <- stripPrefix namePart remaining
              pure (bindings, remaining')
            -- Greedy matching for variables.
            --
            -- TODO: match only up to the next `PExact`
            PBind var ->
              case Map.lookup var bindings of
                Nothing -> do
                  pure (Map.insert var remaining bindings, "")
                Just binding -> do
                  guard $ remaining == binding
                  pure (bindings, "")
      )
      (mempty, resName)
      ps

resourcePatternsOverlap ::
  ResourceNamePattern ->
  ResourceNamePattern ->
  Bool
resourcePatternsOverlap (ResourceNamePattern ps) (ResourceNamePattern ps') =
  go ps ps'
  where
    go [] [] = True
    go (part : parts) [] =
      case part of
        PAny -> go parts []
        PExact namePart -> null namePart && go parts []
        PBind _var -> go parts []
    go [] (part : parts) =
      case part of
        PAny -> go [] parts
        PExact namePart -> null namePart && go parts []
        PBind _var -> go parts []
    -- Variables are greedily matched.
    --
    -- TODO: match only up to the next `PExact`
    go (part : parts) (otherPart : otherParts) =
      case part of
        PAny ->
          go parts []
        PExact namePart ->
          case otherPart of
            PAny -> go [] otherParts
            PExact namePart' -> namePart == namePart' && go parts otherParts
            PBind _var -> go [] otherParts
        PBind _var ->
          go parts []

-- | Declare an input of a particular resource type, matching the given pattern.
--
-- Every matching input in the change set triggers a rule invocation.
iResource ::
  -- | Resource type name
  String ->
  -- | Resource name
  ResourceNamePattern ->
  Input ResourceInput
iResource resTyName = IResource IAny resTyName

-- | Declare a bulk input of a particular resource type, matching the given pattern.
--
-- Every matching input in the change set is collected, and passed to a single rule invocation as a single input.
iResourceAll ::
  -- | Resource type
  String ->
  -- | Resource name
  ResourceNamePattern ->
  Input ResourceInputs
iResourceAll = IResource IAll

data Output :: Type -> Type where
  OFmap :: (a -> b) -> Output a -> Output b
  OPure :: a -> Output a
  OApply :: Output (a -> b) -> Output a -> Output b
  OResourceType :: String -> Output (ResourceOutput String)
  OResource :: String -> OutputResourceNamePattern a -> Output (ResourceOutput a)

instance Functor Output where
  fmap = OFmap

instance Applicative Output where
  pure = OPure
  (<*>) = OApply

newtype ResourceOutput a
  = ResourceOutput
  { writeResource :: a -> LazyByteString -> Action ()
  }

data OutputResourceNamePattern a where
  OPContramap :: (b -> a) -> OutputResourceNamePattern a -> OutputResourceNamePattern b
  OPDivide ::
    (a -> (b, c)) ->
    OutputResourceNamePattern b ->
    OutputResourceNamePattern c ->
    OutputResourceNamePattern a
  OPAny :: OutputResourceNamePattern String
  OPExact :: String -> OutputResourceNamePattern ()
  OPBind :: String -> OutputResourceNamePattern ()

(>*<) ::
  OutputResourceNamePattern a -> OutputResourceNamePattern b -> OutputResourceNamePattern (a, b)
(>*<) = OPDivide id

infixl 4 >*<

(*<) :: OutputResourceNamePattern () -> OutputResourceNamePattern a -> OutputResourceNamePattern a
(*<) = OPDivide ((,) ())

infixl 4 *<

oAny :: OutputResourceNamePattern String
oAny = OPAny

oMatch :: String -> OutputResourceNamePattern ()
oMatch = OPExact

oBind :: String -> OutputResourceNamePattern ()
oBind = OPBind

outputResourceNamePatternToResourceNamePattern :: OutputResourceNamePattern a -> ResourceNamePattern
outputResourceNamePatternToResourceNamePattern (OPContramap _f a) = outputResourceNamePatternToResourceNamePattern a
outputResourceNamePatternToResourceNamePattern (OPDivide _f a b) =
  outputResourceNamePatternToResourceNamePattern a <> outputResourceNamePatternToResourceNamePattern b
outputResourceNamePatternToResourceNamePattern OPAny = iAny
outputResourceNamePatternToResourceNamePattern (OPExact value) = iMatch value
outputResourceNamePatternToResourceNamePattern (OPBind var) = iBind var

renderOutputResourceNamePattern :: Map String String -> OutputResourceNamePattern a -> a -> String
renderOutputResourceNamePattern bindings p x = go p x
  where
    go :: OutputResourceNamePattern a -> a -> String
    go (OPContramap f pat) a = go pat $ f a
    go (OPDivide f pat1 pat2) a =
      let (b, c) = f a
      in go pat1 b ++ go pat2 c
    go OPAny a = a
    go (OPExact value) () = value
    go (OPBind var) () = do
      case Map.lookup var bindings of
        Nothing -> error $ "unbound variable " ++ show var
        Just binding -> binding

oResourceType :: String -> Output (ResourceOutput String)
oResourceType = OResourceType

oResource ::
  -- | Resource type name
  String ->
  -- | Resource name pattern
  OutputResourceNamePattern a ->
  Output (ResourceOutput a)
oResource resTyName = OResource resTyName

makeOutput :: Map String String -> Output a -> a
makeOutput bindings = go
  where
    go :: Output a -> a
    go (OFmap f a) = f (go a)
    go (OPure a) = a
    go (OApply a b) = go a (go b)
    go (OResourceType resTyName) =
      ResourceOutput $
        putResource . ResourceId resTyName
    go (OResource resTyName pat) =
      ResourceOutput $
        \resName -> putResource $ ResourceId resTyName (renderOutputResourceNamePattern bindings pat resName)

data Change
  = Change
      -- | What happened
      !Status
      -- | Target resource
      !ResourceId
      -- | Why the change occurred
      ![Reason]
  deriving (Show)

data Status
  = Created
  | Updated
  deriving (Show, Eq)

data Reason = Reason !Status !ResourceId
  deriving (Show, Eq)

renderChange :: Change -> String
renderChange (Change status resId reasons) =
  renderStatus status
    ++ " "
    ++ renderResourceId resId
    ++ if null reasons
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

inputResourceIdPatterns :: Input a -> [ResourceIdPattern]
inputResourceIdPatterns (IFmap _f a) = inputResourceIdPatterns a
inputResourceIdPatterns (IPure _a) = []
inputResourceIdPatterns (IApply a b) = inputResourceIdPatterns a ++ inputResourceIdPatterns b
inputResourceIdPatterns (IResource _quant resTyName resNamePat) = [ResourceIdPattern resTyName resNamePat]

outputResourceIdPatterns :: Output a -> [ResourceIdPattern]
outputResourceIdPatterns (OFmap _f a) = outputResourceIdPatterns a
outputResourceIdPatterns (OPure _a) = []
outputResourceIdPatterns (OApply a b) = outputResourceIdPatterns a ++ outputResourceIdPatterns b
outputResourceIdPatterns (OResourceType resTyName) = [ResourceIdPattern resTyName iAny]
outputResourceIdPatterns (OResource resTyName pat) = [ResourceIdPattern resTyName $ outputResourceNamePatternToResourceNamePattern pat]

resourceIdPatternMatches :: ResourceIdPattern -> ResourceIdPattern -> Bool
resourceIdPatternMatches (ResourceIdPattern resTyName resNamePattern) (ResourceIdPattern resTyName' resNamePattern') =
  resTyName == resTyName'
    && resourcePatternsOverlap resNamePattern resNamePattern'

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

    go ::
      (MonadState (Set ResourceId) m, MonadWriter [Change] m, MonadError DiagnosticReports m, MonadIO m) =>
      m ()
    go = for_ vertices $ \vertex -> do
      let (r, _, _) = fromVertex vertex
      changedResources <- get
      for_ (matchRule r changedResources) $ \action -> do
        (changedResources', changes, ()) <- runAction fTrace dataDir [] action
        dependents <- liftIO $ concat <$> traverse (getDependents dataDir) changedResources'
        modify $ (Set.fromList changedResources' <>) . (Set.fromList dependents <>)
        tell changes
