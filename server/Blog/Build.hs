{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , ActionT
  , askStore
  , askTransactionId
  , setDependencies
  , trace

    -- * Internals
  , resourcePatternsOverlap
  )
where

import Blog
  ( MetadataValue
  , ResourceId (ResourceId)
  , renderResourceId
  , resourceName
  , resourceType
  )
import Blog.Diagnostic (DiagnosticReports (..))
import Blog.Metadata
  ( parseResourceMetadata
  )
import Blog.Store (Store, TransactionId, hoistStore)
import qualified Blog.Store as Store
import Control.Monad (guard, unless)
import Control.Monad.Error.Class (MonadError, liftEither, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks, local)
import Control.Monad.State.Class (get)
import Control.Monad.State.Strict (StateT, evalStateT, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer.CPS (WriterT, execWriterT, runWriterT)
import Control.Monad.Writer.Class (tell)
import Data.ByteString.Lazy (LazyByteString)
import Data.Foldable (foldlM, for_, traverse_)
import Data.Graph (graphFromEdges, topSort)
import Data.Kind (Type)
import Data.List (intercalate, nub, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import Prelude hiding (any)

newtype Rules m = Rules [Rule m]
  deriving (Semigroup, Monoid)

data Rule m = forall a b. Rule !String (Input m a) (Output m b) (a -> b -> ActionT m ())

runRule ::
  MonadIO m =>
  Rule m ->
  -- | Changes
  Set ResourceId ->
  ActionT m ()
runRule (Rule name inputs outputs f) changes = do
  InputTuples _olds news <- queryInputs inputs changes
  unless (null news) $ do
    trace $ "begin " ++ name
    traverse_
      ( \tuple -> do
          let age = inputTupleAge tuple
          let reasons = inputTupleReasons tuple
          let headers = inputTupleHeaders tuple
          let bindings = inputTupleBindings tuple
          let input' = inputTupleValue tuple

          trace $ show (age, reasons, headers, bindings)
          let outputs' = makeOutput bindings outputs
          withReasons reasons $ f input' outputs'
      )
      news
    trace $ "end " ++ name

withReasons :: Monad m => [Reason] -> ActionT m a -> ActionT m a
withReasons rs (ActionT ma) = ActionT $ local (\env -> env{aeReasons = rs}) ma

newtype ActionT m a
  = ActionT (ReaderT (ActionEnv m) (WriterT ActionSummary (ExceptT DiagnosticReports m)) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError DiagnosticReports)

instance MonadTrans ActionT where
  lift = ActionT . lift . lift . lift

data ActionEnv m
  = ActionEnv
  { aeTrace :: !(String -> IO ())
  , aeStore :: !(Store (ActionT m))
  , aeTransactionId :: !TransactionId
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

runActionT ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Trace
  (String -> IO ()) ->
  Store m ->
  TransactionId ->
  -- | Why the action was triggered
  [Reason] ->
  ActionT m a ->
  m ([ResourceId], [Change], a)
runActionT fTrace store transactionId reasons (ActionT ma) = do
  let env =
        ActionEnv
          { aeTrace = fTrace
          , aeStore = hoistStore lift store
          , aeTransactionId = transactionId
          , aeReasons = reasons
          }
  (a, ActionSummary pending changes) <-
    liftEither =<< runExceptT (runWriterT $ flip runReaderT env ma)
  pure (pending, changes, a)

askStore :: Monad m => ActionT m (Store (ActionT m))
askStore = ActionT $ asks aeStore

askTransactionId :: Monad m => ActionT m TransactionId
askTransactionId = ActionT $ asks aeTransactionId

trace :: MonadIO m => String -> ActionT m ()
trace s = ActionT $ do
  f <- asks aeTrace
  liftIO $ f s

setDependencies :: Monad m => ResourceId -> Set ResourceId -> ActionT m ()
setDependencies a bs = do
  store <- askStore
  transactionId <- askTransactionId

  resTyA <- Store.getResourceType store transactionId $ resourceType a

  for_ bs $ \b -> do
    let ResourceId resTyName resName = b
    resTyB <- Store.getResourceType store transactionId resTyName
    exists <- Store.doesResourceExist resTyB resName
    unless exists . throwError . DiagnosticSimple $
      "dependency " ++ renderResourceId b ++ " does not exist"

  dependencies <- Store.listDependencies resTyA $ resourceName a

  for_ dependencies $ Store.removeDependency resTyA (resourceName a)

  for_ bs $ Store.createDependency resTyA (resourceName a)

putResource :: MonadIO m => ResourceId -> LazyByteString -> ActionT m ()
putResource resId@(ResourceId resTyName resName) content = do
  trace $ "putResource: " ++ renderResourceId resId

  store <- askStore
  transactionId <- askTransactionId

  reasons <- ActionT $ asks aeReasons

  resTy <- Store.getResourceType store transactionId $ fromString resTyName
  exists <- Store.doesResourceExist resTy resName
  if exists
    then do
      Store.writeResource resTy resName content
      let changes = [Change Updated resId reasons]
      ActionT $ tell mempty{asChanges = changes}
    else do
      Store.writeResource resTy resName content
      let changes = [Change Created resId reasons]
      ActionT $ tell mempty{asChanges = changes}
  ActionT $ tell mempty{asPending = [resId]}

rule ::
  -- | ID
  String ->
  Input m a ->
  Output m b ->
  -- | Action
  (a -> b -> ActionT m ()) ->
  Rules m
rule name inputs outputs f = Rules [Rule name inputs outputs f]

data Input m :: Type -> Type where
  IFmap :: (a -> b) -> Input m a -> Input m b
  IPure :: a -> Input m a
  IApply :: Input m (a -> b) -> Input m a -> Input m b
  IResource :: InputQuantifier m a -> String -> ResourceNamePattern -> Input m a

instance Functor (Input m) where
  fmap = IFmap

instance Applicative (Input m) where
  pure = IPure
  (<*>) = IApply

data InputQuantifier m a where
  IAny :: InputQuantifier m (ResourceInput m)
  IAll :: InputQuantifier m (ResourceInputs m)

data ResourceInput m
  = ResourceInput
  { resourceInputId :: !ResourceId
  , resourceInputType :: !(Store.ResourceType (ActionT m))
  , resourceInputMetadata :: !(Map Text MetadataValue)
  , resourceInputContent :: LazyByteString
  }

data ResourceInputs m
  = ResourceInputs
  { resourceInputsType :: !String
  , resourceInputs :: ![ResourceInput m]
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

data InputTuple a
  = InputTuple
  { inputTupleAge :: !Age
  , inputTupleReasons :: ![Reason]
  , inputTupleHeaders :: ![ResourceId]
  , inputTupleBindings :: !(Map String String)
  , inputTupleValue :: !a
  }
  deriving (Functor)

data InputTuples a
  = InputTuples
      -- | Olds
      [InputTuple a]
      -- | News
      [InputTuple a]
  deriving (Functor)

inputTupleJoin ::
  InputTuple (a -> b) ->
  InputTuple a ->
  Maybe (InputTuple b)
inputTupleJoin t1 t2 = do
  let
    common :: Map String (Maybe String)
    common =
      Map.intersectionWith (\x y -> x <$ guard (x == y)) (inputTupleBindings t1) (inputTupleBindings t2)

    left = Map.difference (inputTupleBindings t1) common
    right = Map.difference (inputTupleBindings t2) common

  common' <- sequence common
  pure $!
    InputTuple
      { inputTupleAge = inputTupleAge t1 <> inputTupleAge t2
      , inputTupleReasons = inputTupleReasons t1 <> inputTupleReasons t2
      , inputTupleHeaders = inputTupleHeaders t1 <> inputTupleHeaders t2
      , inputTupleBindings = left <> right <> common'
      , inputTupleValue = inputTupleValue t1 (inputTupleValue t2)
      }

instance Applicative InputTuples where
  pure a =
    InputTuples
      [ InputTuple
          { inputTupleAge = Old
          , inputTupleReasons = []
          , inputTupleHeaders = []
          , inputTupleBindings = mempty
          , inputTupleValue = a
          }
      ]
      []
  (<*>) (InputTuples old1 new1) (InputTuples old2 new2) =
    InputTuples
      [z | x <- old1, y <- old2, Just z <- [inputTupleJoin x y]]
      ( [z | x <- new1, y <- old2 ++ new2, Just z <- [inputTupleJoin x y]]
          ++ [z | x <- old1, y <- new2, Just z <- [inputTupleJoin x y]]
      )

queryInputs ::
  Monad m =>
  Input m a ->
  Set ResourceId ->
  ActionT m (InputTuples a)
queryInputs i = go i
  where
    go ::
      Monad m =>
      Input m a ->
      Set ResourceId ->
      ActionT m (InputTuples a)
    go (IFmap f deps) changes =
      (fmap . fmap) f (go deps changes)
    go (IPure a) _changes =
      pure $ pure a
    go (IApply deps deps') changes =
      liftA2
        (<*>)
        (go deps changes)
        (go deps' changes)
    go (IResource quant resTyName resNamePat) changes = do
      store <- askStore
      transactionId <- askTransactionId

      mResTy <- Store.lookupResourceType store transactionId resTyName
      case mResTy of
        Nothing ->
          -- TODO: not sure if this is the best way to handle missing resource types,
          -- but right now anything stricter stops me from state-machine-testing the
          -- system due to the built-in rules.
          pure $ InputTuples [] []
        Just resTy -> do
          resources <- Store.listResource resTy

          (olds, news) <- do
            (olds, news) <-
              foldlM
                ( \acc@(olds', news') resId ->
                    case matchResourceName resNamePat $ resourceName resId of
                      Nothing -> pure acc
                      Just bindings -> do
                        value <- makeResource resId
                        if resId `Set.member` changes
                          then do
                            let
                              new =
                                InputTuple
                                  { inputTupleAge = New
                                  , inputTupleReasons = [Reason Updated resId]
                                  , inputTupleHeaders = [resId]
                                  , inputTupleBindings = bindings
                                  , inputTupleValue = value
                                  }
                            pure (olds', news' . (new :))
                          else do
                            let
                              old =
                                InputTuple
                                  { inputTupleAge = Old
                                  , inputTupleReasons = []
                                  , inputTupleHeaders = [resId]
                                  , inputTupleBindings = bindings
                                  , inputTupleValue = value
                                  }

                            pure (olds' . (old :), news')
                )
                (id, id)
                resources
            pure (olds [], news [])

          case quant of
            IAny ->
              pure $ InputTuples olds news
            IAll ->
              if null news
                then
                  pure $
                    InputTuples
                      [ InputTuple
                          { inputTupleAge = Old
                          , inputTupleReasons = []
                          , inputTupleHeaders = [ResourceId resTyName "*"]
                          , inputTupleBindings = mempty
                          , inputTupleValue = ResourceInputs resTyName . fmap inputTupleValue $ news ++ olds
                          }
                      ]
                      []
                else do
                  let reasons = nub [reason' | new <- news, reason' <- inputTupleReasons new]
                  pure $
                    InputTuples
                      []
                      [ InputTuple
                          { inputTupleAge = New
                          , inputTupleReasons = reasons
                          , inputTupleHeaders = [ResourceId resTyName "*"]
                          , inputTupleBindings = mempty
                          , inputTupleValue = ResourceInputs resTyName . fmap inputTupleValue $ news ++ olds
                          }
                      ]

makeResource :: Monad m => ResourceId -> ActionT m (ResourceInput m)
makeResource resId@(ResourceId resTyName resName) = do
  store <- askStore
  transactionId <- askTransactionId

  resTy <- Store.getResourceType store transactionId resTyName
  mContent <- Store.readResource resTy resName
  case mContent of
    Nothing ->
      error $ "resource " ++ renderResourceId resId ++ " does not exist"
    Just content -> do
      metadata <- do
        mMetadataContent <- Store.readResourceMetadata resTy resName
        maybe
          (pure mempty)
          (parseResourceMetadata (Store.resourceTypeConfig resTy) resTyName resName)
          mMetadataContent
      pure
        ResourceInput
          { resourceInputId = resId
          , resourceInputType = resTy
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

{-| Declare an input of a particular resource type, matching the given pattern.

Every matching input in the change set triggers a rule invocation.
-}
iResource ::
  -- | Resource type name
  String ->
  -- | Resource name
  ResourceNamePattern ->
  Input m (ResourceInput m)
iResource resTyName = IResource IAny resTyName

{-| Declare a bulk input of a particular resource type, matching the given pattern.

Every matching input in the change set is collected, and passed to a single rule invocation as a single input.
-}
iResourceAll ::
  -- | Resource type
  String ->
  -- | Resource name
  ResourceNamePattern ->
  Input m (ResourceInputs m)
iResourceAll = IResource IAll

data Output m :: Type -> Type where
  OFmap :: (a -> b) -> Output m a -> Output m b
  OPure :: a -> Output m a
  OApply :: Output m (a -> b) -> Output m a -> Output m b
  OResourceType :: String -> Output m (ResourceOutput m String)
  OResource :: String -> OutputResourceNamePattern a -> Output m (ResourceOutput m a)

instance Functor (Output m) where
  fmap = OFmap

instance Applicative (Output m) where
  pure = OPure
  (<*>) = OApply

newtype ResourceOutput m a
  = ResourceOutput
  { writeResource :: a -> LazyByteString -> ActionT m ()
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

oResourceType :: String -> Output m (ResourceOutput m String)
oResourceType = OResourceType

oResource ::
  -- | Resource type name
  String ->
  -- | Resource name pattern
  OutputResourceNamePattern a ->
  Output m (ResourceOutput m a)
oResource resTyName = OResource resTyName

makeOutput :: MonadIO m => Map String String -> Output m a -> a
makeOutput bindings = go
  where
    go :: MonadIO m => Output m a -> a
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

inputResourceIdPatterns :: Input m a -> [ResourceIdPattern]
inputResourceIdPatterns (IFmap _f a) = inputResourceIdPatterns a
inputResourceIdPatterns (IPure _a) = []
inputResourceIdPatterns (IApply a b) = inputResourceIdPatterns a ++ inputResourceIdPatterns b
inputResourceIdPatterns (IResource _quant resTyName resNamePat) = [ResourceIdPattern resTyName resNamePat]

outputResourceIdPatterns :: Output m a -> [ResourceIdPattern]
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
  forall m.
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Trace
  (String -> IO ()) ->
  Store m ->
  TransactionId ->
  Rules m ->
  -- | The created/updated resource
  ResourceId ->
  m [Change]
evalRules fTrace store transactionId (Rules rs) resId = do
  execWriterT . flip evalStateT mempty $ do
    dependents <- lift . lift $ do
      resTy <- Store.getResourceType store transactionId $ resourceType resId
      Store.listDependents resTy $ resourceName resId
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

    go :: StateT (Set ResourceId) (WriterT [Change] m) ()
    go = for_ vertices $ \vertex -> do
      let (r, _, _) = fromVertex vertex
      changedResources <- get
      (changedResources', changes, ()) <-
        lift . lift . runActionT fTrace store transactionId [] $
          runRule r changedResources
      dependents <-
        concat
          <$> traverse
            ( \changed -> lift . lift $ do
                resTy <- Store.getResourceType store transactionId $ resourceType changed
                Store.listDependents resTy $ resourceName changed
            )
            changedResources'
      modify $ (Set.fromList changedResources' <>) . (Set.fromList dependents <>)
      tell changes
