{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Barbies
import Blog (ResourceId (..), renderResourceId)
import qualified Blog.ID as ID
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.Char as Char
import Data.Foldable (for_)
import Data.Functor.Classes (Eq1)
import Data.Kind (Type)
import Data.List (sort, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)
import Data.String (fromString)
import Data.X509.CertificateStore (CertificateStore, readCertificateStore)
import GHC.Generics (Generic)
import Hedgehog
  ( Callback (..)
  , Command (..)
  , MonadGen
  , Var
  , annotateShow
  , classify
  , concrete
  , evalMaybe
  , executeSequential
  , footnote
  , forAll
  , label
  , (===)
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.Connection (TLSSettings (..))
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http.Tls
import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Types.Status (badRequest400, created201, notFound404, ok200)
import qualified Network.TLS as Tls
import Network.TLS.Extra.Cipher (ciphersuite_default)
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hClose, hGetContents)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (callProcess, createProcess, readProcess, terminateProcess, waitForProcess)
import qualified System.Process as Process
import qualified Test.Blog.Store.Overlay
import Test.Hspec (Spec, describe, hspec, it, runIO)
import Test.Hspec.Hedgehog (hedgehog)

main :: IO ()
main = hspec spec

-- <https://stackoverflow.com/a/41816183>
httpManager :: CertificateStore -> IO Http.Manager
httpManager caStore =
  Http.newManager $
    Http.Tls.mkManagerSettings
      (TLSSettings clientParams)
      Nothing
  where
    clientParams =
      (Tls.defaultParamsClient serverName serverId)
        { Tls.clientUseServerNameIndication = True
        , Tls.clientShared = Tls.defaultShared{Tls.sharedCAStore = caStore}
        , Tls.clientSupported = Tls.defaultSupported{Tls.supportedCiphers = ciphersuite_default}
        }
    serverName = mempty
    serverId = mempty

http ::
  Http.Manager ->
  -- | URL
  String ->
  -- | Method
  ByteString ->
  RequestHeaders ->
  LazyByteString ->
  IO (Http.Response LazyByteString)
http manager url method headers body = do
  request <- do
    request <- Http.parseRequest url
    pure
      request
        { Http.method = method
        , Http.requestHeaders = headers
        , Http.requestBody = Http.RequestBodyLBS body
        }
  Http.httpLbs request manager

httpGet ::
  Http.Manager ->
  -- | URL
  String ->
  RequestHeaders ->
  IO (Http.Response LazyByteString)
httpGet manager url headers = http manager url (fromString "GET") headers mempty

httpPost ::
  Http.Manager ->
  -- | URL
  String ->
  RequestHeaders ->
  LazyByteString ->
  IO (Http.Response LazyByteString)
httpPost manager url headers = http manager url (fromString "POST") headers

httpPut ::
  Http.Manager ->
  -- | URL
  String ->
  RequestHeaders ->
  LazyByteString ->
  IO (Http.Response LazyByteString)
httpPut manager url headers = http manager url (fromString "PUT") headers

spec :: Spec
spec = do
  describe "Blog.ID" $ do
    it "toString . fromString = id" . hedgehog $ do
      i <- forAll $ Gen.string (Range.singleton 32) (Gen.element $ ['a' .. 'f'] ++ ['0' .. '9'])
      fmap ID.toString (ID.fromString i) === Just i

  Test.Blog.Store.Overlay.spec

  describe "state machine tests" $ do
    describe "server" $ do
      (caStore, blogServerPath) <-
        runIO $ do
          let caCert = "tls/root.crt"
          caStore <- do
            mStore <- readCertificateStore caCert
            case mStore of
              Nothing -> do
                putStrLn $ "error: failed to read CA certificate from " ++ caCert
                exitFailure
              Just store -> pure store

          callProcess "cabal" ["build", "blog-server"]
          blogServerPath <- filter (not . Char.isSpace) <$> readProcess "cabal" ["list-bin", "blog-server"] ""

          pure (caStore, blogServerPath)

      it "main" $ do
        hedgehog $ do
          manager <- liftIO $ httpManager caStore

          cs <- forAll $ Gen.sequential (Range.constant 0 200) initialState commands
          tmpDir <-
            liftIO $ getCanonicalTemporaryDirectory >>= \tmp -> createTempDirectory tmp "blog-server-tests"
          footnote $ "test data: " ++ tmpDir

          let dataDir = tmpDir </> "data"
          liftIO $ createDirectory dataDir

          let
            proc =
              ( Process.proc
                  blogServerPath
                  [ "--data"
                  , dataDir
                  , "--cert"
                  , "tls/localhost.crt"
                  , "--key"
                  , "tls/localhost.key"
                  , "--port"
                  , "8080"
                  ]
              )
                { Process.std_in = Process.Inherit
                , Process.std_out = Process.CreatePipe
                , Process.std_err = Process.CreatePipe
                }
          (_mStdin, mStdout, mStderr, processHandle) <- liftIO $ createProcess proc

          readyVar <- liftIO newEmptyMVar
          stdoutThread <-
            case mStdout of
              Nothing -> undefined
              Just hStdout ->
                liftIO . async $ do
                  let path = tmpDir </> "stdout"
                  contents <- hGetContents hStdout
                  case filter (isJust . stripPrefix "Running at") $ lines contents of
                    _ : _ ->
                      putMVar readyVar $ Right ()
                    [] ->
                      putMVar readyVar . Left $
                        "missing log start phrase (got: " ++ show contents ++ ") (logs stored in " ++ tmpDir ++ ")"
                  writeFile path contents
                  hClose hStdout
          stderrThread <-
            case mStderr of
              Nothing -> undefined
              Just hStderr ->
                liftIO . async $ do
                  let path = tmpDir </> "stderr"
                  contents <- hGetContents hStderr
                  writeFile path contents
                  hClose hStderr

          let
            cleanup = do
              terminateProcess processHandle
              _ <- waitForProcess processHandle
              wait stdoutThread
              wait stderrThread

          result <- liftIO $ takeMVar readyVar
          either error pure result

          hoist (`finally` cleanup) $ do
            runReaderT (executeSequential initialState cs) manager

          liftIO $ removeDirectoryRecursive tmpDir

data State (v :: Type -> Type)
  = State
  { stateTransaction :: Maybe (StateTransaction v)
  , stateResourceTypes :: Map String StateResourceType
  , stateResources :: Map String (Map String StateResource)
  }

data StateResourceType
  = StateResourceType
  { stateResourceTypeContentType :: LazyByteString
  }
  deriving (Show)

stateResourceTypeContent :: StateResourceType -> LazyByteString
stateResourceTypeContent resTy =
  ByteString.Lazy.Char8.unlines
    [ fromString "content-type = \"" <> stateResourceTypeContentType resTy <> fromString "\""
    , mempty
    , fromString "[metadata]"
    ]

data StateResource
  = StateResource
  { stateResourceContent :: LazyByteString
  }

data StateTransaction (v :: Type -> Type)
  = StateTransaction
  { stateTransactionId :: Var ByteString v
  , stateTransactionResourceTypes :: StateTransactionResources StateResourceType StateResourceType ()
  , stateTransactionResources ::
      StateTransactionResources (Map String StateResource) (Map String StateResource) (Set String)
  }

data StateTransactionResources create update delete
  = StateTransactionResources
  { stateTransactionResourcesCreate :: Map String create
  , stateTransactionResourcesUpdate :: Map String update
  , stateTransactionResourcesDelete :: Map String delete
  }

stateLookupResourceType ::
  Eq1 v =>
  State v ->
  -- | Transaction ID
  Maybe (Var ByteString v) ->
  -- | Resource type
  String ->
  Maybe StateResourceType
stateLookupResourceType state Nothing resTy = Map.lookup resTy $ stateResourceTypes state
stateLookupResourceType state (Just xactId) resTy = do
  transaction <- stateTransaction state
  guard $ stateTransactionId transaction == xactId
  Map.lookup resTy $ stateResourceTypesTransactionView state

stateLookupResource ::
  Eq1 v =>
  State v ->
  -- | Transaction ID
  Maybe (Var ByteString v) ->
  -- | Resource type
  String ->
  -- | Resource name
  String ->
  Maybe StateResource
stateLookupResource state Nothing resTy resName = Map.lookup resName =<< Map.lookup resTy (stateResources state)
stateLookupResource state (Just xactId) resTy resName = do
  transaction <- stateTransaction state
  guard $ stateTransactionId transaction == xactId
  Map.lookup resName =<< Map.lookup resTy (stateResourcesTransactionView state)

stateCommit :: State v -> State v
stateCommit state =
  state
    { stateTransaction = Nothing
    , stateResourceTypes = stateResourceTypesTransactionView state
    , stateResources = stateResourcesTransactionView state
    }

stateResourceTypesTransactionView :: State v -> Map String StateResourceType
stateResourceTypesTransactionView state =
  case stateTransaction state of
    Nothing ->
      stateResourceTypes state
    Just transaction ->
      insertCreatedResourceTypes
        (stateTransactionResourcesCreate $ stateTransactionResourceTypes transaction)
        $ modifyUpdatedResourceTypes
          (stateTransactionResourcesUpdate $ stateTransactionResourceTypes transaction)
        $ removeDeletedResourceTypes
          (stateTransactionResourcesDelete $ stateTransactionResourceTypes transaction)
        $ stateResourceTypes state
  where
    insertCreatedResourceTypes ::
      Map String StateResourceType ->
      Map String StateResourceType ->
      Map String StateResourceType
    insertCreatedResourceTypes created resources = Map.unionWith (\new _old -> new) created resources

    modifyUpdatedResourceTypes ::
      Map String StateResourceType ->
      Map String StateResourceType ->
      Map String StateResourceType
    modifyUpdatedResourceTypes updated resources = Map.unionWith (\new _old -> new) updated resources

    removeDeletedResourceTypes ::
      Map String () ->
      Map String StateResourceType ->
      Map String StateResourceType
    removeDeletedResourceTypes deleted resources =
      resources `Map.difference` deleted

stateResourcesTransactionView :: State v -> Map String (Map String StateResource)
stateResourcesTransactionView state =
  case stateTransaction state of
    Nothing ->
      stateResources state
    Just transaction ->
      insertCreatedResources (stateTransactionResourcesCreate $ stateTransactionResources transaction) $
        modifyUpdatedResources (stateTransactionResourcesUpdate $ stateTransactionResources transaction) $
          removeDeletedResources (stateTransactionResourcesDelete $ stateTransactionResources transaction) $
            stateResources state
  where
    insertCreatedResources ::
      Map String (Map String StateResource) ->
      Map String (Map String StateResource) ->
      Map String (Map String StateResource)
    insertCreatedResources created resources = Map.unionWith (<>) created resources

    modifyUpdatedResources ::
      Map String (Map String StateResource) ->
      Map String (Map String StateResource) ->
      Map String (Map String StateResource)
    modifyUpdatedResources updated resources = Map.unionWith (<>) updated resources

    removeDeletedResources ::
      Map String (Set String) ->
      Map String (Map String StateResource) ->
      Map String (Map String StateResource)
    removeDeletedResources deleted resources =
      Map.differenceWith (\rs ids -> Just $ foldl' (flip Map.delete) rs ids) resources deleted

initialState :: State v
initialState =
  State
    { stateTransaction = Nothing
    , stateResourceTypes = mempty
    , stateResources = mempty
    }

commands ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) =>
  [Command gen m State]
commands =
  [ cBegin
  , cCommit
  , cResourceTypeCreate
  , cResourceTypeCreateDuplicate
  , cResourceTypeUpdate
  , cResourceTypeUpdateMissing
  , cResourceTypeList
  , cResourceTypeGet GetHeaders
  , cResourceTypeGet GetUrl
  , cResourceCreate
  , cResourceCreateMissing
  , cResourceCreateDuplicate
  , cResourceList
  , cResourceListMissing
  , cResourceGet GetHeaders
  , cResourceGet GetUrl
  , cResourceGetMissing GetHeaders
  , cResourceGetMissing GetUrl
  ]

data ResourceTypeList (v :: Type -> Type)
  = ResourceTypeList
      -- | Transaction ID
      (Maybe (Var ByteString v))
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceTypeList :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceTypeList =
  Command
    ( \state -> do
        pure $
          ResourceTypeList
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
    )
    ( \(ResourceTypeList mXactId) -> do
        manager <- ask
        let resTy = "resource"
        let headers = [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpGet manager ("https://localhost:8080/.resource/" ++ resTy) headers
    )
    [ Require $ \state (ResourceTypeList mXactId) ->
        ( case mXactId of
            Nothing -> True
            Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
        )
    , Ensure $ \old _new (ResourceTypeList mXactId) output -> do
        classify (fromString "resource type list (inside transaction)") $ isJust mXactId
        classify (fromString "resource type list (outside transaction)") $ isNothing mXactId

        annotateShow output
        Http.responseStatus output === ok200

        let
          resourceIds =
            fmap (ResourceId "resource") $
              case mXactId of
                Nothing ->
                  Map.keys $ stateResourceTypes old
                Just{} ->
                  Map.keys $ stateResourceTypesTransactionView old

        classify (fromString "resource type list (count == 0)") $ null resourceIds
        classify (fromString "resource type list (count > 0)") $ length resourceIds > 0

        let actual = sort (fmap ByteString.Lazy.Char8.unpack . ByteString.Lazy.Char8.lines $ Http.responseBody output)
        let expected = fmap renderResourceId resourceIds
        actual === expected
    ]

genResourceName :: MonadGen m => m String
genResourceName =
  Gen.list (Range.constant 1 20) genResourceNameChar
  where
    genResourceNameChar = Gen.element (['a' .. 'z'] ++ "-")

genStateResourceType :: MonadGen m => m StateResourceType
genStateResourceType = StateResourceType <$> fmap fromString (Gen.string (Range.constant 1 20) Gen.alphaNum)

data ResourceTypeCreate (v :: Type -> Type)
  = ResourceTypeCreate
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource name
      String
      StateResourceType
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceTypeCreate :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceTypeCreate =
  Command
    ( \state -> do
        pure $
          ResourceTypeCreate
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> genResourceName
            <*> genStateResourceType
    )
    ( \(ResourceTypeCreate mXactId resName value) -> do
        manager <- ask
        let
          headers =
            [ (fromString "X-Blog-ResourceType", fromString "resource")
            , (fromString "X-Blog-ResourceName", fromString resName)
            ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $
          httpPost manager "https://localhost:8080/.resource" headers (stateResourceTypeContent value)
    )
    [ Require $ \state (ResourceTypeCreate mXactId resName _value) ->
        ( case mXactId of
            Nothing -> True
            Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && (not $ resName `Map.member` stateResourceTypesTransactionView state)
    , Update $ \state (ResourceTypeCreate mXactId resName value) _output ->
        case mXactId of
          Nothing ->
            state
              { stateResourceTypes =
                  Map.insert resName value $ stateResourceTypes state
              }
          Just xactId ->
            case stateTransaction state of
              Just transaction
                | xactId == stateTransactionId transaction ->
                    let
                      transactionResourceTypes = stateTransactionResourceTypes transaction
                      transactionResourceTypes' =
                        transactionResourceTypes
                          { stateTransactionResourcesCreate =
                              Map.insert resName value $ stateTransactionResourcesCreate transactionResourceTypes
                          }
                      transaction' = transaction{stateTransactionResourceTypes = transactionResourceTypes'}
                    in
                      state{stateTransaction = Just transaction'}
              _ -> state
    , Ensure $ \_old _new (ResourceTypeCreate mXactId _resName value) output -> do
        label (fromString "resource type create")
        classify (fromString "resource type create (inside transaction)") $ isJust mXactId
        classify (fromString "resource type create (outside transaction)") $ isNothing mXactId

        annotateShow value
        annotateShow output
        Http.responseStatus output === created201
    ]

cResourceTypeCreateDuplicate ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceTypeCreateDuplicate =
  Command
    ( \state -> do
        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes
        pure $
          ResourceTypeCreate
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> Gen.element (Map.keys resourceTypes)
            <*> genStateResourceType
    )
    ( \(ResourceTypeCreate mXactId resName value) -> do
        manager <- ask
        let
          headers =
            [ (fromString "X-Blog-ResourceType", fromString "resource")
            , (fromString "X-Blog-ResourceName", fromString resName)
            ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $
          httpPost manager "https://localhost:8080/.resource" headers (stateResourceTypeContent value)
    )
    [ Require $ \state (ResourceTypeCreate mXactId resName _value) ->
        ( case mXactId of
            Nothing -> True
            Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && (isJust $ stateLookupResourceType state mXactId resName)
    , Ensure $ \_old _new (ResourceTypeCreate mXactId _resName _value) output -> do
        label (fromString "resource type create (duplicate)")
        classify (fromString "resource type create (duplicate) (inside transaction)") $ isJust mXactId
        classify (fromString "resource type create (duplicate) (outside transaction)") $ isNothing mXactId

        annotateShow output
        Http.responseStatus output === badRequest400
    ]

data ResourceCreate (v :: Type -> Type)
  = ResourceCreate
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource type
      String
      -- | Resource name
      String
      -- | Content
      LazyByteString
  deriving (Show, Generic, FunctorB, TraversableB)

genResourceContent :: MonadGen m => StateResourceType -> m LazyByteString
genResourceContent (StateResourceType _content) =
  ByteString.Lazy.Char8.pack <$> Gen.string (Range.constant 0 1000) Gen.alphaNum

cResourceCreate :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceCreate =
  Command
    ( \state -> do
        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes

        pure $
          (\mXactId (resTy, resContent) resName -> ResourceCreate mXactId resTy resName resContent)
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> ( do
                    (resTyName, resTy) <- Gen.element . Map.toList $ stateResourceTypesTransactionView state
                    (,) resTyName <$> genResourceContent resTy
                )
            <*> genResourceName
    )
    ( \(ResourceCreate mXactId resTy resName content) -> do
        manager <- ask
        let
          headers =
            [ (fromString "X-Blog-ResourceType", fromString resTy)
            , (fromString "X-Blog-ResourceName", fromString resName)
            ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpPost manager "https://localhost:8080/.resource" headers content
    )
    [ Require $ \state (ResourceCreate mXactId resTy resName _content) ->
        ( case mXactId of
            Nothing ->
              True
            Just xactId ->
              fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && isJust (stateLookupResourceType state mXactId resTy)
          && isNothing (stateLookupResource state mXactId resTy resName)
    , Update $ \state (ResourceCreate mXactId resTy resName content) _output ->
        case mXactId of
          Nothing ->
            state
              { stateResources =
                  Map.insertWith (<>) resTy (Map.singleton resName (StateResource content)) $ stateResources state
              }
          Just xactId ->
            case stateTransaction state of
              Just transaction
                | xactId == stateTransactionId transaction ->
                    let
                      transactionResources = stateTransactionResources transaction
                      transactionResources' =
                        transactionResources
                          { stateTransactionResourcesCreate =
                              Map.insertWith (<>) resTy (Map.singleton resName (StateResource content)) $
                                stateTransactionResourcesCreate transactionResources
                          }
                      transaction' = transaction{stateTransactionResources = transactionResources'}
                    in
                      state{stateTransaction = Just transaction'}
              _ -> state
    , Ensure $ \_old _new (ResourceCreate mXactId _resTy _resName content) output -> do
        label (fromString "resource create")
        classify (fromString "resource create (inside transaction)") $ isJust mXactId
        classify (fromString "resource create (outside transaction)") $ isNothing mXactId

        annotateShow content
        annotateShow output
        Http.responseStatus output === created201
    ]

cResourceCreateMissing ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceCreateMissing =
  Command
    ( \state -> do
        pure $
          ResourceCreate
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> genResourceName
            <*> genResourceName
            <*> pure mempty
    )
    ( \(ResourceCreate mXactId resTy resName content) -> do
        manager <- ask
        let
          headers =
            [ (fromString "X-Blog-ResourceType", fromString resTy)
            , (fromString "X-Blog-ResourceName", fromString resName)
            ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpPost manager "https://localhost:8080/.resource" headers content
    )
    [ Require $ \state (ResourceCreate mXactId resTy _resName _content) ->
        ( case mXactId of
            Nothing ->
              True
            Just xactId ->
              fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && isNothing (stateLookupResourceType state mXactId resTy)
    , Ensure $ \_old _new (ResourceCreate mXactId _resTy _resName _content) output -> do
        label (fromString "resource create (missing)")
        classify (fromString "resource create (missing) (inside transaction)") $ isJust mXactId
        classify (fromString "resource create (missing) (outside transaction)") $ isNothing mXactId

        annotateShow output
        Http.responseStatus output === badRequest400
    ]

cResourceCreateDuplicate ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceCreateDuplicate =
  Command
    ( \state -> do
        let nonemptyResources = Map.filter (not . null) $ stateResourcesTransactionView state
        guard . not $ null nonemptyResources
        pure $
          (\mXactId (resTy, resName) -> ResourceCreate mXactId resTy resName mempty)
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> ( do
                    (resTy, res) <- Gen.element $ Map.toList nonemptyResources
                    (resName, _resValue) <- Gen.element $ Map.toList res
                    pure (resTy, resName)
                )
    )
    ( \(ResourceCreate mXactId resTy resName content) -> do
        manager <- ask
        let
          headers =
            [ (fromString "X-Blog-ResourceType", fromString resTy)
            , (fromString "X-Blog-ResourceName", fromString resName)
            ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpPost manager "https://localhost:8080/.resource" headers content
    )
    [ Require $ \state (ResourceCreate mXactId resTy resName _content) ->
        ( case mXactId of
            Nothing ->
              True
            Just xactId ->
              fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && isJust (stateLookupResource state mXactId resTy resName)
    , Ensure $ \_old _new (ResourceCreate mXactId _resTy _resName _content) output -> do
        label (fromString "resource create (duplicate)")
        classify (fromString "resource create (duplicate) (inside transaction)") $ isJust mXactId
        classify (fromString "resource create (duplicate) (outside transaction)") $ isNothing mXactId

        annotateShow output
        Http.responseStatus output === badRequest400
    ]

data ResourceList (v :: Type -> Type)
  = ResourceList
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource type
      String
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceList :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceList =
  Command
    ( \state -> do
        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes
        pure $
          ResourceList
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> Gen.element (Map.keys resourceTypes)
    )
    ( \(ResourceList mXactId resTy) -> do
        manager <- ask
        let headers = [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpGet manager ("https://localhost:8080/.resource/" ++ resTy) headers
    )
    [ Require $ \state (ResourceList mXactId resTy) ->
        ( case mXactId of
            Nothing ->
              Map.member resTy (stateResourceTypes state)
            Just xactId ->
              fmap stateTransactionId (stateTransaction state) == Just xactId
                && Map.member resTy (stateResourceTypesTransactionView state)
        )
    , Ensure $ \old _new (ResourceList mXactId resTy) output -> do
        classify (fromString "resource list (inside transaction)") $ isJust mXactId
        classify (fromString "resource list (outside transaction)") $ isNothing mXactId

        annotateShow output
        Http.responseStatus output === ok200

        let
          resourceIds =
            fmap (ResourceId resTy) $
              case mXactId of
                Nothing ->
                  foldMap Map.keys $ Map.lookup resTy (stateResources old)
                Just{} ->
                  foldMap Map.keys $ Map.lookup resTy (stateResourcesTransactionView old)

        classify (fromString "resource list (count == 0)") $ null resourceIds
        classify (fromString "resource list (count > 0)") $ length resourceIds > 0

        let actual = sort (fmap ByteString.Lazy.Char8.unpack . ByteString.Lazy.Char8.lines $ Http.responseBody output)
        let expected = fmap renderResourceId resourceIds
        actual === expected
    ]

cResourceListMissing :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceListMissing =
  Command
    ( \state -> do
        pure $
          ResourceList
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> genResourceName
    )
    ( \(ResourceList mXactId resTy) -> do
        manager <- ask
        let headers = [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpGet manager ("https://localhost:8080/.resource/" ++ resTy) headers
    )
    [ Require $ \state (ResourceList mXactId resTy) ->
        ( case mXactId of
            Nothing ->
              True
            Just xactId ->
              fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && isNothing (stateLookupResourceType state mXactId resTy)
    , Ensure $ \_old _new (ResourceList mXactId _resTy) output -> do
        classify (fromString "resource list (missing) (inside transaction)") $ isJust mXactId
        classify (fromString "resource list (missing) (outside transaction)") $ isNothing mXactId

        annotateShow output
        Http.responseStatus output === notFound404
    ]

data ResourceGet (v :: Type -> Type)
  = ResourceGet
      -- | Use the transaction ID
      (Maybe (Var ByteString v))
      -- | Resource type
      String
      -- | Resource name
      String
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceGet ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => GetStyle -> Command gen m State
cResourceGet getStyle =
  Command
    ( \state -> do
        let nonemptyResources = Map.filter (not . null) $ stateResourcesTransactionView state
        guard . not $ null nonemptyResources
        pure $
          (\mXactId (resTy, resName) -> ResourceGet mXactId resTy resName)
            <$> Gen.element ([Nothing] ++ [Just $ stateTransactionId xact | Just xact <- [stateTransaction state]])
            <*> ( do
                    (resTy, res) <- Gen.element $ Map.toList nonemptyResources
                    (resName, _resValue) <- Gen.element $ Map.toList res
                    pure (resTy, resName)
                )
    )
    ( \(ResourceGet mXactId resTy resName) -> do
        manager <- ask
        case getStyle of
          GetHeaders -> do
            let
              headers =
                [ (fromString "X-Blog-ResourceType", fromString resTy)
                , (fromString "X-Blog-ResourceName", fromString resName)
                ]
                  ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $ httpGet manager "https://localhost:8080/.resource" headers
          GetUrl -> do
            let headers = [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $ httpGet manager ("https://localhost:8080/.resource/" ++ resTy ++ "/" ++ resName) headers
    )
    [ Require $ \state (ResourceGet mXactId resTy resName) ->
        ( case mXactId of
            Nothing -> True
            Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && isJust (stateLookupResource state mXactId resTy resName)
    , Ensure $ \old _new (ResourceGet mXactId resTyName resName) output -> do
        let
          getStyleDesc =
            case getStyle of
              GetHeaders -> "(via headers)"
              GetUrl -> "(via url)"
        label (fromString $ "resource get " ++ getStyleDesc)
        classify (fromString $ "resource get " ++ getStyleDesc ++ " (inside transaction)") (isJust mXactId)
        classify
          (fromString $ "resource get " ++ getStyleDesc ++ " (outside transaction)")
          (isNothing mXactId)

        annotateShow output
        Http.responseStatus output === ok200

        resTy <- evalMaybe $ stateLookupResource old mXactId resTyName resName
        Http.responseBody output === stateResourceContent resTy
    ]

cResourceGetMissing ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => GetStyle -> Command gen m State
cResourceGetMissing getStyle =
  Command
    ( \state -> do
        let currentResources = stateResourcesTransactionView state
        let genXactId =
              Gen.element ([Nothing] ++ [Just $ stateTransactionId xact | Just xact <- [stateTransaction state]])
        pure $
          Gen.choice $
            [ResourceGet <$> genXactId <*> genResourceName <*> genResourceName]
              ++ [ ResourceGet <$> genXactId <*> Gen.element (Map.keys currentResources) <*> genResourceName
                 | not $ null currentResources
                 ]
    )
    ( \(ResourceGet mXactId resTy resName) -> do
        manager <- ask
        case getStyle of
          GetHeaders -> do
            let
              headers =
                [ (fromString "X-Blog-ResourceType", fromString resTy)
                , (fromString "X-Blog-ResourceName", fromString resName)
                ]
                  ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $ httpGet manager "https://localhost:8080/.resource" headers
          GetUrl -> do
            let headers = [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $ httpGet manager ("https://localhost:8080/.resource/" ++ resTy ++ "/" ++ resName) headers
    )
    [ Require $ \state (ResourceGet mXactId resTy resName) ->
        ( case mXactId of
            Nothing -> True
            Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && isNothing (stateLookupResource state mXactId resTy resName)
    , Ensure $ \_old _new (ResourceGet mXactId _resTyName _resName) output -> do
        let
          getStyleDesc =
            case getStyle of
              GetHeaders -> "(via headers)"
              GetUrl -> "(via url)"
        label (fromString $ "resource get (missing) " ++ getStyleDesc)
        classify
          (fromString $ "resource get (missing) " ++ getStyleDesc ++ " (inside transaction)")
          (isJust mXactId)
        classify
          (fromString $ "resource get (missing) " ++ getStyleDesc ++ " (outside transaction)")
          (isNothing mXactId)

        annotateShow output
        case getStyle of
          GetHeaders -> Http.responseStatus output === badRequest400
          GetUrl -> Http.responseStatus output === notFound404
    ]

data ResourceTypeUpdate (v :: Type -> Type)
  = ResourceTypeUpdate
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource name
      String
      -- | New value
      StateResourceType
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceTypeUpdate :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceTypeUpdate =
  Command
    ( \state -> do
        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes

        pure $
          Gen.choice $
            [ (\mXactId resName -> ResourceTypeUpdate mXactId resName)
                <$> Gen.element
                  ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
                <*> Gen.element (Map.keys resourceTypes)
                <*> genStateResourceType
            ]
              ++ [ (\resName -> ResourceTypeUpdate (Just $ stateTransactionId transaction) resName)
                     <$> Gen.element (Map.keys newResourceTypes)
                     <*> genStateResourceType
                 | Just transaction <- [stateTransaction state]
                 , let newResourceTypes = stateTransactionResourcesCreate (stateTransactionResourceTypes transaction)
                 , not $ null newResourceTypes
                 ]
    )
    ( \(ResourceTypeUpdate mXactId resName value) -> do
        manager <- ask
        let
          headers =
            [ (fromString "X-Blog-ResourceType", fromString "resource")
            , (fromString "X-Blog-ResourceName", fromString resName)
            ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpPut manager "https://localhost:8080/.resource" headers (stateResourceTypeContent value)
    )
    [ Require $ \state (ResourceTypeUpdate mXactId resName _value) ->
        ( case mXactId of
            Nothing -> True
            Just xactId ->
              fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && isJust (stateLookupResourceType state mXactId resName)
    , Update $ \state (ResourceTypeUpdate mXactId resName value) _output ->
        case mXactId of
          Nothing ->
            state
              { stateResourceTypes =
                  Map.insert resName value $ stateResourceTypes state
              }
          Just xactId ->
            case stateTransaction state of
              Just transaction
                | xactId == stateTransactionId transaction ->
                    let
                      transactionResourceTypes = stateTransactionResourceTypes transaction
                      transactionResourceTypes' =
                        case Map.lookup resName (stateTransactionResourcesCreate transactionResourceTypes) of
                          Nothing ->
                            transactionResourceTypes
                              { stateTransactionResourcesUpdate =
                                  Map.insert resName value $ stateTransactionResourcesUpdate transactionResourceTypes
                              }
                          Just _resource ->
                            transactionResourceTypes
                              { stateTransactionResourcesCreate =
                                  Map.insert resName value $ stateTransactionResourcesCreate transactionResourceTypes
                              }
                      transaction' =
                        transaction{stateTransactionResourceTypes = transactionResourceTypes'}
                    in
                      state{stateTransaction = Just transaction'}
              _ -> state
    , Ensure $ \old _new (ResourceTypeUpdate mXactId resName value) output -> do
        label (fromString "resource type update")
        classify (fromString "resource type update (inside transaction)") $ isJust mXactId
        classify (fromString "resource type update (outside transaction)") $ isNothing mXactId

        for_ (stateTransaction old) $ \transaction -> do
          let mCreated = Map.lookup resName . stateTransactionResourcesCreate $ stateTransactionResourceTypes transaction
          classify (fromString "resource type update (created in same transaction)") $ isJust mCreated
          classify (fromString "resource type update (created in previous transaction)") $ isNothing mCreated

        annotateShow value
        annotateShow output
        Http.responseStatus output === ok200
    ]

cResourceTypeUpdateMissing ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceTypeUpdateMissing =
  Command
    ( \state -> do
        pure $
          ResourceTypeUpdate
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> genResourceName
            <*> genStateResourceType
    )
    ( \(ResourceTypeUpdate mXactId resName value) -> do
        manager <- ask
        let
          headers =
            [ (fromString "X-Blog-ResourceType", fromString "resource")
            , (fromString "X-Blog-ResourceName", fromString resName)
            ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpPut manager "https://localhost:8080/.resource" headers (stateResourceTypeContent value)
    )
    [ Require $ \state (ResourceTypeUpdate mXactId resName _value) ->
        ( case mXactId of
            Nothing -> True
            Just xactId ->
              fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && isNothing (stateLookupResourceType state mXactId resName)
    , Ensure $ \_old _new (ResourceTypeUpdate mXactId _resName _value) output -> do
        label (fromString "resource type update (missing)")
        classify (fromString "resource type update (missing) (inside transaction)") $ isJust mXactId
        classify (fromString "resource type update (missing) (outside transaction)") $ isNothing mXactId

        annotateShow output
        Http.responseStatus output === badRequest400
    ]

data ResourceTypeGet (v :: Type -> Type)
  = ResourceTypeGet
      -- | Use the transaction ID
      (Maybe (Var ByteString v))
      -- | Resource name
      String
  deriving (Show, Generic, FunctorB, TraversableB)

data GetStyle = GetHeaders | GetUrl

cResourceTypeGet ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => GetStyle -> Command gen m State
cResourceTypeGet getStyle =
  Command
    ( \state -> do
        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes
        pure $
          ResourceTypeGet
            <$> Gen.element ([Nothing] ++ [Just $ stateTransactionId xact | Just xact <- [stateTransaction state]])
            <*> Gen.element (Map.keys resourceTypes)
    )
    ( \(ResourceTypeGet mXactId resName) -> do
        manager <- ask
        let resTy = "resource"
        case getStyle of
          GetHeaders -> do
            let
              headers =
                [ (fromString "X-Blog-ResourceType", fromString resTy)
                , (fromString "X-Blog-ResourceName", fromString resName)
                ]
                  ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $ httpGet manager "https://localhost:8080/.resource" headers
          GetUrl -> do
            let headers = [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $ httpGet manager ("https://localhost:8080/.resource/" ++ resTy ++ "/" ++ resName) headers
    )
    [ Require $ \state (ResourceTypeGet mXactId resName) ->
        ( case mXactId of
            Nothing -> True
            Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && (isJust $ stateLookupResourceType state mXactId resName)
    , Ensure $ \old _new (ResourceTypeGet mXactId resName) output -> do
        let
          getStyleDesc =
            case getStyle of
              GetHeaders -> "(via headers)"
              GetUrl -> "(via url)"
        label (fromString $ "resource type get " ++ getStyleDesc)
        classify
          (fromString $ "resource type get " ++ getStyleDesc ++ " (inside transaction)")
          (isJust mXactId)
        classify
          (fromString $ "resource type get " ++ getStyleDesc ++ " (outside transaction)")
          (isNothing mXactId)

        annotateShow output
        Http.responseStatus output === ok200

        resTy <- evalMaybe $ stateLookupResourceType old mXactId resName
        Http.responseBody output === stateResourceTypeContent resTy
    ]

data Begin (v :: Type -> Type)
  = Begin
  deriving (Show, Generic, FunctorB, TraversableB)

cBegin :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cBegin =
  Command
    (\_state -> Just $ pure Begin)
    ( \Begin -> do
        manager <- ask
        response <- liftIO $ httpPost manager "https://localhost:8080/.transaction/begin" [] mempty
        pure . LazyByteString.toStrict $ Http.responseBody response
    )
    [ Require $ \state Begin -> isNothing $ stateTransaction state
    , Update $ \state Begin output ->
        let
          transaction =
            StateTransaction
              { stateTransactionId = output
              , stateTransactionResourceTypes = StateTransactionResources mempty mempty mempty
              , stateTransactionResources = StateTransactionResources mempty mempty mempty
              }
        in
          state{stateTransaction = Just transaction}
    , Ensure $ \_old _new _cmd _output -> do
        label $ fromString "begin"
    ]

data Commit (v :: Type -> Type)
  = Commit
      -- | Transaction ID
      (Var ByteString v)
  deriving (Show, Generic, FunctorB, TraversableB)

cCommit :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cCommit =
  Command
    ( \state -> do
        xactId <- stateTransactionId <$> stateTransaction state
        pure $ pure (Commit xactId)
    )
    ( \(Commit xactId) -> do
        manager <- ask
        let headers = [(fromString "X-Blog-TransactionId", concrete xactId)]
        liftIO $ httpPost manager "https://localhost:8080/.transaction/commit" headers mempty
    )
    [ Require $ \state (Commit _) -> isJust $ stateTransaction state
    , Update $ \state (Commit _xactId) _output -> stateCommit state
    , Ensure $ \_old _new (Commit _xactId) output -> do
        label $ fromString "commit"
        annotateShow output
        Http.responseStatus output === ok200
    ]
