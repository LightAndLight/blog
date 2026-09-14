{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Barbies
import Blog (Name, ResourceId (..), renderName, renderResourceId, unsafeName)
import Blog.Diagnostic (renderDiagnosticReports)
import qualified Blog.ID as ID
import Blog.Password (HashOptions (..), defaultHashOptions, hashPassword)
import Blog.Session (sessionIdCookieName)
import qualified Blog.Store as Store
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (guard, unless, (<=<))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.Char as Char
import Data.Foldable (for_)
import Data.Functor.Classes (Eq1)
import Data.Kind (Type)
import Data.List (find, sort, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Set (Set)
import Data.String (fromString)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
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
import Network.HTTP.Types.Status (badRequest400, created201, notFound404, ok200, unauthorized401)
import qualified Network.TLS as Tls
import Network.TLS.Extra.Cipher (ciphersuite_default)
import System.Directory (removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hClose, hGetContents)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (callProcess, createProcess, readProcess, terminateProcess, waitForProcess)
import qualified System.Process as Process
import qualified Test.Blog.Store.Overlay
import Test.Hspec (Spec, describe, hspec, it, runIO)
import Test.Hspec.Hedgehog (hedgehog)
import Web.HttpApiData (toEncodedUrlPiece)

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

httpWithCookies ::
  Http.Manager ->
  [Http.Cookie] ->
  -- | URL
  String ->
  -- | Method
  ByteString ->
  RequestHeaders ->
  LazyByteString ->
  IO (Http.Response LazyByteString)
httpWithCookies manager cookies url method headers body = do
  request <- do
    request <- Http.parseRequest url
    pure
      request
        { Http.method = method
        , Http.requestHeaders = headers
        , Http.requestBody = Http.RequestBodyLBS body
        , Http.cookieJar = Just $ Http.createCookieJar cookies
        }
  Http.httpLbs request manager

http ::
  Http.Manager ->
  -- | URL
  String ->
  -- | Method
  ByteString ->
  RequestHeaders ->
  LazyByteString ->
  IO (Http.Response LazyByteString)
http manager = httpWithCookies manager []

httpGet ::
  Http.Manager ->
  -- | URL
  String ->
  RequestHeaders ->
  IO (Http.Response LazyByteString)
httpGet manager url headers = http manager url (fromString "GET") headers mempty

httpPostWithCookies ::
  Http.Manager ->
  [Http.Cookie] ->
  -- | URL
  String ->
  RequestHeaders ->
  LazyByteString ->
  IO (Http.Response LazyByteString)
httpPostWithCookies manager cookies url = httpWithCookies manager cookies url (fromString "POST")

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

httpPatch ::
  Http.Manager ->
  -- | URL
  String ->
  RequestHeaders ->
  LazyByteString ->
  IO (Http.Response LazyByteString)
httpPatch manager url headers = http manager url (fromString "PATCH") headers

testUsername :: String
testUsername = "test-user"

testPassword :: String
testPassword = "test-password"

testPasswordHash :: LazyByteString
testPasswordHash =
  Text.Lazy.Encoding.encodeUtf8 . LazyText.fromStrict $
    hashPassword testHashOptions (fromString "test-salt") (fromString testPassword)
  where
    -- weak password hashing to speed up tests
    testHashOptions = defaultHashOptions{hashIterations = 1, hashMemory = 8, hashParallelism = 1}

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

          let
            state =
              initialState
                { stateResourceTypes =
                    Map.insert
                      (unsafeName "session")
                      StateResourceType{stateResourceTypeContentType = fromString "text/plain"}
                      $ Map.insert
                        (unsafeName "user")
                        StateResourceType{stateResourceTypeContentType = fromString "text/x.phcs"}
                      $ stateResourceTypes initialState
                , stateResources =
                    Map.insertWith
                      (<>)
                      (unsafeName "user")
                      (Map.singleton (unsafeName testUsername) StateResource{stateResourceContent = testPasswordHash})
                      $ stateResources initialState
                }
          cs <- forAll $ Gen.sequential (Range.constant 0 100) state (commands testUsername testPassword)

          tmpDir <-
            liftIO $ getCanonicalTemporaryDirectory >>= \tmp -> createTempDirectory tmp "blog-server-tests"
          footnote $ "test data: " ++ tmpDir

          let dataDir = tmpDir </> "data"

          let
            -- A hack to seed the database with a user.
            setupStore :: IO ()
            setupStore = do
              store <- Store.fromDirectory dataDir

              let handleExceptT = either (error . ByteString.Lazy.Char8.unpack . renderDiagnosticReports) pure <=< runExceptT
              handleExceptT . Store.withTransaction store False $ \xactId -> do
                resourceTy <- Store.getResourceType store xactId (unsafeName "resource")
                _updated <-
                  Store.writeResource resourceTy (unsafeName "user") . fromString $
                    unlines
                      [ "content-type = \"text/x.phcs\""
                      , ""
                      , "[metadata]"
                      ]
                _updated <-
                  Store.writeResource resourceTy (unsafeName "session") . fromString $
                    unlines
                      [ "content-type = \"text/plain\""
                      , ""
                      , "[metadata]"
                      ]

                userTy <- Store.getResourceType store xactId (unsafeName "user")
                _updated <- Store.writeResource userTy (unsafeName testUsername) testPasswordHash

                pure ()

          liftIO setupStore

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
            runReaderT (executeSequential state cs) manager

          liftIO $ removeDirectoryRecursive tmpDir

data State (v :: Type -> Type)
  = State
  { stateSessionIds :: [Var ByteString v]
  , stateTransaction :: Maybe (StateTransaction v)
  , stateResourceTypes :: Map Name StateResourceType
  , stateResources :: Map Name (Map Name StateResource)
  }
  deriving (Show)

stateSessionId :: State v -> Maybe (Var ByteString v)
stateSessionId = listToMaybe . stateSessionIds

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
  deriving (Show)

data StateTransaction (v :: Type -> Type)
  = StateTransaction
  { stateTransactionId :: Var ByteString v
  , stateTransactionResourceTypes :: StateTransactionResources StateResourceType StateResourceType ()
  , stateTransactionResources ::
      StateTransactionResources (Map Name StateResource) (Map Name StateResource) (Set Name)
  }
  deriving (Show)

data StateTransactionResources create update delete
  = StateTransactionResources
  { stateTransactionResourcesCreate :: Map Name create
  , stateTransactionResourcesUpdate :: Map Name update
  , stateTransactionResourcesDelete :: Map Name delete
  }
  deriving (Show)

stateLookupResourceType ::
  Eq1 v =>
  State v ->
  -- | Transaction ID
  Maybe (Var ByteString v) ->
  -- | Resource type
  Name ->
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
  Name ->
  -- | Resource name
  Name ->
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

stateResourceTypesTransactionView :: State v -> Map Name StateResourceType
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
      Map Name StateResourceType ->
      Map Name StateResourceType ->
      Map Name StateResourceType
    insertCreatedResourceTypes created resources = Map.unionWith (\new _old -> new) created resources

    modifyUpdatedResourceTypes ::
      Map Name StateResourceType ->
      Map Name StateResourceType ->
      Map Name StateResourceType
    modifyUpdatedResourceTypes updated resources = Map.unionWith (\new _old -> new) updated resources

    removeDeletedResourceTypes ::
      Map Name () ->
      Map Name StateResourceType ->
      Map Name StateResourceType
    removeDeletedResourceTypes deleted resources =
      resources `Map.difference` deleted

stateResourcesTransactionView :: State v -> Map Name (Map Name StateResource)
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
      Map Name (Map Name StateResource) ->
      Map Name (Map Name StateResource) ->
      Map Name (Map Name StateResource)
    insertCreatedResources created resources = Map.unionWith (<>) created resources

    modifyUpdatedResources ::
      Map Name (Map Name StateResource) ->
      Map Name (Map Name StateResource) ->
      Map Name (Map Name StateResource)
    modifyUpdatedResources updated resources = Map.unionWith (<>) updated resources

    removeDeletedResources ::
      Map Name (Set Name) ->
      Map Name (Map Name StateResource) ->
      Map Name (Map Name StateResource)
    removeDeletedResources deleted resources =
      Map.differenceWith (\rs ids -> Just $ foldl' (flip Map.delete) rs ids) resources deleted

initialState :: State v
initialState =
  State
    { stateSessionIds = []
    , stateTransaction = Nothing
    , stateResourceTypes = mempty
    , stateResources = mempty
    }

notSession :: Name -> Bool
notSession resTy =
  -- `session` resource names are generated by the server, so Hedgehog wraps
  -- them in `Var`. That doesn't fit with the current resource model, so
  -- I'm just excluding them for now.
  renderName resTy /= "session"

commands ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) =>
  -- | Test username
  String ->
  -- | Test password
  String ->
  [Command gen m State]
commands username password =
  [ cLogin username password
  , cLogout
  , cBegin
  , cBeginNoauth
  , cCommit
  , cCommitNoauth
  , cRollbackNoauth
  , cTransactionListNoauth
  , cExportNoauth
  , cImportNoauth
  , cResourceTypeCreate
  , cResourceTypeCreateNoauth
  , cResourceTypeCreateDuplicate
  , cResourceTypeUpdate
  , cResourceTypeUpdateNoauth
  , cResourceTypeUpdateMissing
  , cResourceTypeList
  , cResourceTypeListNoauth
  , cResourceTypeGet GetHeaders
  , cResourceTypeGet GetUrl
  , cResourceTypeGetNoauth GetHeaders
  , cResourceTypeGetNoauth GetUrl
  , cResourceCreate
  , cResourceCreateNoauth
  , cResourceCreateMissing
  , cResourceCreateDuplicate
  , cResourceList
  , cResourceListNoauth
  , cResourceListMissing
  , cResourceGet GetHeaders
  , cResourceGet GetUrl
  , cResourceGetNoauth GetHeaders
  , cResourceGetNoauth GetUrl
  , cResourceGetMissing GetHeaders
  , cResourceGetMissing GetUrl
  , cResourceUpdateNoauth
  , cResourcePropertiesUpdateNoauth
  ]

data ResourceTypeList (v :: Type -> Type)
  = ResourceTypeList
      -- | Session ID
      (Var ByteString v)
      -- | Transaction ID
      (Maybe (Var ByteString v))
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceTypeList :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceTypeList =
  Command
    ( \state -> do
        sessionId <- stateSessionId state
        pure $
          ResourceTypeList sessionId
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
    )
    ( \(ResourceTypeList sessionId mXactId) -> do
        manager <- ask
        let resTy = "resource"
        let
          headers =
            sessionCookieHeaders (concrete sessionId)
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpGet manager ("https://localhost:8080/.resource/" ++ resTy) headers
    )
    [ Require $ \state (ResourceTypeList sessionId mXactId) ->
        Just sessionId == stateSessionId state
          && ( case mXactId of
                 Nothing -> True
                 Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
             )
    , Ensure $ \old _new (ResourceTypeList _sessionId mXactId) output -> do
        classify (fromString "resource type list (inside transaction)") $ isJust mXactId
        classify (fromString "resource type list (outside transaction)") $ isNothing mXactId

        annotateShow output
        Http.responseStatus output === ok200

        let
          resourceIds =
            fmap (ResourceId (unsafeName "resource")) $
              case mXactId of
                Nothing ->
                  Map.keys $ stateResourceTypes old
                Just{} ->
                  Map.keys $ stateResourceTypesTransactionView old

        classify (fromString "resource type list (count == 0)") $ null resourceIds
        classify (fromString "resource type list (count > 0)") $ length resourceIds > 0

        let actual = sort (fmap ByteString.Lazy.Char8.unpack . ByteString.Lazy.Char8.lines $ Http.responseBody output)
        let expected = fmap renderResourceId resourceIds
        annotateShow old
        actual === expected
    ]

data ResourceTypeListNoauth (v :: Type -> Type)
  = ResourceTypeListNoauth
      -- | Transaction ID
      (Maybe (Var ByteString v))
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceTypeListNoauth ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceTypeListNoauth =
  Command
    ( \state -> do
        pure $
          ResourceTypeListNoauth
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
    )
    ( \(ResourceTypeListNoauth mXactId) -> do
        manager <- ask
        let resTy = "resource"
        let headers = [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpGet manager ("https://localhost:8080/.resource/" ++ resTy) headers
    )
    [ Require $ \state (ResourceTypeListNoauth mXactId) ->
        ( case mXactId of
            Nothing -> True
            Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
        )
    , Ensure $ \_old _new (ResourceTypeListNoauth mXactId) output -> do
        classify (fromString "resource type list (unauthenticated)") $ isJust mXactId
        Http.responseStatus output === unauthorized401
    ]

genResourceName :: MonadGen m => m Name
genResourceName =
  unsafeName <$> Gen.list (Range.constant 1 20) genResourceNameChar
  where
    genResourceNameChar = Gen.element (['a' .. 'z'] ++ "-.")

genStateResourceType :: MonadGen m => m StateResourceType
genStateResourceType = StateResourceType <$> fmap fromString (Gen.string (Range.constant 1 20) Gen.alphaNum)

data ResourceTypeCreate (v :: Type -> Type)
  = ResourceTypeCreate
      -- | Session ID
      (Var ByteString v)
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource name
      Name
      StateResourceType
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceTypeCreate :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceTypeCreate =
  Command
    ( \state -> do
        sessionId <- stateSessionId state
        pure $
          ResourceTypeCreate sessionId
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> genResourceName
            <*> genStateResourceType
    )
    ( \(ResourceTypeCreate sessionId mXactId resName value) -> do
        manager <- ask
        let
          headers =
            sessionCookieHeaders (concrete sessionId)
              ++ [ (fromString "X-Blog-ResourceType", fromString "resource")
                 , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
                 ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $
          httpPost manager "https://localhost:8080/.resource" headers (stateResourceTypeContent value)
    )
    [ Require $ \state (ResourceTypeCreate sessionId mXactId resName _value) ->
        Just sessionId == stateSessionId state
          && ( case mXactId of
                 Nothing -> True
                 Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
             )
          && (not $ resName `Map.member` stateResourceTypesTransactionView state)
    , Update $ \state (ResourceTypeCreate _sessionId mXactId resName value) _output ->
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
    , Ensure $ \_old _new (ResourceTypeCreate _sessionId mXactId _resName value) output -> do
        label (fromString "resource type create")
        classify (fromString "resource type create (inside transaction)") $ isJust mXactId
        classify (fromString "resource type create (outside transaction)") $ isNothing mXactId

        annotateShow value
        annotateShow output
        Http.responseStatus output === created201
    ]

data ResourceTypeCreateNoauth (v :: Type -> Type)
  = ResourceTypeCreateNoauth
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource name
      Name
      StateResourceType
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceTypeCreateNoauth ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceTypeCreateNoauth =
  Command
    ( \state -> do
        pure $
          ResourceTypeCreateNoauth
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> genResourceName
            <*> genStateResourceType
    )
    ( \(ResourceTypeCreateNoauth mXactId resName value) -> do
        manager <- ask
        let
          headers =
            [ (fromString "X-Blog-ResourceType", fromString "resource")
            , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
            ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $
          httpPost manager "https://localhost:8080/.resource" headers (stateResourceTypeContent value)
    )
    [ Require $ \state (ResourceTypeCreateNoauth mXactId resName _value) ->
        ( case mXactId of
            Nothing -> True
            Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && (not $ resName `Map.member` stateResourceTypesTransactionView state)
    , Ensure $ \_old _new (ResourceTypeCreateNoauth _mXactId _resName _value) output -> do
        label (fromString "resource type create (unauthenticated)")

        Http.responseStatus output === unauthorized401
    ]

cResourceTypeCreateDuplicate ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceTypeCreateDuplicate =
  Command
    ( \state -> do
        sessionId <- stateSessionId state
        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes
        pure $
          ResourceTypeCreate
            sessionId
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> Gen.element (Map.keys resourceTypes)
            <*> genStateResourceType
    )
    ( \(ResourceTypeCreate sessionId mXactId resName value) -> do
        manager <- ask
        let
          headers =
            sessionCookieHeaders (concrete sessionId)
              ++ [ (fromString "X-Blog-ResourceType", fromString "resource")
                 , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
                 ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $
          httpPost manager "https://localhost:8080/.resource" headers (stateResourceTypeContent value)
    )
    [ Require $ \state (ResourceTypeCreate sessionId mXactId resName _value) ->
        Just sessionId == stateSessionId state
          && ( case mXactId of
                 Nothing -> True
                 Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
             )
          && (isJust $ stateLookupResourceType state mXactId resName)
    , Ensure $ \_old _new (ResourceTypeCreate _sessionId mXactId _resName _value) output -> do
        label (fromString "resource type create (duplicate)")
        classify (fromString "resource type create (duplicate) (inside transaction)") $ isJust mXactId
        classify (fromString "resource type create (duplicate) (outside transaction)") $ isNothing mXactId

        annotateShow output
        Http.responseStatus output === badRequest400
    ]

genResourceContent :: MonadGen m => StateResourceType -> m LazyByteString
genResourceContent (StateResourceType _content) =
  ByteString.Lazy.Char8.pack <$> Gen.string (Range.constant 0 1000) Gen.alphaNum

data ResourceCreate (v :: Type -> Type)
  = ResourceCreate
      -- | Session ID
      (Var ByteString v)
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource type
      Name
      -- | Resource name
      Name
      -- | Content
      LazyByteString
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceCreate :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceCreate =
  Command
    ( \state -> do
        sessionId <- stateSessionId state

        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes

        pure $
          (\mXactId (resTy, resContent) resName -> ResourceCreate sessionId mXactId resTy resName resContent)
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> ( do
                    (resTyName, resTy) <- Gen.element . Map.toList $ stateResourceTypesTransactionView state
                    (,) resTyName <$> genResourceContent resTy
                )
            <*> genResourceName
    )
    ( \(ResourceCreate sessionId mXactId resTy resName content) -> do
        manager <- ask
        let
          headers =
            sessionCookieHeaders (concrete sessionId)
              ++ [ (fromString "X-Blog-ResourceType", fromString $ renderName resTy)
                 , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
                 ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpPost manager "https://localhost:8080/.resource" headers content
    )
    [ Require $ \state (ResourceCreate sessionId mXactId resTy resName _content) ->
        Just sessionId == stateSessionId state
          && ( case mXactId of
                 Nothing ->
                   True
                 Just xactId ->
                   fmap stateTransactionId (stateTransaction state) == Just xactId
             )
          && isJust (stateLookupResourceType state mXactId resTy)
          && isNothing (stateLookupResource state mXactId resTy resName)
          && notSession resTy
    , Update $ \state (ResourceCreate _sessionId mXactId resTy resName content) _output ->
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
    , Ensure $ \_old _new (ResourceCreate _sessionId mXactId _resTy _resName content) output -> do
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
        sessionId <- stateSessionId state
        pure $
          ResourceCreate sessionId
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> genResourceName
            <*> genResourceName
            <*> pure mempty
    )
    ( \(ResourceCreate sessionId mXactId resTy resName content) -> do
        manager <- ask
        let
          headers =
            sessionCookieHeaders (concrete sessionId)
              ++ [ (fromString "X-Blog-ResourceType", fromString $ renderName resTy)
                 , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
                 ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpPost manager "https://localhost:8080/.resource" headers content
    )
    [ Require $ \state (ResourceCreate sessionId mXactId resTy _resName _content) ->
        Just sessionId == stateSessionId state
          && ( case mXactId of
                 Nothing ->
                   True
                 Just xactId ->
                   fmap stateTransactionId (stateTransaction state) == Just xactId
             )
          && isNothing (stateLookupResourceType state mXactId resTy)
    , Ensure $ \_old _new (ResourceCreate _sessionId mXactId _resTy _resName _content) output -> do
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
        sessionId <- stateSessionId state

        let nonemptyResources = Map.filter (not . null) $ stateResourcesTransactionView state
        guard . not $ null nonemptyResources
        pure $
          (\mXactId (resTy, resName) -> ResourceCreate sessionId mXactId resTy resName mempty)
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> ( do
                    (resTy, res) <- Gen.element $ Map.toList nonemptyResources
                    (resName, _resValue) <- Gen.element $ Map.toList res
                    pure (resTy, resName)
                )
    )
    ( \(ResourceCreate sessionId mXactId resTy resName content) -> do
        manager <- ask
        let
          headers =
            sessionCookieHeaders (concrete sessionId)
              ++ [ (fromString "X-Blog-ResourceType", fromString $ renderName resTy)
                 , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
                 ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpPost manager "https://localhost:8080/.resource" headers content
    )
    [ Require $ \state (ResourceCreate sessionId mXactId resTy resName _content) ->
        Just sessionId == stateSessionId state
          && ( case mXactId of
                 Nothing ->
                   True
                 Just xactId ->
                   fmap stateTransactionId (stateTransaction state) == Just xactId
             )
          && isJust (stateLookupResource state mXactId resTy resName)
    , Ensure $ \_old _new (ResourceCreate _sessionId mXactId _resTy _resName _content) output -> do
        label (fromString "resource create (duplicate)")
        classify (fromString "resource create (duplicate) (inside transaction)") $ isJust mXactId
        classify (fromString "resource create (duplicate) (outside transaction)") $ isNothing mXactId

        annotateShow output
        Http.responseStatus output === badRequest400
    ]

data ResourceCreateNoauth (v :: Type -> Type)
  = ResourceCreateNoauth
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource type
      Name
      -- | Resource name
      Name
      -- | Content
      LazyByteString
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceCreateNoauth ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceCreateNoauth =
  Command
    ( \state -> do
        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes

        pure $
          (\mXactId (resTy, resContent) resName -> ResourceCreateNoauth mXactId resTy resName resContent)
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> ( do
                    (resTyName, resTy) <- Gen.element . Map.toList $ stateResourceTypesTransactionView state
                    (,) resTyName <$> genResourceContent resTy
                )
            <*> genResourceName
    )
    ( \(ResourceCreateNoauth mXactId resTy resName content) -> do
        manager <- ask
        let
          headers =
            [ (fromString "X-Blog-ResourceType", fromString $ renderName resTy)
            , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
            ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpPost manager "https://localhost:8080/.resource" headers content
    )
    [ Require $ \state (ResourceCreateNoauth mXactId resTy resName _content) ->
        ( case mXactId of
            Nothing ->
              True
            Just xactId ->
              fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && isJust (stateLookupResourceType state mXactId resTy)
          && isNothing (stateLookupResource state mXactId resTy resName)
    , Ensure $ \_old _new (ResourceCreateNoauth _mXactId _resTy _resName _content) output -> do
        label (fromString "resource create (unauthenticated)")
        Http.responseStatus output === unauthorized401
    ]

nameToPart :: Name -> String
nameToPart = ByteString.Lazy.Char8.unpack . Builder.toLazyByteString . toEncodedUrlPiece . renderName

data ResourceList (v :: Type -> Type)
  = ResourceList
      -- | Session ID
      (Var ByteString v)
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource type
      Name
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceList :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceList =
  Command
    ( \state -> do
        sessionId <- stateSessionId state

        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes
        pure $
          ResourceList sessionId
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> Gen.element (Map.keys resourceTypes)
    )
    ( \(ResourceList sessionId mXactId resTy) -> do
        manager <- ask
        let
          headers =
            sessionCookieHeaders (concrete sessionId)
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpGet manager ("https://localhost:8080/.resource/" ++ nameToPart resTy) headers
    )
    [ Require $ \state (ResourceList sessionId mXactId resTy) ->
        Just sessionId == stateSessionId state
          && ( case mXactId of
                 Nothing ->
                   Map.member resTy (stateResourceTypes state)
                 Just xactId ->
                   fmap stateTransactionId (stateTransaction state) == Just xactId
                     && Map.member resTy (stateResourceTypesTransactionView state)
             )
          && notSession resTy
    , Ensure $ \old _new (ResourceList _sessionId mXactId resTy) output -> do
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
        sessionId <- stateSessionId state
        pure $
          ResourceList sessionId
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> genResourceName
    )
    ( \(ResourceList sessionId mXactId resTy) -> do
        manager <- ask
        let
          headers =
            sessionCookieHeaders (concrete sessionId)
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpGet manager ("https://localhost:8080/.resource/" ++ nameToPart resTy) headers
    )
    [ Require $ \state (ResourceList sessionId mXactId resTy) ->
        Just sessionId == stateSessionId state
          && ( case mXactId of
                 Nothing ->
                   True
                 Just xactId ->
                   fmap stateTransactionId (stateTransaction state) == Just xactId
             )
          && isNothing (stateLookupResourceType state mXactId resTy)
    , Ensure $ \_old _new (ResourceList _sessionId mXactId _resTy) output -> do
        classify (fromString "resource list (missing) (inside transaction)") $ isJust mXactId
        classify (fromString "resource list (missing) (outside transaction)") $ isNothing mXactId

        annotateShow output
        Http.responseStatus output === notFound404
    ]

data ResourceListNoauth (v :: Type -> Type)
  = ResourceListNoauth
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource type
      Name
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceListNoauth :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceListNoauth =
  Command
    ( \state -> do
        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes
        pure $
          ResourceListNoauth
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> Gen.element (Map.keys resourceTypes)
    )
    ( \(ResourceListNoauth mXactId resTy) -> do
        manager <- ask
        let headers = [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpGet manager ("https://localhost:8080/.resource/" ++ nameToPart resTy) headers
    )
    [ Require $ \state (ResourceListNoauth mXactId resTy) ->
        ( case mXactId of
            Nothing ->
              Map.member resTy (stateResourceTypes state)
            Just xactId ->
              fmap stateTransactionId (stateTransaction state) == Just xactId
                && Map.member resTy (stateResourceTypesTransactionView state)
        )
    , Ensure $ \_old _new (ResourceListNoauth _mXactId _resTy) output -> do
        label $ fromString "resource list (unauthenticated)"
        Http.responseStatus output === unauthorized401
    ]

data ResourceGet (v :: Type -> Type)
  = ResourceGet
      -- | Session ID
      (Var ByteString v)
      -- | Use the transaction ID
      (Maybe (Var ByteString v))
      -- | Resource type
      Name
      -- | Resource name
      Name
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceGet ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => GetStyle -> Command gen m State
cResourceGet getStyle =
  Command
    ( \state -> do
        sessionId <- stateSessionId state

        let nonemptyResources = Map.filter (not . null) $ stateResourcesTransactionView state
        guard . not $ null nonemptyResources
        pure $
          (\mXactId (resTy, resName) -> ResourceGet sessionId mXactId resTy resName)
            <$> Gen.element ([Nothing] ++ [Just $ stateTransactionId xact | Just xact <- [stateTransaction state]])
            <*> ( do
                    (resTy, res) <- Gen.element $ Map.toList nonemptyResources
                    (resName, _resValue) <- Gen.element $ Map.toList res
                    pure (resTy, resName)
                )
    )
    ( \(ResourceGet sessionId mXactId resTy resName) -> do
        manager <- ask
        case getStyle of
          GetHeaders -> do
            let
              headers =
                sessionCookieHeaders (concrete sessionId)
                  ++ [ (fromString "X-Blog-ResourceType", fromString $ renderName resTy)
                     , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
                     ]
                  ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $ httpGet manager "https://localhost:8080/.resource" headers
          GetUrl -> do
            let
              headers =
                sessionCookieHeaders (concrete sessionId)
                  ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $
              httpGet
                manager
                ("https://localhost:8080/.resource/" ++ nameToPart resTy ++ "/" ++ nameToPart resName)
                headers
    )
    [ Require $ \state (ResourceGet sessionId mXactId resTy resName) ->
        Just sessionId == stateSessionId state
          && ( case mXactId of
                 Nothing -> True
                 Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
             )
          && isJust (stateLookupResource state mXactId resTy resName)
    , Ensure $ \old _new (ResourceGet _sessionId mXactId resTyName resName) output -> do
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
        sessionId <- stateSessionId state

        let currentResources = stateResourcesTransactionView state
        let genXactId =
              Gen.element ([Nothing] ++ [Just $ stateTransactionId xact | Just xact <- [stateTransaction state]])
        pure $
          Gen.choice $
            [ResourceGet sessionId <$> genXactId <*> genResourceName <*> genResourceName]
              ++ [ ResourceGet sessionId <$> genXactId <*> Gen.element (Map.keys currentResources) <*> genResourceName
                 | not $ null currentResources
                 ]
    )
    ( \(ResourceGet sessionId mXactId resTy resName) -> do
        manager <- ask
        case getStyle of
          GetHeaders -> do
            let
              headers =
                sessionCookieHeaders (concrete sessionId)
                  ++ [ (fromString "X-Blog-ResourceType", fromString $ renderName resTy)
                     , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
                     ]
                  ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $ httpGet manager "https://localhost:8080/.resource" headers
          GetUrl -> do
            let
              headers =
                sessionCookieHeaders (concrete sessionId)
                  ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $
              httpGet
                manager
                ("https://localhost:8080/.resource/" ++ nameToPart resTy ++ "/" ++ nameToPart resName)
                headers
    )
    [ Require $ \state (ResourceGet sessionId mXactId resTy resName) ->
        Just sessionId == stateSessionId state
          && ( case mXactId of
                 Nothing -> True
                 Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
             )
          && isNothing (stateLookupResource state mXactId resTy resName)
    , Ensure $ \_old _new (ResourceGet _sessionId mXactId _resTyName _resName) output -> do
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

data ResourceGetNoauth (v :: Type -> Type)
  = ResourceGetNoauth
      -- | Use the transaction ID
      (Maybe (Var ByteString v))
      -- | Resource type
      Name
      -- | Resource name
      Name
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceGetNoauth ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => GetStyle -> Command gen m State
cResourceGetNoauth getStyle =
  Command
    ( \state -> do
        let nonemptyResources = Map.filter (not . null) $ stateResourcesTransactionView state
        guard . not $ null nonemptyResources
        pure $
          (\mXactId (resTy, resName) -> ResourceGetNoauth mXactId resTy resName)
            <$> Gen.element ([Nothing] ++ [Just $ stateTransactionId xact | Just xact <- [stateTransaction state]])
            <*> ( do
                    (resTy, res) <- Gen.element $ Map.toList nonemptyResources
                    (resName, _resValue) <- Gen.element $ Map.toList res
                    pure (resTy, resName)
                )
    )
    ( \(ResourceGetNoauth mXactId resTy resName) -> do
        manager <- ask
        case getStyle of
          GetHeaders -> do
            let
              headers =
                [ (fromString "X-Blog-ResourceType", fromString $ renderName resTy)
                , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
                ]
                  ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $ httpGet manager "https://localhost:8080/.resource" headers
          GetUrl -> do
            let
              headers =
                [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $
              httpGet
                manager
                ("https://localhost:8080/.resource/" ++ nameToPart resTy ++ "/" ++ nameToPart resName)
                headers
    )
    [ Require $ \state (ResourceGetNoauth mXactId resTy resName) ->
        ( case mXactId of
            Nothing -> True
            Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && isJust (stateLookupResource state mXactId resTy resName)
    , Ensure $ \_old _new (ResourceGetNoauth _mXactId _resTyName _resName) output -> do
        let
          getStyleDesc =
            case getStyle of
              GetHeaders -> "(via headers)"
              GetUrl -> "(via url)"
        label (fromString $ "resource get (unauthenticated) " ++ getStyleDesc)
        Http.responseStatus output === unauthorized401
    ]

data ResourceTypeUpdate (v :: Type -> Type)
  = ResourceTypeUpdate
      (Var ByteString v)
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource name
      Name
      -- | New value
      StateResourceType
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceTypeUpdate :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceTypeUpdate =
  Command
    ( \state -> do
        sessionId <- stateSessionId state

        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes

        pure $
          Gen.choice $
            [ (\mXactId resName -> ResourceTypeUpdate sessionId mXactId resName)
                <$> Gen.element
                  ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
                <*> Gen.element (Map.keys resourceTypes)
                <*> genStateResourceType
            ]
              ++ [ (\resName -> ResourceTypeUpdate sessionId (Just $ stateTransactionId transaction) resName)
                     <$> Gen.element (Map.keys newResourceTypes)
                     <*> genStateResourceType
                 | Just transaction <- [stateTransaction state]
                 , let newResourceTypes = stateTransactionResourcesCreate (stateTransactionResourceTypes transaction)
                 , not $ null newResourceTypes
                 ]
    )
    ( \(ResourceTypeUpdate sessionId mXactId resName value) -> do
        manager <- ask
        let
          headers =
            sessionCookieHeaders (concrete sessionId)
              ++ [ (fromString "X-Blog-ResourceType", fromString "resource")
                 , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
                 ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpPut manager "https://localhost:8080/.resource" headers (stateResourceTypeContent value)
    )
    [ Require $ \state (ResourceTypeUpdate sessionId mXactId resName _value) ->
        Just sessionId == stateSessionId state
          && ( case mXactId of
                 Nothing -> True
                 Just xactId ->
                   fmap stateTransactionId (stateTransaction state) == Just xactId
             )
          && isJust (stateLookupResourceType state mXactId resName)
    , Update $ \state (ResourceTypeUpdate _sessionId mXactId resName value) _output ->
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
    , Ensure $ \old _new (ResourceTypeUpdate _sessionId mXactId resName value) output -> do
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
        sessionId <- stateSessionId state
        pure $
          ResourceTypeUpdate sessionId
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> genResourceName
            <*> genStateResourceType
    )
    ( \(ResourceTypeUpdate sessionId mXactId resName value) -> do
        manager <- ask
        let
          headers =
            sessionCookieHeaders (concrete sessionId)
              ++ [ (fromString "X-Blog-ResourceType", fromString "resource")
                 , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
                 ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpPut manager "https://localhost:8080/.resource" headers (stateResourceTypeContent value)
    )
    [ Require $ \state (ResourceTypeUpdate sessionId mXactId resName _value) ->
        Just sessionId == stateSessionId state
          && ( case mXactId of
                 Nothing -> True
                 Just xactId ->
                   fmap stateTransactionId (stateTransaction state) == Just xactId
             )
          && isNothing (stateLookupResourceType state mXactId resName)
    , Ensure $ \_old _new (ResourceTypeUpdate _sessionId mXactId _resName _value) output -> do
        label (fromString "resource type update (missing)")
        classify (fromString "resource type update (missing) (inside transaction)") $ isJust mXactId
        classify (fromString "resource type update (missing) (outside transaction)") $ isNothing mXactId

        annotateShow output
        Http.responseStatus output === badRequest400
    ]

data ResourceTypeUpdateNoauth (v :: Type -> Type)
  = ResourceTypeUpdateNoauth
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource name
      Name
      -- | New value
      StateResourceType
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceTypeUpdateNoauth ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceTypeUpdateNoauth =
  Command
    ( \state -> do
        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes

        pure $
          Gen.choice $
            [ (\mXactId resName -> ResourceTypeUpdateNoauth mXactId resName)
                <$> Gen.element
                  ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
                <*> Gen.element (Map.keys resourceTypes)
                <*> genStateResourceType
            ]
              ++ [ (\resName -> ResourceTypeUpdateNoauth (Just $ stateTransactionId transaction) resName)
                     <$> Gen.element (Map.keys newResourceTypes)
                     <*> genStateResourceType
                 | Just transaction <- [stateTransaction state]
                 , let newResourceTypes = stateTransactionResourcesCreate (stateTransactionResourceTypes transaction)
                 , not $ null newResourceTypes
                 ]
    )
    ( \(ResourceTypeUpdateNoauth mXactId resName value) -> do
        manager <- ask
        let
          headers =
            [ (fromString "X-Blog-ResourceType", fromString "resource")
            , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
            ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpPut manager "https://localhost:8080/.resource" headers (stateResourceTypeContent value)
    )
    [ Require $ \state (ResourceTypeUpdateNoauth mXactId resName _value) ->
        ( case mXactId of
            Nothing -> True
            Just xactId ->
              fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && isJust (stateLookupResourceType state mXactId resName)
    , Ensure $ \_old _new (ResourceTypeUpdateNoauth _mXactId _resName _value) output -> do
        label (fromString "resource type update (unauthenticated)")
        Http.responseStatus output === unauthorized401
    ]

data GetStyle = GetHeaders | GetUrl

data ResourceTypeGet (v :: Type -> Type)
  = ResourceTypeGet
      -- | Session ID
      (Var ByteString v)
      -- | Use the transaction ID
      (Maybe (Var ByteString v))
      -- | Resource name
      Name
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceTypeGet ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => GetStyle -> Command gen m State
cResourceTypeGet getStyle =
  Command
    ( \state -> do
        sessionId <- stateSessionId state

        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes
        pure $
          ResourceTypeGet sessionId
            <$> Gen.element ([Nothing] ++ [Just $ stateTransactionId xact | Just xact <- [stateTransaction state]])
            <*> Gen.element (Map.keys resourceTypes)
    )
    ( \(ResourceTypeGet sessionId mXactId resName) -> do
        manager <- ask
        let resTy = "resource"
        case getStyle of
          GetHeaders -> do
            let
              headers =
                sessionCookieHeaders (concrete sessionId)
                  ++ [ (fromString "X-Blog-ResourceType", fromString resTy)
                     , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
                     ]
                  ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $ httpGet manager "https://localhost:8080/.resource" headers
          GetUrl -> do
            let
              headers =
                sessionCookieHeaders (concrete sessionId)
                  ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $
              httpGet manager ("https://localhost:8080/.resource/" ++ resTy ++ "/" ++ nameToPart resName) headers
    )
    [ Require $ \state (ResourceTypeGet sessionId mXactId resName) ->
        Just sessionId == stateSessionId state
          && ( case mXactId of
                 Nothing -> True
                 Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
             )
          && (isJust $ stateLookupResourceType state mXactId resName)
    , Ensure $ \old _new (ResourceTypeGet _sessionId mXactId resName) output -> do
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

data ResourceTypeGetNoauth (v :: Type -> Type)
  = ResourceTypeGetNoauth
      -- | Use the transaction ID
      (Maybe (Var ByteString v))
      -- | Resource name
      Name
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceTypeGetNoauth ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => GetStyle -> Command gen m State
cResourceTypeGetNoauth getStyle =
  Command
    ( \state -> do
        let resourceTypes = stateResourceTypesTransactionView state
        guard . not $ null resourceTypes
        pure $
          ResourceTypeGetNoauth
            <$> Gen.element ([Nothing] ++ [Just $ stateTransactionId xact | Just xact <- [stateTransaction state]])
            <*> Gen.element (Map.keys resourceTypes)
    )
    ( \(ResourceTypeGetNoauth mXactId resName) -> do
        manager <- ask
        let resTy = "resource"
        case getStyle of
          GetHeaders -> do
            let
              headers =
                [ (fromString "X-Blog-ResourceType", fromString resTy)
                , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
                ]
                  ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $ httpGet manager "https://localhost:8080/.resource" headers
          GetUrl -> do
            let headers = [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
            liftIO $
              httpGet manager ("https://localhost:8080/.resource/" ++ resTy ++ "/" ++ nameToPart resName) headers
    )
    [ Require $ \state (ResourceTypeGetNoauth mXactId resName) ->
        ( case mXactId of
            Nothing -> True
            Just xactId -> fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && (isJust $ stateLookupResourceType state mXactId resName)
    , Ensure $ \_old _new (ResourceTypeGetNoauth _mXactId _resName) output -> do
        let
          getStyleDesc =
            case getStyle of
              GetHeaders -> "(via headers)"
              GetUrl -> "(via url)"
        label (fromString $ "resource type get (unauthorised) " ++ getStyleDesc)
        Http.responseStatus output === unauthorized401
    ]

data Login (v :: Type -> Type)
  = Login
  deriving (Show, Generic, FunctorB, TraversableB)

cLogin ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) =>
  -- | Test username
  String ->
  -- | Test password
  String ->
  Command gen m State
cLogin username password =
  Command
    (\_state -> Just $ pure Login)
    ( \Login -> do
        manager <- ask
        let headers = [(fromString "Content-Type", fromString "application/x-www-form-urlencoded")]
        response <-
          liftIO $
            httpPostWithCookies manager [] "https://localhost:8080/.login" headers $
              LazyByteString.intercalate
                (fromString "&")
                [fromString $ "username=" ++ username, fromString $ "password=" ++ password]

        let status = Http.responseStatus response
        unless (status == ok200) . error $ "unexpected response status: " ++ show status

        cookie <-
          maybe
            (error $ "response missing session ID cookie: " ++ show (Http.responseCookieJar response))
            pure
            . find ((fromString sessionIdCookieName ==) . Http.cookie_name)
            . Http.destroyCookieJar
            $ Http.responseCookieJar response

        pure $ Http.cookie_value cookie
    )
    [ Update $ \state Login output ->
        state{stateSessionIds = output : stateSessionIds state}
    , Ensure $ \_old _new _cmd _output -> do
        label $ fromString "login"
    ]

data Logout (v :: Type -> Type)
  = Logout
      -- | Session ID
      (Var ByteString v)
  deriving (Show, Generic, FunctorB, TraversableB)

cLogout ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) =>
  Command gen m State
cLogout =
  Command
    ( \state -> do
        guard . not . null $ stateSessionIds state
        pure $
          Logout <$> Gen.element (stateSessionIds state)
    )
    ( \(Logout sessionId) -> do
        manager <- ask
        let headers = sessionCookieHeaders (concrete sessionId)
        liftIO $ httpPost manager "https://localhost:8080/.logout" headers mempty
    )
    [ Require $ \state (Logout sessionId) ->
        sessionId `elem` stateSessionIds state
    , Update $ \state (Logout sessionId) _output ->
        state{stateSessionIds = filter (/= sessionId) (stateSessionIds state)}
    , Ensure $ \_old _new _cmd output -> do
        label $ fromString "logout"

        Http.responseStatus output === ok200
    ]

sessionCookieHeaders :: ByteString -> RequestHeaders
sessionCookieHeaders sessionId = [(fromString "Cookie", fromString (sessionIdCookieName ++ "=") <> sessionId)]

data Begin (v :: Type -> Type)
  = Begin
      -- | Session ID
      (Var ByteString v)
  deriving (Show, Generic, FunctorB, TraversableB)

cBegin :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cBegin =
  Command
    ( \state -> do
        sessionId <- stateSessionId state
        pure $
          pure $
            Begin sessionId
    )
    ( \(Begin sessionId) -> do
        manager <- ask
        let headers = sessionCookieHeaders $ concrete sessionId
        response <- liftIO $ httpPost manager "https://localhost:8080/.transaction/begin" headers mempty
        pure . LazyByteString.toStrict $ Http.responseBody response
    )
    [ Require $ \state (Begin sessionId) ->
        Just sessionId == stateSessionId state
          && isNothing (stateTransaction state)
    , Update $ \state (Begin _sessionId) output ->
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

data BeginNoauth (v :: Type -> Type)
  = BeginNoauth
  deriving (Show, Generic, FunctorB, TraversableB)

cBeginNoauth :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cBeginNoauth =
  Command
    (\_state -> Just $ pure BeginNoauth)
    ( \BeginNoauth -> do
        manager <- ask
        let headers = []
        response <- liftIO $ httpPost manager "https://localhost:8080/.transaction/begin" headers mempty
        pure response
    )
    [ Require $ \state BeginNoauth ->
        isNothing (stateTransaction state)
    , Ensure $ \_old _new _cmd output -> do
        label $ fromString "begin (unauthenticated)"

        Http.responseStatus output === unauthorized401
    ]

data Commit (v :: Type -> Type)
  = Commit
      -- | Session ID
      (Var ByteString v)
      -- | Transaction ID
      (Var ByteString v)
  deriving (Show, Generic, FunctorB, TraversableB)

cCommit :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cCommit =
  Command
    ( \state -> do
        sessionId <- stateSessionId state
        xactId <- stateTransactionId <$> stateTransaction state
        pure $ pure (Commit sessionId xactId)
    )
    ( \(Commit sessionId xactId) -> do
        manager <- ask
        let
          headers =
            sessionCookieHeaders (concrete sessionId)
              ++ [(fromString "X-Blog-TransactionId", concrete xactId)]
        liftIO $ httpPost manager "https://localhost:8080/.transaction/commit" headers mempty
    )
    [ Require $ \state (Commit sessionId _) ->
        Just sessionId == stateSessionId state
          && isJust (stateTransaction state)
    , Update $ \state (Commit _sessionId _xactId) _output -> stateCommit state
    , Ensure $ \_old _new (Commit _sessionId _xactId) output -> do
        label $ fromString "commit"
        annotateShow output
        Http.responseStatus output === ok200
    ]

data CommitNoauth (v :: Type -> Type)
  = CommitNoauth
      -- | Transaction ID
      (Var ByteString v)
  deriving (Show, Generic, FunctorB, TraversableB)

cCommitNoauth :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cCommitNoauth =
  Command
    ( \state -> do
        xactId <- stateTransactionId <$> stateTransaction state
        pure $ pure (CommitNoauth xactId)
    )
    ( \(CommitNoauth xactId) -> do
        manager <- ask
        let headers = [(fromString "X-Blog-TransactionId", concrete xactId)]
        liftIO $ httpPost manager "https://localhost:8080/.transaction/commit" headers mempty
    )
    [ Require $ \state (CommitNoauth _) ->
        isJust (stateTransaction state)
    , Ensure $ \_old _new (CommitNoauth _xactId) output -> do
        label $ fromString "commit (unauthorised)"
        Http.responseStatus output === unauthorized401
    ]

data RollbackNoauth (v :: Type -> Type)
  = RollbackNoauth
      -- | Transaction ID
      (Var ByteString v)
  deriving (Show, Generic, FunctorB, TraversableB)

cRollbackNoauth :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cRollbackNoauth =
  Command
    ( \state -> do
        xactId <- stateTransactionId <$> stateTransaction state
        pure $ pure (RollbackNoauth xactId)
    )
    ( \(RollbackNoauth xactId) -> do
        manager <- ask
        let headers = [(fromString "X-Blog-TransactionId", concrete xactId)]
        liftIO $ httpPost manager "https://localhost:8080/.transaction/rollback" headers mempty
    )
    [ Require $ \state (RollbackNoauth _xactId) ->
        isJust (stateTransaction state)
    , Ensure $ \_old _new (RollbackNoauth _xactId) output -> do
        label $ fromString "rollback (unauthenticated)"
        Http.responseStatus output === unauthorized401
    ]

data TransactionListNoauth (v :: Type -> Type)
  = TransactionListNoauth
  deriving (Show, Generic, FunctorB, TraversableB)

cTransactionListNoauth ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cTransactionListNoauth =
  Command
    (\_state -> Just $ pure TransactionListNoauth)
    ( \TransactionListNoauth -> do
        manager <- ask
        liftIO $ httpGet manager "https://localhost:8080/.transaction" []
    )
    [ Ensure $ \_old _new TransactionListNoauth output -> do
        label $ fromString "transaction list (unauthenticated)"
        Http.responseStatus output === unauthorized401
    ]

data ExportNoauth (v :: Type -> Type)
  = ExportNoauth
  deriving (Show, Generic, FunctorB, TraversableB)

cExportNoauth :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cExportNoauth =
  Command
    (\_state -> Just $ pure ExportNoauth)
    ( \ExportNoauth -> do
        manager <- ask
        liftIO $ httpGet manager "https://localhost:8080/.export" []
    )
    [ Ensure $ \_old _new ExportNoauth output -> do
        label $ fromString "export (unauthenticated)"
        Http.responseStatus output === unauthorized401
    ]

data ImportNoauth (v :: Type -> Type)
  = ImportNoauth
  deriving (Show, Generic, FunctorB, TraversableB)

cImportNoauth :: (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cImportNoauth =
  Command
    (\_state -> Just $ pure ImportNoauth)
    ( \ImportNoauth -> do
        manager <- ask
        -- 1024 zero bytes is an empty tar archive, so the only thing wrong with
        -- this request is that it is unauthenticated.
        liftIO $ httpPut manager "https://localhost:8080/.import" [] (LazyByteString.replicate 1024 0)
    )
    [ Ensure $ \_old _new ImportNoauth output -> do
        label $ fromString "import (unauthenticated)"
        Http.responseStatus output === unauthorized401
    ]

data ResourceUpdateNoauth (v :: Type -> Type)
  = ResourceUpdateNoauth
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource type
      Name
      -- | Resource name
      Name
      -- | Content
      LazyByteString
  deriving (Show, Generic, FunctorB, TraversableB)

cResourceUpdateNoauth ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourceUpdateNoauth =
  Command
    ( \state -> do
        let nonemptyResources = Map.filter (not . null) $ stateResourcesTransactionView state
        guard . not $ null nonemptyResources
        pure $
          (\mXactId (resTy, resName) content -> ResourceUpdateNoauth mXactId resTy resName content)
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> ( do
                    (resTy, res) <- Gen.element $ Map.toList nonemptyResources
                    (resName, _resValue) <- Gen.element $ Map.toList res
                    pure (resTy, resName)
                )
            <*> ( do
                    resTy <- Gen.element . Map.elems $ stateResourceTypesTransactionView state
                    genResourceContent resTy
                )
    )
    ( \(ResourceUpdateNoauth mXactId resTy resName content) -> do
        manager <- ask
        let
          headers =
            [ (fromString "X-Blog-ResourceType", fromString $ renderName resTy)
            , (fromString "X-Blog-ResourceName", fromString $ renderName resName)
            ]
              ++ [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $ httpPut manager "https://localhost:8080/.resource" headers content
    )
    [ Require $ \state (ResourceUpdateNoauth mXactId resTy resName _content) ->
        ( case mXactId of
            Nothing ->
              True
            Just xactId ->
              fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && isJust (stateLookupResource state mXactId resTy resName)
    , Ensure $ \_old _new (ResourceUpdateNoauth _mXactId _resTy _resName _content) output -> do
        label $ fromString "resource update (unauthenticated)"
        Http.responseStatus output === unauthorized401
    ]

data ResourcePropertiesUpdateNoauth (v :: Type -> Type)
  = ResourcePropertiesUpdateNoauth
      -- | Transaction ID
      (Maybe (Var ByteString v))
      -- | Resource type
      Name
      -- | Resource name
      Name
  deriving (Show, Generic, FunctorB, TraversableB)

cResourcePropertiesUpdateNoauth ::
  (MonadGen gen, MonadReader Http.Manager m, MonadIO m) => Command gen m State
cResourcePropertiesUpdateNoauth =
  Command
    ( \state -> do
        let nonemptyResources = Map.filter (not . null) $ stateResourcesTransactionView state
        guard . not $ null nonemptyResources
        pure $
          (\mXactId (resTy, resName) -> ResourcePropertiesUpdateNoauth mXactId resTy resName)
            <$> Gen.element
              ([Nothing] ++ [Just (stateTransactionId transaction) | Just transaction <- [stateTransaction state]])
            <*> ( do
                    (resTy, res) <- Gen.element $ Map.toList nonemptyResources
                    (resName, _resValue) <- Gen.element $ Map.toList res
                    pure (resTy, resName)
                )
    )
    ( \(ResourcePropertiesUpdateNoauth mXactId resTy resName) -> do
        manager <- ask
        let headers = [(fromString "X-Blog-TransactionId", concrete xactId) | Just xactId <- [mXactId]]
        liftIO $
          httpPatch
            manager
            ( "https://localhost:8080/.resource/"
                ++ nameToPart resTy
                ++ "/"
                ++ nameToPart resName
                ++ "/property"
            )
            headers
            (fromString "title = \"unauthenticated\"\n")
    )
    [ Require $ \state (ResourcePropertiesUpdateNoauth mXactId resTy resName) ->
        ( case mXactId of
            Nothing ->
              True
            Just xactId ->
              fmap stateTransactionId (stateTransaction state) == Just xactId
        )
          && isJust (stateLookupResource state mXactId resTy resName)
    , Ensure $ \_old _new (ResourcePropertiesUpdateNoauth _mXactId _resTy _resName) output -> do
        label $ fromString "resource properties update (unauthenticated)"
        Http.responseStatus output === unauthorized401
    ]
