{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Blog
  ( MetadataValue (..)
  , Name
  , ResourceId (..)
  , cfgContentType
  , mkName
  , renderName
  , renderResourceId
  , unsafeName
  )
import qualified Blog.Build as Build
import Blog.Diagnostic (DiagnosticReports (..), renderDiagnosticReports)
import Blog.Error (sageErrorReport, tomlErrorReport)
import Blog.ID (ID)
import qualified Blog.ID as ID
import Blog.Metadata (metadataValueFromToml)
import qualified Blog.Route
import qualified Blog.Rules
import Blog.Session (sessionIdCookieName)
import Blog.Store (Store)
import qualified Blog.Store as Store
import Control.Applicative ((<**>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVar, readTVar, readTVarIO)
import Control.Exception (evaluate)
import Control.Monad (unless, when, (<=<))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, onException)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Crypto.Argon2 (Argon2Status (..))
import qualified Crypto.Argon2 as Argon2
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.Char as Char
import Data.Foldable (for_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified Data.Text.Short as ShortText
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM, rfc822DateFormat)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Traversable (for)
import Network.HTTP.Types.Header (RequestHeaders, hContentType, hLastModified, hSetCookie)
import Network.HTTP.Types.Status
  ( badRequest400
  , created201
  , internalServerError500
  , methodNotAllowed405
  , notFound404
  , notImplemented501
  , ok200
  , preconditionFailed412
  , unauthorized401
  , unsupportedMediaType415
  )
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Options.Applicative as Options
import System.Directory
  ( createDirectoryIfMissing
  )
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stdout)
import qualified Text.Sage as Sage
import qualified Toml
import Web.Cookie (parseCookies)
import Web.FormUrlEncoded (Form (..), urlDecodeAsForm)

data Cli
  = Cli
  { cliData :: !FilePath
  -- ^ Data directory
  , cliCert :: !FilePath
  -- ^ TLS certificate
  , cliKey :: !FilePath
  -- ^ TLS key
  , cliPort :: !Int
  -- ^ Port
  }

cliParser :: Options.Parser Cli
cliParser =
  Cli
    <$> Options.strOption
      (Options.long "data" <> Options.metavar "DIR" <> Options.help "Server data directory")
    <*> Options.strOption
      (Options.long "cert" <> Options.metavar "FILE" <> Options.help "TLS certificate file")
    <*> Options.strOption (Options.long "key" <> Options.metavar "FILE" <> Options.help "TLS key file")
    <*> Options.option
      Options.auto
      (Options.long "port" <> Options.metavar "PORT" <> Options.help "Server port")

initStore :: FilePath -> IO (Store (ExceptT DiagnosticReports IO))
initStore data_ = do
  createDirectoryIfMissing True data_
  createDirectoryIfMissing False $ data_ </> "resource"
  Store.fromDirectory data_

data Routes
  = Routes
  { routesActive :: TVar (Blog.Route.Routes ResourceId)
  , routesPending :: TVar (Map Store.TransactionId (Blog.Route.Routes ResourceId))
  }

readActiveRoutes :: Routes -> IO (Blog.Route.Routes ResourceId)
readActiveRoutes = readTVarIO . routesActive

beginRoutes :: Routes -> Store.TransactionId -> IO ()
beginRoutes routes xactId = atomically $ modifyTVar (routesPending routes) (Map.insert xactId Blog.Route.empty)

commitRoutes :: Routes -> Store.TransactionId -> IO ()
commitRoutes routes xactId = atomically $ do
  routes' <- readTVar $ routesPending routes
  case Map.lookup xactId routes' of
    Nothing -> pure ()
    Just routes'' -> do
      modifyTVar (routesActive routes) (routes'' <>)
      modifyTVar (routesPending routes) (Map.delete xactId)

rollbackRoutes :: Routes -> Store.TransactionId -> IO ()
rollbackRoutes routes xactId = atomically $ modifyTVar (routesPending routes) (Map.delete xactId)

insertRoute :: Store.TransactionId -> [Text] -> ResourceId -> Routes -> IO ()
insertRoute xactId path value routes =
  atomically $
    modifyTVar (routesPending routes) (Map.insertWith (<>) xactId (Blog.Route.singleton path value))

initRoutes :: Store (ExceptT DiagnosticReports IO) -> IO Routes
initRoutes store = do
  routesVar <- atomically $ Routes <$> newTVar Blog.Route.empty <*> newTVar mempty

  result <- runExceptT . withTransaction store routesVar Nothing $ \xactId _defer -> do
    mResTy <- Store.lookupResourceType store xactId (unsafeName "route")
    case mResTy of
      Nothing -> do
        liftIO $ putStrLn "info: resource type 'route' missing (starting with empty routes)"
      Just resTy -> do
        entries <- Store.listResource resTy
        for_ entries $ \entry -> do
          mContent <- Store.readResource resTy (resourceName entry)
          content <- maybe (error $ "missing " ++ renderResourceId entry) pure mContent
          Blog.Route.RouteEntry path resId <-
            case Sage.parse (Blog.Route.routeEntryParser <* Sage.eof) content of
              Right x ->
                pure x
              Left err ->
                throwError $
                  DiagnosticReports
                    (fromString $ renderResourceId entry)
                    (LazyByteString.fromStrict content)
                    (sageErrorReport err)
          liftIO $ insertRoute xactId path resId routesVar

  case result of
    Right () ->
      pure routesVar
    Left err -> do
      ByteString.Lazy.Char8.putStrLn $ renderDiagnosticReports err
      exitFailure

main :: IO ()
main = do
  cli <- Options.execParser $ Options.info (cliParser <**> Options.helper) Options.fullDesc

  hSetBuffering stdout LineBuffering

  store <- initStore $ cliData cli
  routes <- initRoutes store

  let tlsSettings = WarpTLS.tlsSettings (cliCert cli) (cliKey cli)

  let
    startup = do
      putStrLn $ "Running at https://localhost:" ++ show (cliPort cli)
      putStrLn $ "  Data directory: " ++ cliData cli

    settings =
      Warp.setPort (cliPort cli) $
        Warp.setBeforeMainLoop startup $
          Warp.defaultSettings

  WarpTLS.runTLS tlsSettings settings $ app store routes

newtype HandlerT m a = HandlerT (ExceptT Wai.Response m a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadTrans
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadError Wai.Response
    )

handleT ::
  MonadIO m =>
  (Wai.Response -> IO Wai.ResponseReceived) -> HandlerT m Wai.Response -> m Wai.ResponseReceived
handleT respond (HandlerT ma) = liftIO . either respond respond =<< runExceptT ma

handleExceptT ::
  Monad n => (forall x. m x -> HandlerT n x) -> ExceptT DiagnosticReports m a -> HandlerT n a
handleExceptT f ma = do
  result <- f $ runExceptT ma
  case result of
    Left err ->
      throwError $ Wai.responseLBS badRequest400 [] (renderDiagnosticReports err)
    Right a ->
      pure a

requireHeader :: (MonadError Wai.Response m, MonadIO m) => RequestHeaders -> String -> m ByteString
requireHeader headers headerName = do
  case lookup (fromString headerName) headers of
    Nothing ->
      throwError $
        Wai.responseLBS
          badRequest400
          []
          (fromString $ "missing header: " ++ headerName)
    Just headerValue ->
      pure headerValue

optionalTransactionIdHeader ::
  MonadError Wai.Response m => RequestHeaders -> m (Maybe Store.TransactionId)
optionalTransactionIdHeader headers =
  case lookup (fromString "X-Blog-TransactionId") headers of
    Nothing ->
      pure Nothing
    Just xactId ->
      Just <$> parseTransactionId (ByteString.Char8.unpack xactId)

parseTransactionId :: MonadError Wai.Response m => String -> m Store.TransactionId
parseTransactionId value =
  case Store.parseTransactionId value of
    Nothing ->
      throwError $
        Wai.responseLBS badRequest400 [] (fromString $ "invalid transaction ID: " ++ show value)
    Just x -> pure x

withTransaction ::
  (MonadError DiagnosticReports m, MonadMask m, MonadIO m) =>
  Store m ->
  Routes ->
  Maybe Store.TransactionId ->
  (Store.TransactionId -> Bool -> m a) ->
  m a
withTransaction store routes Nothing f =
  Store.bracketTransaction
    (liftIO . beginRoutes routes)
    (liftIO . commitRoutes routes)
    (liftIO . rollbackRoutes routes)
    store
    False
    (\xactId -> f xactId False)
withTransaction store _routes (Just xactId) f = do
  transaction <-
    maybe
      (throwError . DiagnosticSimple $ "transaction not found: " ++ Store.renderTransactionId xactId)
      pure
      =<< Store.lookupTransaction store xactId
  f xactId $ Store.xactDefer transaction

getRouteEntry ::
  MonadError DiagnosticReports m => Store.ResourceType m -> ResourceId -> m Blog.Route.RouteEntry
getRouteEntry resTy resId = do
  mContent <- Store.readResource resTy (resourceName resId)
  content <- maybe (error $ "missing " ++ renderResourceId resId) pure mContent
  case Sage.parse (Blog.Route.routeEntryParser <* Sage.eof) content of
    Right x ->
      pure x
    Left err ->
      throwError $
        DiagnosticReports
          (fromString $ renderResourceId resId)
          (LazyByteString.fromStrict content)
          (sageErrorReport err)

evalRules ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  Store m ->
  Routes ->
  Store.TransactionId ->
  [ResourceId] ->
  m (Map ResourceId Build.Change)
evalRules store routesVar xactId resIds = do
  changes <- Build.evalRules putStrLn store xactId Blog.Rules.rules resIds
  mRouteResTy <- Store.lookupResourceType store xactId (unsafeName "route")
  -- TODO: not sure if this is a good idea
  case mRouteResTy of
    Nothing ->
      liftIO . putStrLn $
        "warning: missing 'route' resource type (rules will not result in route updates)"
    Just routeResTy ->
      for_ (Map.toList changes) $ \(changedId, Build.Change status _reasons) ->
        case renderName $ resourceType changedId of
          "route" -> do
            case status of
              Build.Created -> do
                Blog.Route.RouteEntry path value <- getRouteEntry routeResTy changedId
                liftIO $ insertRoute xactId path value routesVar
              Build.Updated -> do
                Blog.Route.RouteEntry path value <- getRouteEntry routeResTy changedId
                liftIO $ insertRoute xactId path value routesVar
          _ -> pure ()
  pure changes

lookupSessionCookie :: Wai.Request -> Maybe ByteString
lookupSessionCookie request = do
  cookies <- lookup (fromString "Cookie") $ Wai.requestHeaders request
  lookup (fromString sessionIdCookieName) $ parseCookies cookies

newtype AuthenticatedUser = AuthenticatedUser String

authenticate ::
  (MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m AuthenticatedUser
authenticate store routesVar request = do
  let store' = Store.hoistStore (hoist lift) store
  handleExceptT id . withTransaction store' routesVar Nothing $ \xactId _defer -> do
    sessionTy <- do
      mSessionTy <- Store.lookupResourceType store' xactId (unsafeName "session")
      case mSessionTy of
        Nothing -> do
          liftIO . putStrLn $ "warning: no 'session' resource type (refusing authentication)"
          lift . throwError $
            Wai.responseLBS
              notImplemented501
              []
              (fromString $ "error: server has no 'session' resource type")
        Just x -> pure x

    value <- maybe authenticationRequired pure $ lookupSessionCookie request

    sessionId <-
      case mkName $ ByteString.Char8.unpack value of
        Nothing -> invalidSession
        Just x -> pure x
    exists <- Store.doesResourceExist sessionTy sessionId
    if exists
      then do
        let
          getStringProperty key = do
            mValue <- Store.lookupProperty sessionTy sessionId key
            case mValue of
              Just (VString x) -> pure x
              Just x -> do
                liftIO . putStrLn $
                  "error: "
                    ++ renderResourceId (ResourceId (Store.resourceTypeName sessionTy) sessionId)
                    ++ ":"
                    ++ renderName key
                    ++ " is not a string (got "
                    ++ show x
                    ++ ")"
                invalidSession
              Nothing -> do
                liftIO . putStrLn $
                  "error: "
                    ++ renderResourceId (ResourceId (Store.resourceTypeName sessionTy) sessionId)
                    ++ " is missing '"
                    ++ renderName key
                    ++ "' property"
                invalidSession

        expires <- do
          expires <- getStringProperty (unsafeName "expires")
          case iso8601ParseM $ Text.unpack expires of
            Nothing -> do
              liftIO . putStrLn $
                "error: failed to parse "
                  ++ renderResourceId (ResourceId (Store.resourceTypeName sessionTy) sessionId)
                  ++ ":expires as a datetime (got "
                  ++ show expires
                  ++ ")"
              invalidSession
            Just x -> pure (x :: UTCTime)

        now <- liftIO getCurrentTime
        if now >= expires
          then invalidSession
          else do
            user <- getStringProperty (unsafeName "user")
            pure . AuthenticatedUser $ Text.unpack user
      else invalidSession
  where
    authenticationRequired =
      lift . throwError $
        Wai.responseLBS unauthorized401 [] (fromString "authentication required")

    invalidSession =
      lift . throwError $
        Wai.responseLBS unauthorized401 [] (fromString "invalid session ID")

app ::
  Store (ExceptT DiagnosticReports IO) ->
  Routes ->
  Wai.Application
app store routesVar request respond = do
  handleT respond $
    case Wai.pathInfo request of
      [part] | part == fromString ".login" ->
        case ByteString.Char8.unpack $ Wai.requestMethod request of
          "POST" -> httpLogin store routesVar request
          _ -> methodNotAllowed
      [part] | part == fromString ".logout" ->
        case ByteString.Char8.unpack $ Wai.requestMethod request of
          "POST" -> httpLogout store routesVar request
          _ -> methodNotAllowed
      part : parts | part == fromString ".resource" -> do
        _user <- authenticate store routesVar request
        case parts of
          [] ->
            case ByteString.Char8.unpack $ Wai.requestMethod request of
              "GET" -> httpResourceGet store routesVar request
              "POST" -> httpResourceCreate store routesVar request
              "PUT" -> httpResourceUpdate store routesVar request
              _ -> methodNotAllowed
          [resTyName] ->
            case ByteString.Char8.unpack $ Wai.requestMethod request of
              "GET" -> do
                resTyName' <- requireName $ Text.unpack resTyName
                httpResourceTypeList store routesVar request resTyName'
              "REFRESH" -> do
                resTyName' <- requireName $ Text.unpack resTyName
                httpResourceTypeRefresh store routesVar request resTyName'
              _ -> methodNotAllowed
          [resTyName, resName] ->
            case ByteString.Char8.unpack $ Wai.requestMethod request of
              "GET" -> do
                resName' <- requireName $ Text.unpack resName
                resTyName' <- requireName $ Text.unpack resTyName
                httpResourceLookup store routesVar request resTyName' resName'
              _ -> methodNotAllowed
          [resTyName, resName, part']
            | part' == fromString "metadata" ->
                case ByteString.Char8.unpack $ Wai.requestMethod request of
                  "GET" -> do
                    resName' <- requireName $ Text.unpack resName
                    resTyName' <- requireName $ Text.unpack resTyName
                    httpResourceMetadataLookup store routesVar request resTyName' resName'
                  _ -> methodNotAllowed
            | part' == fromString "property" -> do
                case ByteString.Char8.unpack $ Wai.requestMethod request of
                  "PATCH" -> do
                    resName' <- requireName $ Text.unpack resName
                    resTyName' <- requireName $ Text.unpack resTyName
                    httpResourcePropertiesUpdate store routesVar request resTyName' resName'
                  _ -> methodNotAllowed
          [resTyName, resName, part', propName]
            | part' == fromString "property" -> do
                case ByteString.Char8.unpack $ Wai.requestMethod request of
                  "GET" -> do
                    resName' <- requireName $ Text.unpack resName
                    resTyName' <- requireName $ Text.unpack resTyName
                    propName' <- requireName $ Text.unpack propName
                    httpResourcePropertyLookup store routesVar request resTyName' resName' propName'
                  _ -> methodNotAllowed
          _ ->
            throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
      part : parts | part == fromString ".transaction" -> do
        _user <- authenticate store routesVar request
        case parts of
          [] ->
            case ByteString.Char8.unpack $ Wai.requestMethod request of
              "GET" -> httpTransactionList store
              _ -> methodNotAllowed
          [action] ->
            case Text.unpack action of
              "begin" ->
                case ByteString.Char8.unpack $ Wai.requestMethod request of
                  "POST" -> httpTransactionBegin store routesVar request
                  _ -> methodNotAllowed
              "commit" -> do
                case ByteString.Char8.unpack $ Wai.requestMethod request of
                  "POST" -> httpTransactionCommit store routesVar request
                  _ -> methodNotAllowed
              "rollback" -> do
                case ByteString.Char8.unpack $ Wai.requestMethod request of
                  "POST" -> httpTransactionRollback store routesVar request
                  _ -> methodNotAllowed
              _ -> throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
          _ ->
            throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
      [part]
        | part == fromString ".export" -> do
            _user <- authenticate store routesVar request
            case ByteString.Char8.unpack $ Wai.requestMethod request of
              "GET" -> httpExport store routesVar request
              _ -> methodNotAllowed
      [part]
        | part == fromString ".import" -> do
            _user <- authenticate store routesVar request
            case ByteString.Char8.unpack $ Wai.requestMethod request of
              "PUT" -> httpImport store routesVar request
              _ -> methodNotAllowed
      path -> do
        case ByteString.Char8.unpack $ Wai.requestMethod request of
          "GET" -> httpRouteGet store routesVar request path
          _ -> methodNotAllowed
  where
    methodNotAllowed = throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")

requireName :: MonadError Wai.Response m => String -> m Name
requireName n =
  case mkName n of
    Nothing ->
      throwError $
        Wai.responseLBS
          badRequest400
          []
          (fromString $ "invalid resource [type] name '" ++ n ++ "'")
    Just x -> pure x

httpResourceGet ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpResourceGet store routesVar request = do
  let headers = Wai.requestHeaders request

  resTyName <-
    requireName
      <=< fmap ByteString.Char8.unpack . requireHeader headers
      $ fromString "X-Blog-ResourceType"
  resName <-
    requireName
      <=< fmap ByteString.Char8.unpack . requireHeader headers
      $ fromString "X-Blog-ResourceName"
  mXactId <- optionalTransactionIdHeader headers

  mBody <- handleExceptT lift . withTransaction store routesVar mXactId $ \xactId _defer -> do
    resTy <- Store.getResourceType store xactId resTyName
    Store.readResource resTy resName
  case mBody of
    Just body ->
      pure $ Wai.responseLBS ok200 [] (LazyByteString.fromStrict body)
    Nothing ->
      throwError $
        Wai.responseLBS
          badRequest400
          []
          (fromString $ "resource " ++ renderResourceId (ResourceId resTyName resName) ++ " does not exist")

renderChangeList :: Map ResourceId Build.Change -> [LazyByteString]
renderChangeList changes =
  fmap
    (\(changedId, change) -> fromString "* " <> fromString (Build.renderChange changedId change))
    (Map.toList changes)

sessionDuration :: NominalDiffTime
sessionDuration = 30 * nominalDay

createSession ::
  Monad m =>
  Store.ResourceType m ->
  -- | Session ID
  ID ->
  -- | Username
  Name ->
  -- | Expires
  UTCTime ->
  m ()
createSession sessionTy sessionId username expires = do
  let sessionId' = ID.toString sessionId
  _updated <- Store.writeResource sessionTy (unsafeName sessionId') mempty

  Store.setProperty sessionTy (unsafeName sessionId') (unsafeName "user") $
    VString . fromString $
      renderName username

  Store.setProperty sessionTy (unsafeName sessionId') (unsafeName "expires") $
    VString (fromString $ iso8601Show expires)

httpLogin ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpLogin store routesVar request = do
  let contentType = "application/x-www-form-urlencoded"
  case lookup (fromString "Content-Type") $ Wai.requestHeaders request of
    Just value | value == fromString contentType -> pure ()
    _ ->
      throwError $
        Wai.responseLBS
          unsupportedMediaType415
          []
          (fromString $ "error: unsupported Content-Type (expected " ++ contentType ++ ")")

  result <- liftIO $ urlDecodeAsForm <$> Wai.consumeRequestBodyLazy request
  case result of
    Left err ->
      throwError $
        Wai.responseLBS
          badRequest400
          []
          (Text.Lazy.Encoding.encodeUtf8 $ LazyText.fromStrict err)
    Right (Form form) -> do
      let
        getFormField key =
          case Map.lookup (fromString key) form of
            Nothing ->
              throwError $
                Wai.responseLBS
                  badRequest400
                  []
                  (fromString $ "error: missing field '" ++ key ++ "'")
            Just [] ->
              throwError $
                Wai.responseLBS
                  badRequest400
                  []
                  (fromString $ "error: not enough values for '" ++ key ++ "'")
            Just (_ : _ : _) ->
              throwError $
                Wai.responseLBS
                  badRequest400
                  []
                  (fromString $ "error: too many values for '" ++ key ++ "'")
            Just [value] ->
              pure value

      username <- requireName . Text.unpack =<< getFormField "username"
      password <- getFormField "password"

      handleExceptT lift . withTransaction store routesVar Nothing $ \xactId _defer -> do
        let
          missingResource resTyName = do
            liftIO . putStrLn $ "warning: no '" ++ resTyName ++ "' resource type (skipping login)"
            pure $
              Wai.responseLBS
                notImplemented501
                []
                (fromString $ "error: server has no '" ++ resTyName ++ "' resource type")

        mUserTy <- Store.lookupResourceType store xactId (unsafeName "user")
        case mUserTy of
          Nothing -> missingResource "user"
          Just userTy -> do
            mSessionTy <- Store.lookupResourceType store xactId (unsafeName "session")
            case mSessionTy of
              Nothing -> missingResource "session"
              Just sessionTy -> do
                let
                  authenticationFailure =
                    Wai.responseLBS
                      badRequest400
                      []
                      (fromString "error: invalid username/password")

                mUser <- Store.readResource userTy username
                case mUser of
                  Nothing -> pure authenticationFailure
                  Just hashInfo ->
                    case ShortText.fromByteString hashInfo of
                      Nothing -> do
                        liftIO . putStrLn $ "error: ShortText.fromByteString failed on " ++ show hashInfo
                        pure $
                          Wai.responseLBS
                            internalServerError500
                            []
                            (fromString "error: internal server error")
                      Just hashInfo' -> do
                        case Argon2.verifyEncoded hashInfo' (Text.Encoding.encodeUtf8 password) of
                          Argon2Ok -> do
                            now <- liftIO getCurrentTime

                            do
                              -- Clean up expired sessions on login, rather than
                              -- setting up a recurring task.

                              sessions <- Store.listResource sessionTy
                              for_ sessions $ \session -> do
                                expires <- Store.lookupProperty sessionTy (resourceName session) (unsafeName "expires")

                                let
                                  expired
                                    | Just (VString expires') <- expires
                                    , Just expires'' <- iso8601ParseM (Text.unpack expires') =
                                        now >= expires''
                                    | otherwise = False
                                when expired $ Store.removeResource sessionTy (resourceName session)

                            sessionId <- liftIO ID.generate

                            let expires = addUTCTime sessionDuration now
                            createSession sessionTy sessionId username expires

                            pure $
                              Wai.responseLBS
                                ok200
                                [
                                  ( hSetCookie
                                  , fromString $
                                      sessionIdCookieName
                                        ++ "="
                                        ++ ID.toString sessionId
                                        ++ "; Secure; HttpOnly; SameSite=Strict; Path=/; Max-Age="
                                        ++ show (truncate sessionDuration :: Int)
                                  )
                                ]
                                (fromString "logged in")
                          err -> do
                            unless (err == Argon2VerifyMismatch) $ do
                              liftIO . putStrLn $ "error: Argon2.verifyEncoded failed: " ++ show err
                            pure authenticationFailure

httpLogout ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpLogout store routesVar request = do
  case lookupSessionCookie request of
    Nothing ->
      loggedOut
    Just value ->
      case ID.fromString $ ByteString.Char8.unpack value of
        Nothing ->
          invalidSessionId
        Just sessionId -> do
          handleExceptT lift . withTransaction store routesVar Nothing $ \xactId _defer -> do
            mSessionTy <- Store.lookupResourceType store xactId (unsafeName "session")
            for_ mSessionTy $ \sessionTy ->
              Store.removeResource sessionTy (unsafeName $ ID.toString sessionId)

          loggedOut
  where
    loggedOut = pure $ Wai.responseLBS ok200 [] (fromString "logged out")
    invalidSessionId = pure $ Wai.responseLBS badRequest400 [] (fromString "invalid session ID")

httpResourceCreate ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpResourceCreate store routesVar request = do
  let headers = Wai.requestHeaders request

  resTyName <-
    requireName
      <=< fmap (ByteString.Char8.unpack) . requireHeader headers
      $ fromString "X-Blog-ResourceType"
  resName <-
    requireName
      <=< fmap ByteString.Char8.unpack . requireHeader headers
      $ fromString "X-Blog-ResourceName"
  let resId = ResourceId resTyName resName
  mXactId <- optionalTransactionIdHeader headers

  handleExceptT lift . withTransaction store routesVar mXactId $ \xactId defer -> do
    resTy <- Store.getResourceType store xactId resTyName
    exists <- Store.doesResourceExist resTy resName
    if exists
      then
        throwError . DiagnosticSimple $
          "resource " ++ renderResourceId (ResourceId resTyName resName) ++ " already exists"
      else do
        body <- liftIO $ Wai.consumeRequestBodyLazy request
        _changed <- Store.writeResource resTy resName body
        if defer
          then
            pure $
              Wai.responseLBS
                created201
                []
                ( ByteString.Lazy.Char8.unlines
                    [ fromString ("created " ++ renderResourceId (ResourceId resTyName resName))
                    , fromString "(rules deferred)"
                    ]
                )
          else do
            changes <- evalRules store routesVar xactId [resId]
            pure $
              Wai.responseLBS
                created201
                []
                ( ByteString.Lazy.Char8.unlines $
                    fromString ("created " ++ renderResourceId (ResourceId resTyName resName))
                      : renderChangeList changes
                )

httpResourceUpdate ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpResourceUpdate store routesVar request = do
  let headers = Wai.requestHeaders request

  resTyName <-
    requireName
      <=< fmap ByteString.Char8.unpack
      $ requireHeader headers "X-Blog-ResourceType"
  resName <-
    requireName
      <=< fmap ByteString.Char8.unpack
      $ requireHeader headers "X-Blog-ResourceName"
  let resId = ResourceId resTyName resName
  mXactId <- optionalTransactionIdHeader headers

  mLocalModificationTime <- do
    let mValue = ByteString.Char8.unpack <$> lookup (fromString "If-Unmodified-Since") headers
    case mValue of
      Nothing -> pure Nothing
      Just value ->
        case parseTimeM False defaultTimeLocale rfc822DateFormat value of
          Nothing ->
            throwError $
              Wai.responseLBS
                badRequest400
                []
                (fromString "If-Unmodified-Since: invalid date format")
          Just x -> pure $ Just (x :: UTCTime)

  handleExceptT lift . withTransaction store routesVar mXactId $ \xactId defer -> do
    resTy <- Store.getResourceType store xactId resTyName
    mServerModificationTime <- Store.readResourceModificationTime resTy resName
    case mServerModificationTime of
      Just serverModificationTime -> do
        let
          responseHeaders =
            [ (hLastModified, fromString $ formatTime defaultTimeLocale rfc822DateFormat serverModificationTime)
            ]
        if maybe True (serverModificationTime <=) mLocalModificationTime
          then do
            body <- liftIO $ Wai.consumeRequestBodyLazy request
            changed <- Store.writeResource resTy resName body

            if defer
              then
                pure $
                  Wai.responseLBS
                    ok200
                    responseHeaders
                    ( ByteString.Lazy.Char8.unlines
                        [ fromString ("updated " ++ renderResourceId (ResourceId resTyName resName))
                        , fromString "(rules deferred)"
                        ]
                    )
              else do
                changes <- evalRules store routesVar xactId [resId | changed]
                pure $
                  Wai.responseLBS
                    ok200
                    responseHeaders
                    ( ByteString.Lazy.Char8.unlines $
                        fromString
                          ( "updated "
                              ++ renderResourceId (ResourceId resTyName resName)
                              ++ if changed then "" else " (nothing changed)"
                          )
                          : renderChangeList changes
                    )
          else
            pure . Wai.responseLBS preconditionFailed412 [] . fromString $
              "the local copy of the resource is out of date"
      Nothing ->
        pure . Wai.responseLBS badRequest400 [] . fromString $
          "resource " ++ renderResourceId (ResourceId resTyName resName) ++ " does not exist"

httpResourceTypeList ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  Name ->
  HandlerT m Wai.Response
httpResourceTypeList store routesVar request resTyName = do
  mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

  let store' = Store.hoistStore lift store
  mItems <- handleExceptT lift . runMaybeT . withTransaction store' routesVar mXactId $ \xactId _defer -> do
    resTy <- MaybeT $ Store.lookupResourceType store xactId resTyName
    lift $ Store.listResource resTy

  case mItems of
    Nothing ->
      pure $ Wai.responseLBS notFound404 [] (fromString "resource not found")
    Just items ->
      pure $
        Wai.responseLBS
          ok200
          []
          (foldMap ((<> fromString "\n") . fromString . renderResourceId) items)

httpResourceTypeRefresh ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  Name ->
  HandlerT m Wai.Response
httpResourceTypeRefresh store routesVar request resTyName = do
  mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

  handleExceptT lift . withTransaction store routesVar mXactId $ \xactId defer -> do
    mResTy <- Store.lookupResourceType store xactId resTyName
    case mResTy of
      Nothing ->
        pure $ Wai.responseLBS notFound404 [] (fromString "resource not found")
      Just resTy -> do
        entries <- Store.listResource resTy
        changes <- evalRules store routesVar xactId entries

        pure $
          Wai.responseLBS
            ok200
            []
            ( ByteString.Lazy.Char8.unlines $
                fromString
                  ("refreshed " ++ renderName resTyName ++ ":*" ++ if defer then " (ignoring defer)" else "")
                  : renderChangeList changes
            )

httpTransactionList :: Monad m => Store (ExceptT DiagnosticReports m) -> HandlerT m Wai.Response
httpTransactionList store = do
  xactIds <- handleExceptT lift $ Store.listTransactions store
  pure . Wai.responseLBS ok200 [] . fromString $
    foldMap ((++ "\n") . Store.renderTransactionId) xactIds

httpTransactionBegin ::
  MonadIO m => Store (ExceptT DiagnosticReports m) -> Routes -> Wai.Request -> HandlerT m Wai.Response
httpTransactionBegin store routesVar request = do
  defer <-
    case lookup (fromString "X-Blog-Transaction-Defer") (Wai.requestHeaders request) of
      Nothing -> pure False
      Just value -> do
        case fmap Char.toLower $ ByteString.Char8.unpack value of
          "true" -> pure True
          "false" -> pure False
          _ ->
            throwError
              . Wai.responseLBS badRequest400 []
              $ fromString "invalid X-Blog-Transaction-Defer value: " <> LazyByteString.fromStrict value

  xactId <- handleExceptT lift $ Store.beginTransaction store defer
  liftIO $ beginRoutes routesVar xactId
  pure $ Wai.responseLBS ok200 [] (fromString $ Store.renderTransactionId xactId)

httpTransactionCommit ::
  (MonadCatch m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) -> Routes -> Wai.Request -> HandlerT m Wai.Response
httpTransactionCommit store routesVar request = do
  let headers = Wai.requestHeaders request
  xactId <- do
    value <- fmap ByteString.Char8.unpack . requireHeader headers $ fromString "X-Blog-TransactionId"
    parseTransactionId value
  handleExceptT lift $ do
    transaction <-
      maybe (throwError $ DiagnosticSimple "transaction not found") pure
        =<< Store.lookupTransaction store xactId
    let
      commit = do
        Store.commitTransaction store xactId
        liftIO $ commitRoutes routesVar xactId
    if Store.xactDefer transaction
      then do
        changes <- do
          let resIds = fmap Store.xactChangeId (Store.xactChanges transaction)
          Store.saveDeferred store xactId
          evalRules store routesVar xactId resIds `onException` Store.restoreDeferred store xactId
        commit
        pure . Wai.responseLBS ok200 [] . ByteString.Lazy.Char8.unlines $
          fromString "resource changes:"
            : ( renderChangeList changes
                  ++ [mempty, fromString ("committed " ++ Store.renderTransactionId xactId)]
              )
      else do
        commit
        pure . Wai.responseLBS ok200 [] . fromString $ "committed " ++ Store.renderTransactionId xactId

httpTransactionRollback ::
  MonadIO m => Store (ExceptT DiagnosticReports m) -> Routes -> Wai.Request -> HandlerT m Wai.Response
httpTransactionRollback store routesVar request = do
  let headers = Wai.requestHeaders request
  xactId <- do
    value <-
      fmap ByteString.Char8.unpack . requireHeader headers $
        fromString "X-Blog-TransactionId"
    parseTransactionId value
  handleExceptT lift $ Store.rollbackTransaction store xactId
  liftIO $ rollbackRoutes routesVar xactId
  pure $ Wai.responseLBS ok200 [] (fromString $ "rolled back " ++ Store.renderTransactionId xactId)

httpExport ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpExport store routesVar request = do
  mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request
  content <-
    handleExceptT lift . withTransaction store routesVar mXactId $ \xactId _defer -> do
      content <- Store.export store xactId

      -- If I don't force `content` here then I get a "thread
      -- blocked indefinitely on MVar" error when
      -- `Wai.responseLBS` tries to write out the result. Why?
      -- This prevents me from streaming the archive out.
      --
      -- TODO: fix this
      _ <- liftIO . evaluate $ LazyByteString.length content

      pure content

  now <- liftIO getCurrentTime
  let
    headers =
      [ (fromString "Content-Type", fromString "application/tar")
      ,
        ( fromString "Content-Disposition"
        , fromString $
            "attachment; filename=\"" ++ formatTime defaultTimeLocale "%FT%H:%M:%SZ" now ++ "-blog-export.tar\""
        )
      ]
  pure $ Wai.responseLBS ok200 headers content

httpImport ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpImport store routesVar request = do
  mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request
  handleExceptT lift . withTransaction store routesVar mXactId $ \xactId defer -> do
    imported <- Store.import_ store xactId =<< liftIO (Wai.consumeRequestBodyLazy request)
    if defer
      then do
        let
          response =
            fromString "imported resources:\n"
              <> foldMap (\resId -> fromString $ "* " <> renderResourceId resId <> "\n") imported
              <> fromString "(rules deferred)"

        pure $ Wai.responseLBS ok200 [] response
      else do
        changes <- evalRules store routesVar xactId imported

        let
          response =
            ByteString.Lazy.Char8.unlines $
              fromString "imported resources:"
                : ( fmap (\resId -> fromString $ "* " <> renderResourceId resId) imported
                      <> if null changes
                        then []
                        else
                          [mempty, fromString ("resource changes:")]
                            <> renderChangeList changes
                  )

        pure $ Wai.responseLBS ok200 [] response

httpResourceLookup ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  -- | Resource type name
  Name ->
  -- | Resource name
  Name ->
  HandlerT m Wai.Response
httpResourceLookup store routesVar request resTyName resName = do
  mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

  let store' = Store.hoistStore lift store
  mBody <- handleExceptT lift . runMaybeT . withTransaction store' routesVar mXactId $ \xactId _defer -> do
    resTy <- MaybeT $ Store.lookupResourceType store xactId resTyName
    MaybeT $ Store.readResource resTy resName

  case mBody of
    Nothing -> throwError $ Wai.responseLBS notFound404 [] (fromString "resource not found")
    Just body -> pure $ Wai.responseLBS ok200 [] (LazyByteString.fromStrict body)

httpResourceMetadataLookup ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  -- | Resource type name
  Name ->
  -- | Resource name
  Name ->
  HandlerT m Wai.Response
httpResourceMetadataLookup store routesVar request resTyName resName = do
  mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

  mBody <- handleExceptT lift . withTransaction store routesVar mXactId $ \xactId _defer -> do
    resTy <- Store.getResourceType store xactId resTyName
    Store.readProperty resTy resName (unsafeName "metadata")

  case mBody of
    Nothing -> throwError $ Wai.responseLBS notFound404 [] (fromString "metadata not found")
    Just body -> pure $ Wai.responseLBS ok200 [] (LazyByteString.fromStrict body)

httpResourcePropertiesUpdate ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  -- | Resource type name
  Name ->
  -- | Resource name
  Name ->
  HandlerT m Wai.Response
httpResourcePropertiesUpdate store routesVar request resTyName resName = do
  mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

  properties <- handleExceptT lift $ do
    body <- liftIO $ Wai.consumeRequestBodyLazy request
    Toml.Toml (Toml.Located _offset properties) nonKeys <-
      case Toml.parse $ LazyByteString.toStrict body of
        Left err -> do
          let resourceId = renderResourceId (ResourceId resTyName resName)
          throwError
            . DiagnosticReports
              (fromString $ "(" ++ resourceId ++ ":properties)")
              body
            $ tomlErrorReport err
        Right x -> pure x

    unless (null nonKeys) . error $
      "TODO: non-key-value properties: " ++ show nonKeys

    pure properties

  properties' <-
    for properties $ \(name, Toml.TomlKeyEntry _offset (Toml.Located _offset' value)) -> do
      name' <- requireName $ Text.unpack name
      pure (name', value)

  handleExceptT lift . withTransaction store routesVar mXactId $ \xactId defer -> do
    resTy <- Store.getResourceType store xactId resTyName
    let resId = ResourceId resTyName resName
    names <- for properties' $ \(name, value) -> do
      Store.setProperty
        resTy
        (resourceName resId)
        name
        (metadataValueFromToml value)
      pure name

    if defer
      then do
        let
          response =
            fromString "updated properties:\n"
              <> foldMap (\propName -> fromString $ "* " <> renderName propName <> "\n") names
              <> fromString "(rules deferred)"

        pure $ Wai.responseLBS ok200 [] response
      else do
        changes <- evalRules store routesVar xactId [resId]

        let
          response =
            ByteString.Lazy.Char8.unlines $
              fromString "updated properties:"
                : ( fmap (\propName -> fromString $ "* " <> renderName propName <> "\n") names
                      ++ if null changes
                        then []
                        else
                          [mempty, fromString ("resource changes:")]
                            ++ renderChangeList changes
                  )

        pure $ Wai.responseLBS ok200 [] response

httpResourcePropertyLookup ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  -- | Resource type name
  Name ->
  -- | Resource name
  Name ->
  -- | Property name
  Name ->
  HandlerT m Wai.Response
httpResourcePropertyLookup store routesVar request resTyName resName propName = do
  mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

  mBody <- handleExceptT lift . withTransaction store routesVar mXactId $ \xactId _defer -> do
    resTy <- Store.getResourceType store xactId resTyName
    Store.readProperty resTy resName propName

  case mBody of
    Nothing ->
      pure $ Wai.responseLBS notFound404 [] (fromString "property not found")
    Just body ->
      pure $ Wai.responseLBS ok200 [] (LazyByteString.fromStrict body)

httpRouteGet ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  -- | Path
  [Text] ->
  HandlerT m Wai.Response
httpRouteGet store routesVar request path = do
  mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

  handleExceptT lift . withTransaction store routesVar mXactId $ \xactId _defer -> do
    routes <- liftIO $ readActiveRoutes routesVar
    case Blog.Route.lookup path routes of
      Nothing ->
        pure $ Wai.responseLBS notFound404 [] (fromString "not found")
      Just resId -> do
        resTy <- Store.getResourceType store xactId $ resourceType resId
        mContent <- Store.readResource resTy $ resourceName resId
        case mContent of
          Nothing ->
            pure $ Wai.responseLBS notFound404 [] (fromString "not found")
          Just content -> do
            let contentType = Text.Encoding.encodeUtf8 . cfgContentType $ Store.resourceTypeConfig resTy
            pure $ Wai.responseLBS ok200 [(hContentType, contentType)] (LazyByteString.fromStrict content)
