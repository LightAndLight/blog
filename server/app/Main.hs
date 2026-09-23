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
import Blog.Log (LogT, MonadLog, runLogT, (.=))
import qualified Blog.Log as Log
import Blog.Metadata (metadataValueFromToml)
import qualified Blog.Route
import qualified Blog.Rules
import Blog.Session (sessionIdCookieName)
import Blog.Store (Store)
import qualified Blog.Store as Store
import Blog.Time (renderUTCTime)
import Control.Applicative (optional, (<**>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVar, readTVar, readTVarIO)
import Control.Exception (evaluate)
import Control.Monad (unless, when, (<=<))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, onException)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
import Data.Time.Clock
  ( NominalDiffTime
  , UTCTime
  , addUTCTime
  , diffUTCTime
  , getCurrentTime
  , nominalDay
  )
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM, rfc822DateFormat)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Traversable (for)
import Network.HTTP.Types.Header (Header, RequestHeaders, hContentType, hLastModified, hSetCookie)
import Network.HTTP.Types.Status
  ( badRequest400
  , created201
  , internalServerError500
  , methodNotAllowed405
  , notFound404
  , notImplemented501
  , ok200
  , preconditionFailed412
  , statusCode
  , unauthorized401
  , unsupportedMediaType415
  )
import Network.HTTP.Types.URI (renderQuery)
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
  , cliCommand :: !Command
  }

data Command
  = Run
      -- | TLS configuration
      !(Maybe Tls)
      -- | Port
      !Int
  | Restore
      -- | Archive path
      FilePath

data Tls
  = Tls
  { tlsCert :: !FilePath
  -- ^ TLS certificate
  , tlsKey :: !FilePath
  -- ^ TLS key
  }

cliParser :: Options.Parser Cli
cliParser =
  Cli
    <$> Options.strOption
      (Options.long "data" <> Options.metavar "DIR" <> Options.help "Server data directory")
    <*> Options.hsubparser
      ( Options.command "run" (Options.info runParser Options.fullDesc)
          <> Options.command "restore" (Options.info restoreParser Options.fullDesc)
      )
  where
    runParser =
      Run
        <$> optional
          ( Tls
              <$> Options.strOption
                (Options.long "cert" <> Options.metavar "FILE" <> Options.help "TLS certificate file")
              <*> Options.strOption (Options.long "key" <> Options.metavar "FILE" <> Options.help "TLS key file")
          )
        <*> Options.option
          Options.auto
          (Options.long "port" <> Options.metavar "PORT" <> Options.help "Server port")

    restoreParser =
      Restore
        <$> Options.strArgument (Options.metavar "FILE" <> Options.help "Archive from which to restore")

initStore :: FilePath -> IO (Store (ExceptT DiagnosticReports (LogT IO)))
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

initRoutes :: Store (ExceptT DiagnosticReports (LogT IO)) -> LogT IO Routes
initRoutes store = do
  routesVar <- liftIO . atomically $ Routes <$> newTVar Blog.Route.empty <*> newTVar mempty

  result <- runExceptT . withTransaction store routesVar Nothing $ \xactId _defer -> do
    mResTy <- Store.lookupResourceType store (Just xactId) (unsafeName "route")
    case mResTy of
      Nothing ->
        Log.scope (fromString "init-routes") $ do
          Log.attach (fromString "missing-resource") "route"
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
    Left err -> liftIO $ do
      ByteString.Lazy.Char8.putStrLn $ renderDiagnosticReports err
      exitFailure

main :: IO ()
main = do
  cli <- Options.execParser $ Options.info (cliParser <**> Options.helper) Options.fullDesc

  hSetBuffering stdout LineBuffering

  store <- initStore $ cliData cli
  routesVar <- runLogT ByteString.Lazy.Char8.putStrLn $ initRoutes store

  case cliCommand cli of
    Run mTls port -> do
      let mTlsSettings = fmap (\tls -> WarpTLS.tlsSettings (tlsCert tls) (tlsKey tls)) mTls

      let
        startup = runLogT ByteString.Lazy.Char8.putStrLn $ do
          Log.scope (fromString "startup-complete") $ do
            Log.attach
              (fromString "url")
              (maybe "http" (const "https") mTlsSettings ++ "://localhost:" ++ show port)
            Log.attach (fromString "data-directory") (cliData cli)

        settings =
          Warp.setPort port $
            Warp.setBeforeMainLoop startup $
              Warp.defaultSettings

      case mTlsSettings of
        Nothing ->
          Warp.runSettings settings $ app store routesVar
        Just tlsSettings ->
          WarpTLS.runTLS tlsSettings settings $ app store routesVar
    Restore archivePath -> do
      let
        reportError :: MonadIO m => ExceptT DiagnosticReports m () -> m ()
        reportError ma = do
          result <- runExceptT ma
          case result of
            Left err -> liftIO $ do
              ByteString.Lazy.Char8.putStrLn $ renderDiagnosticReports err
              exitFailure
            Right () -> pure ()

      runLogT (const $ pure ()) . reportError . withTransaction store routesVar Nothing $ \xactId _defer -> do
        content <- liftIO $ LazyByteString.readFile archivePath
        imported <- Store.import_ store xactId content
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

        liftIO $ ByteString.Lazy.Char8.putStrLn response

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
    , MonadLog
    )

handleT ::
  (MonadLog m, MonadIO m) =>
  (Wai.Response -> IO Wai.ResponseReceived) ->
  HandlerT m Wai.Response ->
  m Wai.ResponseReceived
handleT respond (HandlerT ma) = do
  response <- either id id <$> runExceptT ma
  Log.scope (fromString "response") $ do
    Log.attach (fromString "status") (statusCode $ Wai.responseStatus response)
  liftIO $ respond response

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
  Monad m =>
  Store (ExceptT DiagnosticReports m) ->
  RequestHeaders ->
  HandlerT m (Maybe Store.TransactionId)
optionalTransactionIdHeader store headers =
  traverse (requireTransactionId store) $ lookup (fromString "X-Blog-TransactionId") headers

requireTransactionId ::
  Monad m =>
  Store (ExceptT DiagnosticReports m) ->
  ByteString ->
  HandlerT m Store.TransactionId
requireTransactionId store value =
  case Store.parseTransactionId $ ByteString.Char8.unpack value of
    Nothing ->
      invalidTransactionId
    Just xactId -> do
      exists <- handleDiagnosticReports $ Store.doesTransactionExist store xactId
      unless exists invalidTransactionId
      pure xactId
  where
    invalidTransactionId =
      throwError $
        Wai.responseLBS
          badRequest400
          []
          (fromString $ "invalid transaction ID: " ++ show value)

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
  (MonadLog m, MonadError DiagnosticReports m, MonadIO m) =>
  Store m ->
  Routes ->
  Store.TransactionId ->
  [ResourceId] ->
  m (Map ResourceId Build.Change)
evalRules store routesVar xactId resIds =
  Log.scope (fromString "eval-rules") $ do
    start <- liftIO getCurrentTime
    changes <- Build.evalRules store xactId Blog.Rules.rules resIds

    Log.attach
      (fromString "changes")
      [ Log.object
          [ fromString "resource-id" .= renderResourceId resId
          , fromString "change"
              .= let Build.Change status _reasons = change
                 in case status of
                      Build.Created -> "created"
                      Build.Updated -> "updated"
          ]
      | (resId, change) <- Map.toList changes
      ]

    mRouteResTy <- Store.lookupResourceType store (Just xactId) (unsafeName "route")
    -- TODO: not sure if this is a good idea
    case mRouteResTy of
      Nothing ->
        Log.attach (fromString "missing-resource") "route"
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

    end <- liftIO getCurrentTime
    Log.attach (fromString "duration-ms") (truncate (1000 * diffUTCTime end start) :: Int)

    pure changes

lookupSessionCookie :: Wai.Request -> Maybe ByteString
lookupSessionCookie request = do
  cookies <- lookup (fromString "Cookie") $ Wai.requestHeaders request
  lookup (fromString sessionIdCookieName) $ parseCookies cookies

handleDiagnosticReports :: Monad m => ExceptT DiagnosticReports m a -> HandlerT m a
handleDiagnosticReports ma = do
  result <- lift $ runExceptT ma
  case result of
    Left err ->
      throwError $ Wai.responseLBS badRequest400 [] (renderDiagnosticReports err)
    Right a ->
      pure a

newtype AuthenticatedUser = AuthenticatedUser String

authenticate ::
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Wai.Request ->
  HandlerT m AuthenticatedUser
authenticate store request =
  Log.scope (fromString "authentication") $ do
    sessionTy <- do
      mSessionTy <-
        handleDiagnosticReports $ Store.lookupResourceType store Nothing (unsafeName "session")
      case mSessionTy of
        Nothing -> do
          Log.scope (fromString "failure") $ do
            Log.attach (fromString "missing-resource") "session"

          throwError $
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
    exists <- handleDiagnosticReports $ Store.doesResourceExist sessionTy sessionId
    if exists
      then do
        let
          getStringProperty key = do
            mValue <- handleDiagnosticReports $ Store.lookupProperty sessionTy sessionId key
            case mValue of
              Just (VString x) ->
                pure x
              Just x -> do
                Log.scope (fromString "failure") $ do
                  Log.attach (fromString "function") "getStringProperty"
                  Log.attach (fromString "property") (renderName key)
                  Log.scope (fromString "error") $ do
                    Log.attach (fromString "expected-type") "string"
                    Log.attach (fromString "actual-value") (show x)

                invalidSession
              Nothing -> do
                Log.scope (fromString "failure") $ do
                  Log.attach (fromString "function") "getStringProperty"
                  Log.attach (fromString "property") (renderName key)
                  Log.scope (fromString "error") $ do
                    Log.attach (fromString "description") "missing property"

                invalidSession

        expires <- do
          expires <- getStringProperty (unsafeName "expires")
          case iso8601ParseM $ Text.unpack expires of
            Nothing -> do
              Log.scope (fromString "failure") $ do
                Log.attach (fromString "function") "iso8601ParseM"
                Log.attach (fromString "input") expires
                Log.attach (fromString "property") "expires"
                Log.scope (fromString "error") $ do
                  Log.attach (fromString "description") "failed to parse as a datetime"

              invalidSession
            Just x -> pure (x :: UTCTime)

        now <- liftIO getCurrentTime
        if now >= expires
          then invalidSession
          else do
            user <- getStringProperty (unsafeName "user")
            Log.attach (fromString "username") user

            userName <-
              case mkName $ Text.unpack user of
                Nothing -> do
                  Log.scope (fromString "failure") $ do
                    Log.attach (fromString "function") "mkName"
                    Log.scope (fromString "resource") $ do
                      Log.attach (fromString "type") (renderName $ Store.resourceTypeName sessionTy)
                      Log.attach (fromString "name") (renderName sessionId)
                    Log.scope (fromString "error") $ do
                      Log.attach (fromString "message") "invalid username"

                  invalidSession
                Just x -> pure x

            mUserTy <- handleDiagnosticReports $ Store.lookupResourceType store Nothing (unsafeName "user")
            case mUserTy of
              Nothing -> do
                Log.scope (fromString "failure") $ do
                  Log.attach (fromString "missing-resource") "user"
                throwError $
                  Wai.responseLBS
                    notImplemented501
                    []
                    (fromString $ "error: server has no 'user' resource type")
              Just userTy -> do
                userExists <- handleDiagnosticReports $ Store.doesResourceExist userTy userName
                if userExists
                  then pure . AuthenticatedUser $ renderName userName
                  else invalidSession
      else invalidSession
  where
    authenticationRequired =
      throwError $
        Wai.responseLBS unauthorized401 [] (fromString "authentication required")

    invalidSession =
      throwError $
        Wai.responseLBS unauthorized401 [] (fromString "invalid session ID")

app ::
  Store (ExceptT DiagnosticReports (LogT IO)) ->
  Routes ->
  Wai.Application
app store routesVar request respond = do
  runLogT (liftIO . ByteString.Lazy.Char8.putStrLn) . handleT respond $ do
    Log.scope (fromString "request") $ do
      Log.attach (fromString "path") (foldMap (fromString "/" <>) $ Wai.pathInfo request)
      Log.attach
        (fromString "query")
        (ByteString.Char8.unpack . renderQuery True $ Wai.queryString request)
      Log.attach (fromString "method") (ByteString.Char8.unpack $ Wai.requestMethod request)
      Log.attach
        (fromString "user-agent")
        (ByteString.Char8.unpack <$> Wai.requestHeaderUserAgent request)
      Log.attach
        (fromString "body-length")
        ( case Wai.requestBodyLength request of
            Wai.ChunkedBody -> Log.toJSON "chunked"
            Wai.KnownLength len -> Log.toJSON len
        )

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
        _user <- authenticate store request
        case parts of
          [] ->
            case ByteString.Char8.unpack $ Wai.requestMethod request of
              "GET" -> httpResourceGet store request
              "POST" -> httpResourceCreate store routesVar request
              "PUT" -> httpResourceUpdate store routesVar request
              _ -> methodNotAllowed
          [resTyName] ->
            case ByteString.Char8.unpack $ Wai.requestMethod request of
              "GET" -> do
                resTyName' <- requireName $ Text.unpack resTyName
                httpResourceTypeList store request resTyName'
              "REFRESH" -> do
                resTyName' <- requireName $ Text.unpack resTyName
                httpResourceTypeRefresh store routesVar request resTyName'
              _ -> methodNotAllowed
          [resTyName, resName] ->
            case ByteString.Char8.unpack $ Wai.requestMethod request of
              "GET" -> do
                resName' <- requireName $ Text.unpack resName
                resTyName' <- requireName $ Text.unpack resTyName
                httpResourceLookup store request resTyName' resName'
              _ -> methodNotAllowed
          [resTyName, resName, part']
            | part' == fromString "metadata" ->
                case ByteString.Char8.unpack $ Wai.requestMethod request of
                  "GET" -> do
                    resName' <- requireName $ Text.unpack resName
                    resTyName' <- requireName $ Text.unpack resTyName
                    httpResourceMetadataLookup store request resTyName' resName'
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
                    httpResourcePropertyLookup store request resTyName' resName' propName'
                  _ -> methodNotAllowed
          _ ->
            throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
      part : parts | part == fromString ".transaction" -> do
        _user <- authenticate store request
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
            _user <- authenticate store request
            case ByteString.Char8.unpack $ Wai.requestMethod request of
              "GET" -> httpExport store request
              _ -> methodNotAllowed
      [part]
        | part == fromString ".import" -> do
            _user <- authenticate store request
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
  Wai.Request ->
  HandlerT m Wai.Response
httpResourceGet store request = do
  let headers = Wai.requestHeaders request

  resTyName <-
    requireName
      <=< fmap ByteString.Char8.unpack . requireHeader headers
      $ fromString "X-Blog-ResourceType"
  resName <-
    requireName
      <=< fmap ByteString.Char8.unpack . requireHeader headers
      $ fromString "X-Blog-ResourceName"
  mXactId <- optionalTransactionIdHeader store headers

  mBody <- handleDiagnosticReports $ do
    resTy <- Store.getResourceType store mXactId resTyName
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

sessionCookie ::
  {-| Session ID

  'Nothing' clears the cookie.
  -}
  Maybe ID ->
  -- | @Max-Age@, in seconds
  Int ->
  Header
sessionCookie value maxAge =
  ( hSetCookie
  , fromString $
      sessionIdCookieName
        ++ "="
        ++ foldMap ID.toString value
        ++ "; Secure; HttpOnly; SameSite=Strict; Path=/; Max-Age="
        ++ show maxAge
  )

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

  _updated <-
    Store.setProperty sessionTy (unsafeName sessionId') (unsafeName "user") $
      VString . fromString $
        renderName username

  _updated <-
    Store.setProperty sessionTy (unsafeName sessionId') (unsafeName "expires") $
      VString (fromString $ iso8601Show expires)

  pure ()

httpLogin ::
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpLogin store routesVar request =
  Log.scope (fromString "endpoint.login") $ do
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
        Log.attach (fromString "username") (renderName username)

        password <- getFormField "password"

        handleDiagnosticReports . withTransaction store routesVar Nothing $ \xactId _defer -> do
          let
            missingResource resTyName = do
              Log.attach (fromString "missing-resource") resTyName
              pure $
                Wai.responseLBS
                  notImplemented501
                  []
                  (fromString $ "error: server has no '" ++ resTyName ++ "' resource type")

          mUserTy <- Store.lookupResourceType store (Just xactId) (unsafeName "user")
          case mUserTy of
            Nothing -> missingResource "user"
            Just userTy -> do
              mSessionTy <- Store.lookupResourceType store (Just xactId) (unsafeName "session")
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
                          Log.scope (fromString "get-user-hash") $ do
                            Log.attach (fromString "function") "ShortText.fromBytestring"
                            Log.attach (fromString "error") "failure"

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
                                  [sessionCookie (Just sessionId) (truncate sessionDuration)]
                                  (fromString "logged in")
                            err -> do
                              unless (err == Argon2VerifyMismatch) $ do
                                Log.scope (fromString "verify-user-hash") $ do
                                  Log.attach (fromString "function") "Argon2.verifyEncoded"
                                  Log.attach (fromString "error") $ show err

                              pure authenticationFailure

httpLogout ::
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpLogout store routesVar request =
  Log.scope (fromString "endpoint.logout") $ do
    for_ (ID.fromString . ByteString.Char8.unpack =<< lookupSessionCookie request) $ \sessionId ->
      handleDiagnosticReports . withTransaction store routesVar Nothing $ \xactId _defer -> do
        mSessionTy <- Store.lookupResourceType store (Just xactId) (unsafeName "session")
        for_ mSessionTy $ \sessionTy -> do
          let sessionName = unsafeName $ ID.toString sessionId
          exists <- Store.doesResourceExist sessionTy sessionName
          when exists $ Store.removeResource sessionTy sessionName

    pure $ Wai.responseLBS ok200 [sessionCookie Nothing 0] (fromString "logged out")

httpResourceCreate ::
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpResourceCreate store routesVar request =
  Log.scope (fromString "endpoint.resource-create") $ do
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
    mXactId <- optionalTransactionIdHeader store headers

    handleDiagnosticReports . withTransaction store routesVar mXactId $ \xactId defer -> do
      resTy <- Store.getResourceType store (Just xactId) resTyName
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
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpResourceUpdate store routesVar request = do
  Log.scope (fromString "endpoint.resource-update") $ do
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
    mXactId <- optionalTransactionIdHeader store headers

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

    handleDiagnosticReports . withTransaction store routesVar mXactId $ \xactId defer -> do
      resTy <- Store.getResourceType store (Just xactId) resTyName
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
                                ++ if changed then "" else " (resource unchanged)"
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
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Wai.Request ->
  Name ->
  HandlerT m Wai.Response
httpResourceTypeList store request resTyName = do
  Log.scope (fromString "endpoint.resource-type-list") $ do
    mXactId <- optionalTransactionIdHeader store (Wai.requestHeaders request)

    mItems <- handleDiagnosticReports . runMaybeT $ do
      resTy <- MaybeT $ Store.lookupResourceType store mXactId resTyName
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
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  Name ->
  HandlerT m Wai.Response
httpResourceTypeRefresh store routesVar request resTyName = do
  mXactId <- optionalTransactionIdHeader store (Wai.requestHeaders request)

  handleDiagnosticReports . withTransaction store routesVar mXactId $ \xactId defer -> do
    mResTy <- Store.lookupResourceType store (Just xactId) resTyName
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

httpTransactionList :: MonadLog m => Store (ExceptT DiagnosticReports m) -> HandlerT m Wai.Response
httpTransactionList store =
  Log.scope (fromString "endpoint.transaction-list") $ do
    xactIds <- handleDiagnosticReports $ Store.listTransactions store
    pure . Wai.responseLBS ok200 [] . fromString $
      foldMap ((++ "\n") . Store.renderTransactionId) xactIds

httpTransactionBegin ::
  (MonadLog m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpTransactionBegin store routesVar request =
  Log.scope (fromString "endpoint.transaction-begin") $ do
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

    xactId <- handleDiagnosticReports $ Store.beginTransaction store defer
    liftIO $ beginRoutes routesVar xactId
    pure $ Wai.responseLBS ok200 [] (fromString $ Store.renderTransactionId xactId)

httpTransactionCommit ::
  (MonadLog m, MonadIO m, MonadCatch m) =>
  Store (ExceptT DiagnosticReports m) -> Routes -> Wai.Request -> HandlerT m Wai.Response
httpTransactionCommit store routesVar request =
  Log.scope (fromString "endpoint.transaction-commit") $ do
    let headers = Wai.requestHeaders request
    xactId <-
      requireTransactionId store
        =<< requireHeader headers (fromString "X-Blog-TransactionId")
    handleDiagnosticReports $ do
      transaction <-
        maybe (error $ "transaction not found: " ++ show xactId) pure
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
  (MonadLog m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) -> Routes -> Wai.Request -> HandlerT m Wai.Response
httpTransactionRollback store routesVar request =
  Log.scope (fromString "endpoint.transaction-rollback") $ do
    let headers = Wai.requestHeaders request
    xactId <-
      requireTransactionId store
        =<< requireHeader headers (fromString "X-Blog-TransactionId")
    handleDiagnosticReports $ Store.rollbackTransaction store xactId
    liftIO $ rollbackRoutes routesVar xactId
    pure $ Wai.responseLBS ok200 [] (fromString $ "rolled back " ++ Store.renderTransactionId xactId)

httpExport ::
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Wai.Request ->
  HandlerT m Wai.Response
httpExport store request = do
  Log.scope (fromString "endpoint.export") $ do
    mXactId <- optionalTransactionIdHeader store (Wai.requestHeaders request)
    content <-
      handleDiagnosticReports $ do
        content <- Store.export store mXactId

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
              "attachment; filename=\"" ++ renderUTCTime now ++ "-blog-export.tar\""
          )
        ]
    pure $ Wai.responseLBS ok200 headers content

httpImport ::
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpImport store routesVar request =
  Log.scope (fromString "endpoint.import") $ do
    mXactId <- optionalTransactionIdHeader store (Wai.requestHeaders request)
    handleDiagnosticReports . withTransaction store routesVar mXactId $ \xactId defer -> do
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
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Wai.Request ->
  -- | Resource type name
  Name ->
  -- | Resource name
  Name ->
  HandlerT m Wai.Response
httpResourceLookup store request resTyName resName =
  Log.scope (fromString "endpoint.resource-lookup") $ do
    mXactId <- optionalTransactionIdHeader store (Wai.requestHeaders request)

    mBody <- handleDiagnosticReports . runMaybeT $ do
      resTy <- MaybeT $ Store.lookupResourceType store mXactId resTyName
      MaybeT $ Store.readResource resTy resName

    case mBody of
      Nothing -> throwError $ Wai.responseLBS notFound404 [] (fromString "resource not found")
      Just body -> pure $ Wai.responseLBS ok200 [] (LazyByteString.fromStrict body)

httpResourceMetadataLookup ::
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Wai.Request ->
  -- | Resource type name
  Name ->
  -- | Resource name
  Name ->
  HandlerT m Wai.Response
httpResourceMetadataLookup store request resTyName resName =
  Log.scope (fromString "endpoint.resource-metadata-lookup") $ do
    mXactId <- optionalTransactionIdHeader store (Wai.requestHeaders request)

    mBody <- handleDiagnosticReports $ do
      resTy <- Store.getResourceType store mXactId resTyName
      Store.readProperty resTy resName (unsafeName "metadata")

    case mBody of
      Nothing -> throwError $ Wai.responseLBS notFound404 [] (fromString "metadata not found")
      Just body -> pure $ Wai.responseLBS ok200 [] (LazyByteString.fromStrict body)

httpResourcePropertiesUpdate ::
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  -- | Resource type name
  Name ->
  -- | Resource name
  Name ->
  HandlerT m Wai.Response
httpResourcePropertiesUpdate store routesVar request resTyName resName =
  Log.scope (fromString "endpoint.resource-properties-update") $ do
    mXactId <- optionalTransactionIdHeader store (Wai.requestHeaders request)

    properties <- handleDiagnosticReports $ do
      body <- liftIO $ Wai.consumeRequestBodyLazy request
      let
        resourceId = renderResourceId (ResourceId resTyName resName)
        reportTomlError =
          throwError
            . DiagnosticReports (fromString $ "(" ++ resourceId ++ ":properties)") body
            . tomlErrorReport

      Toml.Toml (Toml.Located _offset properties) nonKeys <-
        either reportTomlError pure . Toml.parse $ LazyByteString.toStrict body

      -- Tables and arrays are currently not accepted as properties
      unless (null nonKeys) . reportTomlError $
        Toml.UnexpectedEntries [] (fmap Toml.locatedOffset nonKeys)

      pure properties

    properties' <-
      for properties $ \(name, Toml.TomlKeyEntry _offset (Toml.Located _offset' value)) -> do
        name' <- requireName $ Text.unpack name
        pure (name', value)

    handleDiagnosticReports . withTransaction store routesVar mXactId $ \xactId defer -> do
      resTy <- Store.getResourceType store (Just xactId) resTyName
      let resId = ResourceId resTyName resName
      names <- for properties' $ \(name, value) -> do
        updated <- Store.setProperty resTy (resourceName resId) name (metadataValueFromToml value)
        pure (name, updated)

      if defer
        then do
          let
            response =
              fromString "updated properties:\n"
                <> foldMap
                  ( \(propName, updated) -> fromString $ "* " <> renderName propName <> (if updated then "" else " (unchanged)") <> "\n"
                  )
                  names
                <> fromString "(rules deferred)"

          pure $ Wai.responseLBS ok200 [] response
        else do
          let changed = any snd names
          changes <- evalRules store routesVar xactId [resId | changed]

          let
            response =
              ByteString.Lazy.Char8.unlines $
                fromString "updated properties:"
                  : ( fmap
                        ( \(propName, updated) -> fromString $ "* " <> renderName propName <> (if updated then "" else " (unchanged)")
                        )
                        names
                        ++ if null changes
                          then []
                          else
                            [mempty, fromString ("resource changes:")]
                              ++ renderChangeList changes
                    )

          pure $ Wai.responseLBS ok200 [] response

httpResourcePropertyLookup ::
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Wai.Request ->
  -- | Resource type name
  Name ->
  -- | Resource name
  Name ->
  -- | Property name
  Name ->
  HandlerT m Wai.Response
httpResourcePropertyLookup store request resTyName resName propName =
  Log.scope (fromString "endpoint.resource-property-lookup") $ do
    mXactId <- optionalTransactionIdHeader store (Wai.requestHeaders request)

    mBody <- handleDiagnosticReports $ do
      resTy <- Store.getResourceType store mXactId resTyName
      Store.readProperty resTy resName propName

    case mBody of
      Nothing ->
        pure $ Wai.responseLBS notFound404 [] (fromString "property not found")
      Just body ->
        pure $ Wai.responseLBS ok200 [] (LazyByteString.fromStrict body)

httpRouteGet ::
  (MonadLog m, MonadIO m, MonadMask m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  -- | Path
  [Text] ->
  HandlerT m Wai.Response
httpRouteGet store routesVar request path =
  Log.scope (fromString "endpoint.route-get") $ do
    mXactId <- optionalTransactionIdHeader store (Wai.requestHeaders request)

    handleDiagnosticReports $ do
      routes <- liftIO $ readActiveRoutes routesVar
      case Blog.Route.lookup path routes of
        Nothing ->
          pure $ Wai.responseLBS notFound404 [] (fromString "not found")
        Just resId -> do
          resTy <- Store.getResourceType store mXactId $ resourceType resId
          mContent <- Store.readResource resTy $ resourceName resId
          case mContent of
            Nothing ->
              pure $ Wai.responseLBS notFound404 [] (fromString "not found")
            Just content -> do
              let contentType = Text.Encoding.encodeUtf8 . cfgContentType $ Store.resourceTypeConfig resTy
              pure $ Wai.responseLBS ok200 [(hContentType, contentType)] (LazyByteString.fromStrict content)
