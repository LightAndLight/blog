{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Blog (ResourceId (..), cfgContentType, renderResourceId)
import qualified Blog.Build as Build
import Blog.Diagnostic (DiagnosticReports (..), renderDiagnosticReports)
import Blog.Error (sageErrorReport, tomlErrorReport)
import Blog.Metadata (metadataValueFromToml)
import qualified Blog.Route
import qualified Blog.Route as Route
import qualified Blog.Route as Routes
import qualified Blog.Rules
import Blog.Store (Store)
import qualified Blog.Store as Store
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVar, readTVar, readTVarIO)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Control.Monad.Catch (MonadMask, onException)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.Char as Char
import Data.Foldable (for_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM, rfc822DateFormat)
import Data.Traversable (for)
import GHC.Stack (HasCallStack)
import Network.HTTP.Types.Header (RequestHeaders, hContentType, hLastModified)
import Network.HTTP.Types.Status
  ( badRequest400
  , created201
  , methodNotAllowed405
  , notFound404
  , ok200
  , preconditionFailed412
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
beginRoutes routes xactId = atomically $ modifyTVar (routesPending routes) (Map.insert xactId Routes.empty)

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
    modifyTVar (routesPending routes) (Map.insertWith (<>) xactId (Routes.singleton path value))

initRoutes :: Store (ExceptT DiagnosticReports IO) -> IO Routes
initRoutes store = do
  routesVar <- atomically $ Routes <$> newTVar Routes.empty <*> newTVar mempty

  result <- runExceptT . withTransaction store routesVar Nothing $ \xactId _defer -> do
    mResTy <- Store.lookupResourceType store xactId "route"
    case mResTy of
      Nothing -> do
        liftIO $ putStrLn "info: resource type 'route' missing (starting with empty routes)"
      Just resTy -> do
        entries <- Store.listResource resTy
        for_ entries $ \entry -> do
          mContent <- Store.readResource resTy (resourceName entry)
          content <- maybe (error $ "missing " ++ renderResourceId entry) pure mContent
          Routes.RouteEntry path resId <-
            case Sage.parse (Route.routeEntryParser <* Sage.eof) $ LazyByteString.toStrict content of
              Right x ->
                pure x
              Left err ->
                throwError $
                  DiagnosticReports
                    (fromString $ renderResourceId entry)
                    content
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
  cli <- Options.execParser $ Options.info cliParser Options.fullDesc

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
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadError Wai.Response)

handleT ::
  MonadIO m =>
  (Wai.Response -> IO Wai.ResponseReceived) -> HandlerT m Wai.Response -> m Wai.ResponseReceived
handleT respond (HandlerT ma) = liftIO . either respond respond =<< runExceptT ma

handleExceptT :: Monad m => ExceptT DiagnosticReports m a -> HandlerT m a
handleExceptT ma = do
  result <- lift $ runExceptT ma
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
  HasCallStack =>
  (MonadMask m, MonadIO m) =>
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
    fromMaybe (error $ "transaction not found: " ++ Store.renderTransactionId xactId)
      <$> Store.lookupTransaction store xactId
  f xactId $ Store.xactDefer transaction

getRouteEntry ::
  MonadError DiagnosticReports m => Store.ResourceType m -> ResourceId -> m Blog.Route.RouteEntry
getRouteEntry resTy resId = do
  mContent <- Store.readResource resTy (resourceName resId)
  content <- maybe (error $ "missing " ++ renderResourceId resId) pure mContent
  case Sage.parse (Route.routeEntryParser <* Sage.eof) $ LazyByteString.toStrict content of
    Right x ->
      pure x
    Left err ->
      throwError $
        DiagnosticReports
          (fromString $ renderResourceId resId)
          content
          (sageErrorReport err)

evalRules ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  Store m ->
  Routes ->
  Store.TransactionId ->
  [ResourceId] ->
  m [Build.Change]
evalRules store routesVar xactId resIds = do
  changes <- Build.evalRules putStrLn store xactId Blog.Rules.rules resIds
  routeResTy <- Store.getResourceType store xactId "route"
  for_ changes $ \(Build.Change status changedId _reasons) ->
    case resourceType changedId of
      "route" -> do
        case status of
          Build.Created -> do
            Routes.RouteEntry path value <- getRouteEntry routeResTy changedId
            liftIO $ insertRoute xactId path value routesVar
          Build.Updated -> do
            Routes.RouteEntry path value <- getRouteEntry routeResTy changedId
            liftIO $ insertRoute xactId path value routesVar
      _ -> pure ()
  pure changes

app ::
  Store (ExceptT DiagnosticReports IO) ->
  Routes ->
  Wai.Application
app store routesVar request respond = do
  handleT respond $
    case Wai.pathInfo request of
      [part]
        | part == fromString ".resource" ->
            if Wai.requestMethod request == fromString "GET"
              then httpResourceGet store routesVar request
              else
                if Wai.requestMethod request == fromString "POST"
                  then httpResourcePost store routesVar request
                  else
                    if Wai.requestMethod request == fromString "PUT"
                      then httpResourcePut store routesVar request
                      else throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")
      [part, resTyName]
        | part == fromString ".resource" ->
            if Wai.requestMethod request == fromString "GET"
              then do
                mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

                let store' = Store.hoistStore lift store
                mItems <- handleExceptT . runMaybeT . withTransaction store' routesVar mXactId $ \xactId _defer -> do
                  resTy <- MaybeT $ Store.lookupResourceType store xactId (Text.unpack resTyName)
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
              else
                if Wai.requestMethod request == fromString "REFRESH"
                  then do
                    mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

                    handleExceptT . withTransaction store routesVar mXactId $ \xactId defer -> do
                      mResTy <- Store.lookupResourceType store xactId (Text.unpack resTyName)
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
                                    ("refreshed " ++ Text.unpack resTyName ++ ":*" ++ if defer then " (ignoring defer)" else "")
                                    : fmap ((fromString "* " <>) . fromString . Build.renderChange) changes
                              )
                  else throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")
      [part]
        | part == fromString ".transaction" ->
            if Wai.requestMethod request == fromString "GET"
              then do
                xactIds <- handleExceptT $ Store.listTransactions store
                pure . Wai.responseLBS ok200 [] . fromString $
                  foldMap ((++ "\n") . Store.renderTransactionId) xactIds
              else
                throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
      [part, action]
        | part == fromString ".transaction" ->
            if action == fromString "begin"
              then do
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

                xactId <- handleExceptT $ Store.beginTransaction store defer
                liftIO $ beginRoutes routesVar xactId
                pure $ Wai.responseLBS ok200 [] (fromString $ Store.renderTransactionId xactId)
              else
                if action == fromString "commit"
                  then do
                    let headers = Wai.requestHeaders request
                    xactId <- do
                      value <- fmap ByteString.Char8.unpack . requireHeader headers $ fromString "X-Blog-TransactionId"
                      parseTransactionId value
                    handleExceptT $ do
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
                          pure . Wai.responseLBS ok200 [] $
                            fromString "resource changes:\n"
                              <> foldMap ((fromString "* " <>) . (<> fromString "\n") . fromString . Build.renderChange) changes
                              <> fromString ("\ncommitted " ++ Store.renderTransactionId xactId)
                        else do
                          commit
                          pure . Wai.responseLBS ok200 [] . fromString $ "committed " ++ Store.renderTransactionId xactId
                  else
                    if action == fromString "rollback"
                      then do
                        let headers = Wai.requestHeaders request
                        xactId <- do
                          value <-
                            fmap ByteString.Char8.unpack . requireHeader headers $
                              fromString "X-Blog-TransactionId"
                          parseTransactionId value
                        handleExceptT $ Store.rollbackTransaction store xactId
                        liftIO $ rollbackRoutes routesVar xactId
                        pure $ Wai.responseLBS ok200 [] (fromString $ "committed " ++ Store.renderTransactionId xactId)
                      else
                        throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
      [part]
        | part == fromString ".export" ->
            if Wai.requestMethod request == fromString "GET"
              then do
                mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request
                content <-
                  handleExceptT . withTransaction store routesVar mXactId $ \xactId _defer -> do
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
              else
                throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
      [part]
        | part == fromString ".import" ->
            if Wai.requestMethod request == fromString "PUT"
              then do
                mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request
                handleExceptT . withTransaction store routesVar mXactId $ \xactId defer -> do
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
                          fromString "imported resources:\n"
                            <> foldMap (\resId -> fromString $ "* " <> renderResourceId resId <> "\n") imported
                            <> if null changes
                              then mempty
                              else
                                fromString ("\nresource changes:\n")
                                  <> foldMap ((fromString "* " <>) . (<> fromString "\n") . fromString . Build.renderChange) changes

                      pure $ Wai.responseLBS ok200 [] response
              else
                throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
      [part, resTyName, resName]
        | part == fromString ".resource" ->
            if Wai.requestMethod request == fromString "GET"
              then do
                mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

                let store' = Store.hoistStore lift store
                mBody <- handleExceptT . runMaybeT . withTransaction store' routesVar mXactId $ \xactId _defer -> do
                  resTy <- MaybeT $ Store.lookupResourceType store xactId (Text.unpack resTyName)
                  MaybeT $ Store.readResource resTy (Text.unpack resName)

                case mBody of
                  Nothing -> throwError $ Wai.responseLBS notFound404 [] (fromString "resource not found")
                  Just body -> pure $ Wai.responseLBS ok200 [] body
              else throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")
      [part, resTyName, resName, part']
        | part == fromString ".resource"
        , part' == fromString "metadata" ->
            if Wai.requestMethod request == fromString "GET"
              then do
                mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

                mBody <- handleExceptT . withTransaction store routesVar mXactId $ \xactId _defer -> do
                  resTy <- Store.getResourceType store xactId (Text.unpack resTyName)
                  Store.readResourceMetadata resTy (Text.unpack resName)

                case mBody of
                  Nothing -> throwError $ Wai.responseLBS notFound404 [] (fromString "metadata not found")
                  Just body -> pure $ Wai.responseLBS ok200 [] body
              else throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")
        | part == fromString ".resource"
        , part' == fromString "property" -> do
            if Wai.requestMethod request == fromString "PATCH"
              then do
                mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

                handleExceptT . withTransaction store routesVar mXactId $ \xactId defer -> do
                  body <- liftIO $ Wai.consumeRequestBodyLazy request
                  Toml.Toml (Toml.Located _offset properties) nonKeys <-
                    case Toml.parse $ LazyByteString.toStrict body of
                      Left err -> do
                        let resourceId = renderResourceId (ResourceId (Text.unpack resTyName) (Text.unpack resName))
                        throwError
                          . DiagnosticReports
                            (fromString $ "(" ++ resourceId ++ ":properties)")
                            body
                          $ tomlErrorReport err
                      Right x -> pure x

                  unless (null nonKeys) . error $
                    "TODO: non-key-value properties: " ++ show nonKeys

                  resTy <- Store.getResourceType store xactId (Text.unpack resTyName)
                  let resId = ResourceId (Text.unpack resTyName) (Text.unpack resName)
                  names <- for properties $ \(name, Toml.TomlKeyEntry _offset (Toml.Located _offset' value)) -> do
                    Store.setProperty resTy (resourceName resId) (Text.unpack name) (metadataValueFromToml value)
                    pure name

                  if defer
                    then do
                      let
                        response =
                          fromString "updated properties:\n"
                            <> foldMap (\propName -> fromString $ "* " <> Text.unpack propName <> "\n") names
                            <> fromString "(rules deferred)"

                      pure $ Wai.responseLBS ok200 [] response
                    else do
                      changes <- evalRules store routesVar xactId [resId]

                      let
                        response =
                          fromString "updated properties:\n"
                            <> foldMap (\propName -> fromString $ "* " <> Text.unpack propName <> "\n") names
                            <> if null changes
                              then mempty
                              else
                                fromString ("\nresource changes:\n")
                                  <> foldMap ((fromString "* " <>) . fromString . Build.renderChange) changes

                      pure $ Wai.responseLBS ok200 [] response
              else throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")
      [part, resTyName, resName, part', propName]
        | part == fromString ".resource"
        , part' == fromString "property" -> do
            if Wai.requestMethod request == fromString "GET"
              then do
                mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

                mBody <- handleExceptT . withTransaction store routesVar mXactId $ \xactId _defer -> do
                  resTy <- Store.getResourceType store xactId $ Text.unpack resTyName
                  Store.readProperty resTy (Text.unpack resName) (Text.unpack propName)

                case mBody of
                  Nothing ->
                    pure $ Wai.responseLBS notFound404 [] (fromString "property not found")
                  Just body ->
                    pure $ Wai.responseLBS ok200 [] body
              else throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")
      path -> do
        mXactId <- optionalTransactionIdHeader $ Wai.requestHeaders request

        handleExceptT . withTransaction store routesVar mXactId $ \xactId _defer -> do
          routes <- liftIO $ readActiveRoutes routesVar
          case Routes.lookup path routes of
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
                  pure $ Wai.responseLBS ok200 [(hContentType, contentType)] content

httpResourceGet ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpResourceGet store routesVar request = do
  let headers = Wai.requestHeaders request

  resTyName <-
    fmap (Text.pack . ByteString.Char8.unpack) . requireHeader headers $
      fromString "X-Blog-ResourceType"
  resName <- fmap ByteString.Char8.unpack . requireHeader headers $ fromString "X-Blog-ResourceName"
  mXactId <- optionalTransactionIdHeader headers

  mBody <- handleExceptT . withTransaction store routesVar mXactId $ \xactId _defer -> do
    resTy <- Store.getResourceType store xactId (Text.unpack resTyName)
    Store.readResource resTy resName
  case mBody of
    Just body ->
      pure $ Wai.responseLBS ok200 [] body
    Nothing ->
      throwError $
        Wai.responseLBS
          badRequest400
          []
          (fromString $ "resource " ++ Text.unpack resTyName ++ ":" ++ resName ++ " does not exist")

httpResourcePost ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpResourcePost store routesVar request = do
  let headers = Wai.requestHeaders request

  resTyName <-
    fmap (ByteString.Char8.unpack) . requireHeader headers $ fromString "X-Blog-ResourceType"
  resName <- fmap ByteString.Char8.unpack . requireHeader headers $ fromString "X-Blog-ResourceName"
  let resId = ResourceId resTyName resName
  mXactId <- optionalTransactionIdHeader headers

  handleExceptT . withTransaction store routesVar mXactId $ \xactId defer -> do
    resTy <- Store.getResourceType store xactId (fromString resTyName)
    exists <- Store.doesResourceExist resTy resName
    if exists
      then throwError . DiagnosticSimple $ "resource " ++ resTyName ++ ":" ++ resName ++ " already exists"
      else do
        body <- liftIO $ Wai.consumeRequestBodyLazy request
        Store.writeResource resTy resName body
        if defer
          then
            pure $
              Wai.responseLBS
                created201
                []
                ( ByteString.Lazy.Char8.unlines
                    [ fromString ("created " ++ resTyName ++ ":" ++ resName)
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
                    fromString ("created " ++ resTyName ++ ":" ++ resName)
                      : fmap ((fromString "* " <>) . fromString . Build.renderChange) changes
                )

httpResourcePut ::
  (MonadMask m, MonadIO m) =>
  Store (ExceptT DiagnosticReports m) ->
  Routes ->
  Wai.Request ->
  HandlerT m Wai.Response
httpResourcePut store routesVar request = do
  let headers = Wai.requestHeaders request

  resTyName <- fmap ByteString.Char8.unpack $ requireHeader headers "X-Blog-ResourceType"
  resName <- fmap ByteString.Char8.unpack $ requireHeader headers "X-Blog-ResourceName"
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

  handleExceptT . withTransaction store routesVar mXactId $ \xactId defer -> do
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
            Store.writeResource resTy resName body

            if defer
              then
                pure $
                  Wai.responseLBS
                    ok200
                    responseHeaders
                    ( ByteString.Lazy.Char8.unlines
                        [ fromString ("updated " ++ resTyName ++ ":" ++ resName)
                        , fromString "(rules deferred)"
                        ]
                    )
              else do
                changes <- evalRules store routesVar xactId [resId]
                pure $
                  Wai.responseLBS
                    ok200
                    responseHeaders
                    ( ByteString.Lazy.Char8.unlines $
                        fromString ("updated " ++ resTyName ++ ":" ++ resName)
                          : fmap ((fromString "* " <>) . fromString . Build.renderChange) changes
                    )
          else
            pure . Wai.responseLBS preconditionFailed412 [] . fromString $
              "the local copy of the resource is out of date"
      Nothing ->
        pure . Wai.responseLBS badRequest400 [] . fromString $
          "resource " ++ resTyName ++ ":" ++ resName ++ " does not exist"
