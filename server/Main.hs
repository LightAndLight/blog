{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Blog (ResourceId (..), renderResourceId)
import qualified Blog.Build as Build
import Blog.Diagnostic (DiagnosticReports, renderDiagnosticReports)
import qualified Blog.Rules
import Blog.Store (Store)
import qualified Blog.Store as Store
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM, rfc822DateFormat)
import Network.HTTP.Types.Header (RequestHeaders, hLastModified)
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
import System.FilePath ((</>))

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

initData :: FilePath -> IO ()
initData data_ = do
  createDirectoryIfMissing True data_
  createDirectoryIfMissing False $ data_ </> "resource"

main :: IO ()
main = do
  cli <- Options.execParser $ Options.info cliParser Options.fullDesc

  putStrLn $ "Running at https://localhost:" ++ show (cliPort cli)
  putStrLn $ "  Data directory: " ++ cliData cli
  initData $ cliData cli

  let tlsSettings = WarpTLS.tlsSettings (cliCert cli) (cliKey cli)
  let settings = Warp.setPort (cliPort cli) Warp.defaultSettings
  WarpTLS.runTLS tlsSettings settings $ app cli

newtype HandlerT m a = HandlerT (ExceptT Wai.Response m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadError Wai.Response)

handleT ::
  MonadIO m =>
  (Wai.Response -> IO Wai.ResponseReceived) -> HandlerT m Wai.Response -> m Wai.ResponseReceived
handleT respond (HandlerT ma) = liftIO . either respond respond =<< runExceptT ma

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

handleExceptT :: Monad m => ExceptT DiagnosticReports m a -> HandlerT m a
handleExceptT ma = do
  result <- lift $ runExceptT ma
  case result of
    Left err ->
      throwError $ Wai.responseLBS badRequest400 [] (renderDiagnosticReports err)
    Right a ->
      pure a

app :: Cli -> Wai.Application
app cli request respond = do
  store :: Store (ExceptT DiagnosticReports IO) <- Store.fromDirectory $ cliData cli
  handleT respond $
    case Wai.pathInfo request of
      [part]
        | part == fromString ".resource" ->
            if Wai.requestMethod request == fromString "GET"
              then httpResourceGet store request
              else
                if Wai.requestMethod request == fromString "POST"
                  then httpResourcePost store request
                  else
                    if Wai.requestMethod request == fromString "PUT"
                      then httpResourcePut store request
                      else throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")
      [part, resTyName]
        | part == fromString ".resource" ->
            if Wai.requestMethod request == fromString "GET"
              then do
                resTy <- handleExceptT $ Store.getResourceType store (Text.unpack resTyName)
                items <- handleExceptT $ Store.listResource resTy
                pure $
                  Wai.responseLBS
                    ok200
                    []
                    (foldMap ((<> fromString "\n") . fromString . renderResourceId) items)
              else throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")
      [part, resTyName, resName]
        | part == fromString ".resource" ->
            if Wai.requestMethod request == fromString "GET"
              then do
                resTy <- handleExceptT $ Store.getResourceType store (Text.unpack resTyName)
                mBody <- handleExceptT $ Store.readResource resTy (Text.unpack resName)
                case mBody of
                  Nothing -> throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
                  Just body -> pure $ Wai.responseLBS ok200 [] body
              else throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")
      [part, resTyName, resName, part']
        | part == fromString ".resource"
        , part' == fromString "metadata" ->
            if Wai.requestMethod request == fromString "GET"
              then do
                resTy <- handleExceptT $ Store.getResourceType store (Text.unpack resTyName)
                mBody <- handleExceptT $ Store.readResourceMetadata resTy (Text.unpack resName)
                case mBody of
                  Nothing -> throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
                  Just body -> pure $ Wai.responseLBS ok200 [] body
              else throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")
      _ ->
        throwError $ Wai.responseLBS notFound404 [] (fromString "not found")

httpResourceGet ::
  MonadIO m =>
  Store (ExceptT DiagnosticReports m) ->
  Wai.Request ->
  HandlerT m Wai.Response
httpResourceGet store request = do
  let headers = Wai.requestHeaders request

  resTyName <-
    fmap (Text.pack . ByteString.Char8.unpack) . requireHeader headers $
      fromString "X-Blog-ResourceType"
  resName <- fmap ByteString.Char8.unpack . requireHeader headers $ fromString "X-Blog-ResourceName"

  resTy <- handleExceptT $ Store.getResourceType store (Text.unpack resTyName)
  exists <- handleExceptT $ Store.doesResourceExist resTy resName
  if exists
    then do
      body <- liftIO $ Wai.consumeRequestBodyLazy request
      pure $ Wai.responseLBS ok200 [] body
    else
      throwError $
        Wai.responseLBS
          badRequest400
          []
          (fromString $ "resource " ++ Text.unpack resTyName ++ ":" ++ resName ++ " does not exist")

httpResourcePost ::
  MonadIO m =>
  Store (ExceptT DiagnosticReports m) ->
  Wai.Request ->
  HandlerT m Wai.Response
httpResourcePost store request = do
  let headers = Wai.requestHeaders request

  resTyName <-
    fmap (ByteString.Char8.unpack) . requireHeader headers $ fromString "X-Blog-ResourceType"
  resName <- fmap ByteString.Char8.unpack . requireHeader headers $ fromString "X-Blog-ResourceName"
  let resId = ResourceId resTyName resName

  resTy <- handleExceptT $ Store.getResourceType store (fromString resTyName)
  exists <- handleExceptT $ Store.doesResourceExist resTy resName
  if exists
    then
      throwError $
        Wai.responseLBS
          badRequest400
          []
          (fromString $ "resource " ++ resTyName ++ ":" ++ resName ++ " already exists")
    else do
      body <- liftIO $ Wai.consumeRequestBodyLazy request
      changes <- handleExceptT $ do
        Store.writeResource resTy resName body
        Build.evalRules putStrLn store Blog.Rules.rules resId
      pure $
        Wai.responseLBS
          created201
          []
          ( ByteString.Lazy.Char8.unlines $
              fromString ("created " ++ resTyName ++ ":" ++ resName)
                : fmap ((fromString "* " <>) . fromString . Build.renderChange) changes
          )

httpResourcePut ::
  MonadIO m =>
  Store (ExceptT DiagnosticReports m) ->
  Wai.Request ->
  HandlerT m Wai.Response
httpResourcePut store request = do
  let headers = Wai.requestHeaders request

  resTyName <- fmap ByteString.Char8.unpack $ requireHeader headers "X-Blog-ResourceType"
  resName <- fmap ByteString.Char8.unpack $ requireHeader headers "X-Blog-ResourceName"
  let resId = ResourceId resTyName resName

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

  resTy <- handleExceptT $ Store.getResourceType store resTyName
  mServerModificationTime <- handleExceptT $ Store.readResourceModificationTime resTy resName
  case mServerModificationTime of
    Just serverModificationTime -> do
      let
        responseHeaders =
          [ (hLastModified, fromString $ formatTime defaultTimeLocale rfc822DateFormat serverModificationTime)
          ]
      if maybe True (serverModificationTime <=) mLocalModificationTime
        then do
          body <- liftIO $ Wai.consumeRequestBodyLazy request
          changes <- handleExceptT $ do
            Store.writeResource resTy resName body
            Build.evalRules putStrLn store Blog.Rules.rules resId
          pure $
            Wai.responseLBS
              ok200
              responseHeaders
              ( ByteString.Lazy.Char8.unlines $
                  fromString ("updated " ++ resTyName ++ ":" ++ resName)
                    : fmap ((fromString "* " <>) . fromString . Build.renderChange) changes
              )
        else
          throwError $
            Wai.responseLBS
              preconditionFailed412
              responseHeaders
              (fromString "the local copy of the resource is out of date")
    Nothing ->
      throwError $
        Wai.responseLBS
          badRequest400
          []
          (fromString $ "resource " ++ resTyName ++ ":" ++ resName ++ " does not exist")
