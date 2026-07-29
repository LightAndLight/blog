{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Blog (ResourceId (..), renderResourceId)
import qualified Blog.Build as Build
import Blog.Diagnostic (renderDiagnosticReports)
import Blog.Metadata (lookupResourceMetadata)
import Blog.Resource
  ( createResource
  , doesResourceExist
  , getResourceType
  , listResource
  , lookupResource
  , updateResource
  )
import qualified Blog.Rules
import Control.Exception (catch, throwIO)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
  , getModificationTime
  )
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

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
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Wai.Response)

handleT ::
  MonadIO m =>
  (Wai.Response -> IO Wai.ResponseReceived) -> HandlerT m Wai.Response -> m Wai.ResponseReceived
handleT respond (HandlerT ma) = liftIO . either respond respond =<< runExceptT ma

requireHeader :: MonadIO m => RequestHeaders -> String -> HandlerT m ByteString
requireHeader headers headerName = HandlerT $ do
  case lookup (fromString headerName) headers of
    Nothing ->
      throwError $
        Wai.responseLBS
          badRequest400
          []
          (fromString $ "missing header: " ++ headerName)
    Just headerValue ->
      pure headerValue

app :: Cli -> Wai.Application
app cli request respond =
  handleT respond $
    case Wai.pathInfo request of
      [part]
        | part == fromString ".resource" ->
            if Wai.requestMethod request == fromString "GET"
              then httpResourceGet cli request
              else
                if Wai.requestMethod request == fromString "POST"
                  then httpResourcePost cli request
                  else
                    if Wai.requestMethod request == fromString "PUT"
                      then httpResourcePut cli request
                      else throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")
      [part, resTyName]
        | part == fromString ".resource" ->
            if Wai.requestMethod request == fromString "GET"
              then do
                mResTy <- runExceptT $ getResourceType (cliData cli) resTyName
                case mResTy of
                  Left err ->
                    throwError $ Wai.responseLBS badRequest400 [] (renderDiagnosticReports err)
                  Right Nothing ->
                    throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
                  Right (Just (resTyDir, resTy)) -> do
                    mBody <- runExceptT $ listResource resTyDir resTy
                    case mBody of
                      Left err ->
                        throwError $ Wai.responseLBS badRequest400 [] (renderDiagnosticReports err)
                      Right items ->
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
                mResTy <- runExceptT $ getResourceType (cliData cli) resTyName
                case mResTy of
                  Left err ->
                    throwError $ Wai.responseLBS badRequest400 [] (renderDiagnosticReports err)
                  Right Nothing ->
                    throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
                  Right (Just (resTyDir, _resTy)) -> do
                    mBody <- liftIO $ lookupResource resTyDir $ Text.unpack resName
                    case mBody of
                      Nothing -> throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
                      Just body -> pure $ Wai.responseLBS ok200 [] body
              else throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")
      [part, resTyName, resName, part']
        | part == fromString ".resource"
        , part' == fromString "metadata" ->
            if Wai.requestMethod request == fromString "GET"
              then do
                mResTy <- runExceptT $ getResourceType (cliData cli) resTyName
                case mResTy of
                  Left err ->
                    throwError $ Wai.responseLBS badRequest400 [] (renderDiagnosticReports err)
                  Right Nothing ->
                    throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
                  Right (Just (resTyDir, _resTy)) -> do
                    mBody <- liftIO $ lookupResourceMetadata resTyDir $ Text.unpack resName
                    case mBody of
                      Nothing -> throwError $ Wai.responseLBS notFound404 [] (fromString "not found")
                      Just body -> pure $ Wai.responseLBS ok200 [] body
              else throwError $ Wai.responseLBS methodNotAllowed405 [] (fromString "method not allowed")
      _ ->
        throwError $ Wai.responseLBS notFound404 [] (fromString "not found")

httpResourceGet :: MonadIO m => Cli -> Wai.Request -> HandlerT m Wai.Response
httpResourceGet cli request = do
  let headers = Wai.requestHeaders request

  resTyName <-
    fmap (Text.pack . ByteString.Char8.unpack) . requireHeader headers $
      fromString "X-Blog-ResourceType"
  resName <- fmap ByteString.Char8.unpack . requireHeader headers $ fromString "X-Blog-ResourceName"

  mResTyDir <- runExceptT $ getResourceType (cliData cli) resTyName
  case mResTyDir of
    Left err ->
      throwError $ Wai.responseLBS badRequest400 [] (renderDiagnosticReports err)
    Right Nothing ->
      throwError $
        Wai.responseLBS
          badRequest400
          []
          (fromString $ "no such resource type: " ++ Text.unpack resTyName)
    Right (Just (resTyDir, _resTy)) -> do
      exists <- liftIO $ doesResourceExist resTyDir resName
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

httpResourcePost :: MonadIO m => Cli -> Wai.Request -> HandlerT m Wai.Response
httpResourcePost cli request = do
  let headers = Wai.requestHeaders request

  resTyName <-
    fmap (ByteString.Char8.unpack) . requireHeader headers $ fromString "X-Blog-ResourceType"
  resName <- fmap ByteString.Char8.unpack . requireHeader headers $ fromString "X-Blog-ResourceName"
  let resId = ResourceId resTyName resName

  mResTyDir <- runExceptT $ getResourceType (cliData cli) (fromString resTyName)
  case mResTyDir of
    Left err ->
      throwError $ Wai.responseLBS badRequest400 [] (renderDiagnosticReports err)
    Right Nothing ->
      throwError $
        Wai.responseLBS
          badRequest400
          []
          (fromString $ "no such resource type: " ++ resTyName)
    Right (Just (resTyDir, resTy)) -> do
      exists <- liftIO $ doesResourceExist resTyDir resName
      if exists
        then
          throwError $
            Wai.responseLBS
              badRequest400
              []
              (fromString $ "resource " ++ resTyName ++ ":" ++ resName ++ " already exists")
        else do
          body <- liftIO $ Wai.consumeRequestBodyLazy request
          result <- runExceptT $ do
            createResource (cliData cli) resTy resName body
            Build.evalRules putStrLn (cliData cli) Blog.Rules.rules resId
          case result of
            Right changes -> do
              pure $
                Wai.responseLBS
                  created201
                  []
                  ( ByteString.Lazy.Char8.unlines $
                      fromString ("created " ++ resTyName ++ ":" ++ resName)
                        : fmap ((fromString "* " <>) . fromString . Build.renderChange) changes
                  )
            Left err ->
              throwError $
                Wai.responseLBS
                  badRequest400
                  []
                  (renderDiagnosticReports err)

httpResourcePut :: MonadIO m => Cli -> Wai.Request -> HandlerT m Wai.Response
httpResourcePut cli request = do
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

  mResTyDir <- runExceptT $ getResourceType (cliData cli) (Text.pack resTyName)
  case mResTyDir of
    Left err ->
      throwError $ Wai.responseLBS badRequest400 [] (renderDiagnosticReports err)
    Right Nothing ->
      throwError $
        Wai.responseLBS
          badRequest400
          []
          (fromString $ "no such resource type: " ++ resTyName)
    Right (Just (resTyDir, resTy)) -> do
      mServerModificationTime <-
        liftIO $
          fmap Just (getModificationTime $ resTyDir </> resName)
            `catch` \err -> if isDoesNotExistError err then pure Nothing else throwIO err
      case mServerModificationTime of
        Just serverModificationTime -> do
          let
            responseHeaders =
              [ (hLastModified, fromString $ formatTime defaultTimeLocale rfc822DateFormat serverModificationTime)
              ]
          if maybe True (serverModificationTime <=) mLocalModificationTime
            then do
              body <- liftIO $ Wai.consumeRequestBodyLazy request
              result <- runExceptT $ do
                updateResource (cliData cli) resTy resName body
                Build.evalRules putStrLn (cliData cli) Blog.Rules.rules resId
              case result of
                Right changes -> do
                  pure $
                    Wai.responseLBS
                      ok200
                      responseHeaders
                      ( ByteString.Lazy.Char8.unlines $
                          fromString ("updated " ++ resTyName ++ ":" ++ resName)
                            : fmap ((fromString "* " <>) . fromString . Build.renderChange) changes
                      )
                Left err ->
                  throwError $
                    Wai.responseLBS
                      badRequest400
                      []
                      (renderDiagnosticReports err)
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
