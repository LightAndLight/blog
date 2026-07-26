module Main (main) where

import Blog (ResourceId (..), renderResourceId, resourceIdParser)
import Control.Applicative (optional, (<**>))
import Control.Exception (catch, finally, throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.Time.Format (defaultTimeLocale, formatTime, rfc822DateFormat)
import Data.X509.CertificateStore (CertificateStore, readCertificateStore)
import Network.Connection (TLSSettings (..))
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Client.TLS as Http.Tls
import Network.HTTP.Types.Header (RequestHeaders, ResponseHeaders, hIfUnmodifiedSince)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.TLS as Tls
import Network.TLS.Extra.Cipher (ciphersuite_default)
import qualified Options.Applicative as Options
import System.Directory (createDirectoryIfMissing, getModificationTime, removeFile)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Process (callProcess)
import qualified Text.Sage as Sage

data Cli
  = Cli
  { cliBaseUrl :: String
  -- ^ Base URL of blog server
  , cliCaCert :: !(Maybe FilePath)
  -- ^ CA certificate
  , cliCommand :: Command
  }

data Command
  = View
      -- | View metadata
      Bool
      -- | ID of resource to view
      String
  | List
      -- | Resource type
      String
  | Create
      -- | Source file
      (Maybe FilePath)
      -- | ID of resource to create
      String
  | Update
      -- | Source file
      FilePath
      -- | ID of resource to create
      String
  | Edit
      -- | ID of resource to edit
      String

cliParser :: Options.Parser Cli
cliParser =
  Cli
    <$> Options.strOption
      (Options.long "base" <> Options.metavar "URL" <> Options.help "Blog server base URL")
    <*> optional
      ( Options.strOption $
          Options.long "cacert" <> Options.metavar "FILE" <> Options.help "TLS CA certificate"
      )
    <*> Options.hsubparser
      ( Options.command "view" (Options.info viewParser $ Options.progDesc "View a resource")
          <> Options.command
            "list"
            (Options.info listParser $ Options.progDesc "List resources of a specific type")
          <> Options.command "create" (Options.info createParser $ Options.progDesc "Create an empty resource")
          <> Options.command "update" (Options.info updateParser $ Options.progDesc "Update a resource")
          <> Options.command "edit" (Options.info editParser $ Options.progDesc "Edit a resource")
      )
  where
    viewParser =
      View
        <$> Options.switch
          (Options.long "metadata" <> Options.help "View metadata only")
        <*> Options.strArgument
          (Options.metavar "RESOURCE" <> Options.help "ID of resource to view (format: `TYPE:NAME`)")

    listParser =
      List
        <$> Options.strArgument
          (Options.metavar "TYPE" <> Options.help "Type of resource to view")

    createParser =
      Create
        <$> optional
          ( Options.strOption $
              Options.long "from" <> Options.short 'f' <> Options.metavar "FILE" <> Options.help "Source file"
          )
        <*> Options.strArgument
          (Options.metavar "RESOURCE" <> Options.help "ID of resource to create (format: `TYPE:NAME`)")

    updateParser =
      Update
        <$> Options.strOption
          (Options.long "from" <> Options.short 'f' <> Options.metavar "FILE" <> Options.help "Source file")
        <*> Options.strArgument
          (Options.metavar "RESOURCE" <> Options.help "ID of resource to create (format: `TYPE:NAME`)")

    editParser =
      Edit
        <$> Options.strArgument
          (Options.metavar "RESOURCE" <> Options.help "ID of resource to edit (format: `TYPE:NAME`)")

parseResourceId ::
  -- | Input
  String ->
  IO ResourceId
parseResourceId input =
  case Sage.parse (resourceIdParser <* Sage.eof) $ fromString input of
    Left _err -> do
      putStrLn $ "error: invalid resource ID '" ++ input ++ "'"
      exitFailure
    Right x ->
      pure x

main :: IO ()
main = do
  cli <- Options.execParser $ Options.info (cliParser <**> Options.helper) Options.fullDesc
  mCertificateStore <-
    case cliCaCert cli of
      Nothing -> pure Nothing
      Just caCert -> do
        mStore <- readCertificateStore caCert
        case mStore of
          Nothing -> do
            putStrLn $ "error: failed to read CA certificate from " ++ caCert
            exitFailure
          Just store -> pure $ Just store
  let baseUrl = cliBaseUrl cli
  case cliCommand cli of
    View metadata resourceId -> do
      resourceId' <- parseResourceId resourceId
      view baseUrl mCertificateStore metadata resourceId'
    List resourceTyName ->
      list baseUrl mCertificateStore resourceTyName
    Create mSrcFile resourceId -> do
      resourceId' <- parseResourceId resourceId
      create baseUrl mCertificateStore mSrcFile resourceId'
    Update srcFile resourceId -> do
      resourceId' <- parseResourceId resourceId
      update baseUrl mCertificateStore srcFile resourceId'
    Edit resourceId -> do
      resourceId' <- parseResourceId resourceId
      edit baseUrl mCertificateStore resourceId'

-- <https://stackoverflow.com/a/41816183>
httpManager :: Maybe CertificateStore -> IO Http.Manager
httpManager Nothing =
  Http.newManager tlsManagerSettings
httpManager (Just caStore) =
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

data Response a
  = NotFound
  | PreconditionFailed
  | Conflict
  | Created
  | Ok a

http ::
  Http.Manager ->
  -- | URL
  String ->
  -- | Method
  ByteString ->
  RequestHeaders ->
  LazyByteString ->
  IO (ResponseHeaders, Response LazyByteString)
http manager url method headers body = do
  request <- do
    request <- Http.parseRequest url
    pure
      request
        { Http.method = method
        , Http.requestHeaders = headers
        , Http.requestBody = Http.RequestBodyLBS body
        }
  response <- Http.httpLbs request manager
  case statusCode $ Http.responseStatus response of
    404 -> pure (Http.responseHeaders response, NotFound)
    409 -> pure (Http.responseHeaders response, Conflict)
    412 -> pure (Http.responseHeaders response, PreconditionFailed)
    200 -> pure (Http.responseHeaders response, Ok $ Http.responseBody response)
    201 -> pure (Http.responseHeaders response, Created)
    _status -> do
      putStrLn $ ByteString.Lazy.Char8.unpack (Http.responseBody response)
      exitFailure

httpGet ::
  Http.Manager ->
  -- | URL
  String ->
  RequestHeaders ->
  IO (ResponseHeaders, Response LazyByteString)
httpGet manager url headers = http manager url (fromString "GET") headers mempty

httpPost ::
  Http.Manager ->
  -- | URL
  String ->
  RequestHeaders ->
  LazyByteString ->
  IO (ResponseHeaders, Response LazyByteString)
httpPost manager url = http manager url (fromString "POST")

httpPut ::
  Http.Manager ->
  -- | URL
  String ->
  RequestHeaders ->
  LazyByteString ->
  IO (ResponseHeaders, Response LazyByteString)
httpPut manager url = http manager url (fromString "PUT")

requireEnv :: String -> IO String
requireEnv key = do
  mValue <- lookupEnv key
  case mValue of
    Nothing -> do
      putStrLn $ "error: " ++ key ++ " not set"
      exitFailure
    Just editor -> pure editor

getEditor :: IO String
getEditor = requireEnv "EDITOR"

getPager :: IO String
getPager = requireEnv "PAGER"

getDataHome :: IO FilePath
getDataHome = do
  mDataHome <- lookupEnv "XDG_DATA_HOME"
  case mDataHome of
    Just dataHome -> pure dataHome
    Nothing -> do
      home <- requireEnv "HOME"
      pure $ home </> ".local" </> "share"

resourceIdHeaders :: ResourceId -> RequestHeaders
resourceIdHeaders resourceId =
  [ (fromString "X-Blog-ResourceType", fromString $ resourceType resourceId)
  , (fromString "X-Blog-ResourceName", fromString $ resourceName resourceId)
  ]

view ::
  String ->
  Maybe CertificateStore ->
  -- | View metadata only
  Bool ->
  ResourceId ->
  IO ()
view baseUrl mCertificateStore metadata resourceId = do
  dataHome <- getDataHome
  pager <- getPager

  manager <- httpManager mCertificateStore

  let
    resourceDirLocal
      | metadata = dataHome </> "blog" </> resourceType resourceId </> (resourceName resourceId ++ ".d")
      | otherwise = dataHome </> "blog" </> resourceType resourceId
  createDirectoryIfMissing True resourceDirLocal

  let
    resourcePathLocal
      | metadata = resourceDirLocal </> "metadata"
      | otherwise = resourceDirLocal </> resourceName resourceId

  (_responseHeaders, rBody) <- do
    mLocalModificationTime <-
      fmap Just (getModificationTime resourcePathLocal)
        `catch` \err ->
          if isDoesNotExistError err
            then pure Nothing
            else throwIO err
    let
      headers =
        resourceIdHeaders resourceId
          ++ [ ( hIfUnmodifiedSince
               , fromString $ formatTime defaultTimeLocale rfc822DateFormat localModificationTime
               )
             | Just localModificationTime <- pure $ mLocalModificationTime
             ]

      url
        | metadata =
            (baseUrl ++ "/.resource/" ++ resourceType resourceId ++ "/" ++ resourceName resourceId ++ "/metadata")
        | otherwise =
            (baseUrl ++ "/.resource/" ++ resourceType resourceId ++ "/" ++ resourceName resourceId)

    httpGet manager url headers

  case rBody of
    Conflict ->
      error "impossible"
    Created ->
      error "impossible"
    PreconditionFailed -> do
      putStrLn "error: the local copy of this resource is out of date"
      exitFailure
    NotFound -> do
      putStrLn $ "error: resource " ++ renderResourceId resourceId ++ " not found"
      exitFailure
    Ok body -> do
      LazyByteString.writeFile resourcePathLocal body

  callProcess pager [resourcePathLocal] `finally` removeFile resourcePathLocal

list ::
  String ->
  Maybe CertificateStore ->
  -- | Resource type
  String ->
  IO ()
list baseUrl mCertificateStore resourceTyName = do
  dataHome <- getDataHome
  pager <- getPager

  manager <- httpManager mCertificateStore

  let resourceDirLocal = dataHome </> "blog" </> "resource" </> (resourceTyName ++ ".d")
  createDirectoryIfMissing True resourceDirLocal

  let resourcePathLocal = resourceDirLocal </> "list"

  (_responseHeaders, rBody) <- do
    let
      headers = []
      url = baseUrl ++ "/.resource/" ++ resourceTyName

    httpGet manager url headers

  case rBody of
    Conflict ->
      error "impossible"
    Created ->
      error "impossible"
    PreconditionFailed -> do
      error "impossible"
    NotFound -> do
      putStrLn $ "error: resource type " ++ resourceTyName ++ " not found"
      exitFailure
    Ok body -> do
      LazyByteString.writeFile resourcePathLocal body

  callProcess pager [resourcePathLocal] `finally` removeFile resourcePathLocal

create :: String -> Maybe CertificateStore -> Maybe FilePath -> ResourceId -> IO ()
create baseUrl mCertificateStore mSrcFile resourceId = do
  manager <- httpManager mCertificateStore

  let headers = resourceIdHeaders resourceId
  (_responseHeaders, response) <- do
    body <-
      case mSrcFile of
        Nothing -> pure mempty
        Just srcFile -> LazyByteString.readFile srcFile
    httpPost manager (baseUrl ++ "/.resource") headers body

  case response of
    PreconditionFailed ->
      error "impossible"
    NotFound ->
      error "impossible"
    Ok{} ->
      error "impossible"
    Conflict -> do
      putStrLn $ "error: " ++ renderResourceId resourceId ++ " already exists"
      exitFailure
    Created -> do
      putStrLn $ "created " ++ renderResourceId resourceId

update :: String -> Maybe CertificateStore -> FilePath -> ResourceId -> IO ()
update baseUrl mCertificateStore srcFile resourceId = do
  manager <- httpManager mCertificateStore

  (_responseHeaders, response) <- do
    body <- LazyByteString.readFile srcFile
    let headers = resourceIdHeaders resourceId
    httpPut manager (baseUrl ++ "/.resource") headers body

  case response of
    PreconditionFailed ->
      error "impossible"
    NotFound ->
      error "impossible"
    Created{} ->
      error "impossible"
    Conflict -> do
      error "impossible"
    Ok{} -> do
      putStrLn $ "updated " ++ renderResourceId resourceId

edit :: String -> Maybe CertificateStore -> ResourceId -> IO ()
edit baseUrl mCertificateStore resourceId = do
  dataHome <- getDataHome
  editor <- getEditor

  manager <- httpManager mCertificateStore

  let resourceDirLocal = dataHome </> "blog" </> resourceType resourceId
  createDirectoryIfMissing True resourceDirLocal

  let resourcePathLocal = resourceDirLocal </> resourceName resourceId

  mLocalModificationTime <-
    fmap Just (getModificationTime resourcePathLocal)
      `catch` \err ->
        if isDoesNotExistError err
          then pure Nothing
          else throwIO err
  (_responseHeaders, rBody) <- do
    let
      headers =
        resourceIdHeaders resourceId
          ++ [ ( hIfUnmodifiedSince
               , fromString $ formatTime defaultTimeLocale rfc822DateFormat localModificationTime
               )
             | Just localModificationTime <- pure $ mLocalModificationTime
             ]
    httpGet
      manager
      (baseUrl ++ "/.resource/" ++ resourceType resourceId ++ "/" ++ resourceName resourceId)
      headers

  updated <-
    case rBody of
      Conflict ->
        error "impossible"
      Created ->
        error "impossible"
      PreconditionFailed -> do
        putStrLn "error: the local copy of this resource is out of date"
        exitFailure
      NotFound -> do
        when (isNothing mLocalModificationTime) $ writeFile resourcePathLocal ""
        pure False
      Ok body -> do
        when (isNothing mLocalModificationTime) $ LazyByteString.writeFile resourcePathLocal body
        pure True

  callProcess editor [resourcePathLocal]

  (_responseHeaders, response) <- do
    let url = baseUrl ++ "/.resource"
    if updated
      then do
        localModificationTime <- getModificationTime resourcePathLocal
        let
          headers =
            resourceIdHeaders resourceId
              ++ [
                   ( hIfUnmodifiedSince
                   , fromString $ formatTime defaultTimeLocale rfc822DateFormat localModificationTime
                   )
                 ]
        body <- LazyByteString.readFile resourcePathLocal
        httpPut manager url headers body
      else do
        let headers = resourceIdHeaders resourceId
        body <- LazyByteString.readFile resourcePathLocal
        httpPost manager url headers body

  case response of
    Conflict ->
      error "impossible"
    NotFound -> do
      error "impossible"
    PreconditionFailed -> do
      putStrLn "error: the server has a newer copy of the resource (update aborted)"
      exitFailure
    Created -> do
      putStrLn $ "created " ++ renderResourceId resourceId
      removeFile resourcePathLocal
    Ok _body -> do
      putStrLn $ "updated " ++ renderResourceId resourceId
      removeFile resourcePathLocal
