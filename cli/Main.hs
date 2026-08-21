module Main (main) where

import Blog (ResourceId (..), propertiesPart, renderResourceId, resourceIdParser)
import Control.Applicative (many, optional, (<**>), (<|>))
import Control.Exception (catch, finally, throwIO)
import Control.Monad (unless, when)
import Control.Monad.Catch (ExitCase (..), generalBracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Foldable (for_)
import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
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
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getModificationTime
  , listDirectory
  , removeFile
  )
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Process (callProcess)
import qualified Text.Diagnostic as Diagnostic
import qualified Text.Diagnostic.Sage
import qualified Text.Sage as Sage
import qualified Toml

data Cli
  = Cli
  { cliBaseUrl :: String
  -- ^ Base URL of blog server
  , cliCaCert :: !(Maybe FilePath)
  -- ^ CA certificate
  , cliCommand :: Command
  }

data ViewTarget
  = ViewMetadata
  | ViewProperty String
  | ViewContent

data Command
  = Begin
  | Commit
      -- | Transaction ID
      String
  | Rollback
      -- | Transaction ID
      String
  | ListTransactions
  | View
      -- | What to view
      ViewTarget
      -- | ID of resource to view
      String
  | List
      -- | Resource type
      String
  | Create
      -- | Source file
      (Maybe FilePath)
      -- | Properties to create
      [String]
      -- | ID of resource to create
      String
  | CreateAll
      -- | Source directory
      FilePath
      -- | Resource type to create
      String
  | Update
      -- | Source file
      (Maybe FilePath)
      -- | Properties to update
      [String]
      -- | ID of resource to update
      String
  | Edit
      -- | ID of resource to edit
      String
  | RefreshAll
      -- | Resource type to refresh
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
      ( Options.command "begin" (Options.info beginParser $ Options.progDesc "Begin a transaction")
          <> Options.command "commit" (Options.info commitParser $ Options.progDesc "Commit a transaction")
          <> Options.command
            "rollback"
            (Options.info rollbackParser $ Options.progDesc "Roll back a transaction")
          <> Options.command
            "list-transactions"
            (Options.info listTransactionsParser $ Options.progDesc "List uncommitted transactions")
          <> Options.command "view" (Options.info viewParser $ Options.progDesc "View a resource")
          <> Options.command
            "list"
            (Options.info listParser $ Options.progDesc "List resources of a specific type")
          <> Options.command "create" (Options.info createParser $ Options.progDesc "Create an empty resource")
          <> Options.command
            "create-all"
            (Options.info createAllParser $ Options.progDesc "Create multiple resources")
          <> Options.command "update" (Options.info updateParser $ Options.progDesc "Update a resource")
          <> Options.command "edit" (Options.info editParser $ Options.progDesc "Edit a resource")
          <> Options.command
            "refresh-all"
            (Options.info refreshAllParser $ Options.progDesc "Mark resources as changed")
      )
  where
    beginParser =
      pure Begin

    commitParser =
      Commit
        <$> Options.strArgument
          (Options.metavar "ID" <> Options.help "ID of transaction to commit")

    rollbackParser =
      Rollback
        <$> Options.strArgument
          (Options.metavar "ID" <> Options.help "ID of transaction to roll back")

    listTransactionsParser =
      pure ListTransactions

    viewParser =
      View
        <$> ( Options.flag ViewContent ViewMetadata (Options.long "metadata" <> Options.help "View metadata only")
                <|> ViewProperty
                  <$> Options.strOption
                    (Options.long "property" <> Options.metavar "NAME" <> Options.help "View a property only")
                <|> pure ViewContent
            )
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
        <*> many
          ( Options.strOption $
              Options.long "property"
                <> Options.short 'p'
                <> Options.metavar "NAME=VALUE"
                <> Options.help "Property to create"
          )
        <*> Options.strArgument
          (Options.metavar "RESOURCE" <> Options.help "ID of resource to create (format: `TYPE:NAME`)")

    createAllParser =
      CreateAll
        <$> Options.strOption
          (Options.long "from" <> Options.short 'f' <> Options.metavar "DIR" <> Options.help "Source directory")
        <*> Options.strArgument
          (Options.metavar "TYPE" <> Options.help "Type of resource to create")

    updateParser =
      Update
        <$> optional
          ( Options.strOption $
              Options.long "from" <> Options.short 'f' <> Options.metavar "FILE" <> Options.help "Source file"
          )
        <*> many
          ( Options.strOption $
              Options.long "property"
                <> Options.short 'p'
                <> Options.metavar "NAME=VALUE"
                <> Options.help "Property to update"
          )
        <*> Options.strArgument
          (Options.metavar "RESOURCE" <> Options.help "ID of resource to create (format: `TYPE:NAME`)")

    editParser =
      Edit
        <$> Options.strArgument
          (Options.metavar "RESOURCE" <> Options.help "ID of resource to edit (format: `TYPE:NAME`)")

    refreshAllParser =
      RefreshAll
        <$> Options.strArgument
          (Options.metavar "TYPE" <> Options.help "Type of resource to refresh")

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

data Property
  = Property Text Toml.TomlValue

renderProperties :: [Property] -> LazyByteString
renderProperties = foldMap ((<> fromString "\n") . renderProperty)
  where
    renderProperty :: Property -> LazyByteString
    renderProperty (Property name value) =
      Text.Lazy.Encoding.encodeUtf8 . Text.Lazy.Builder.toLazyText $
        Toml.keyPrinter name value

parseProperties :: [String] -> IO [Property]
parseProperties = traverse (uncurry parseProperty) . zip [0 ..]
  where
    parseProperty :: Int -> String -> IO Property
    parseProperty index input =
      case Sage.parse (propertyParser <* Sage.eof) $ fromString input of
        Left err -> do
          ByteString.Lazy.Char8.putStrLn
            . Diagnostic.render
              Diagnostic.defaultConfig
              (fromString $ "(property " ++ show index ++ ")")
              (fromString input)
            $ Text.Diagnostic.Sage.parseError err
          exitFailure
        Right x ->
          pure x

    propertyParser :: Sage.Parser Property
    propertyParser =
      (\(name, Toml.TomlKeyEntry _offset value) -> Property name $ Toml.locatedValue value)
        <$> Toml.keyParser

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
    Begin ->
      begin baseUrl mCertificateStore
    Commit xactId ->
      commit baseUrl mCertificateStore $ fromString xactId
    Rollback xactId ->
      rollback baseUrl mCertificateStore $ fromString xactId
    ListTransactions ->
      listTransactions baseUrl mCertificateStore
    View viewTarget resourceId -> do
      resourceId' <- parseResourceId resourceId
      view baseUrl mCertificateStore viewTarget resourceId'
    List resourceTyName ->
      list baseUrl mCertificateStore resourceTyName
    Create mSrcFile properties resourceId -> do
      resourceId' <- parseResourceId resourceId
      properties' <- parseProperties properties
      create baseUrl mCertificateStore Nothing mSrcFile properties' resourceId'
    CreateAll srcDir resTy ->
      createAll baseUrl mCertificateStore srcDir resTy
    Update mSrcFile properties resourceId -> do
      resourceId' <- parseResourceId resourceId
      properties' <- parseProperties properties
      update baseUrl mCertificateStore mSrcFile properties' resourceId'
    Edit resourceId -> do
      resourceId' <- parseResourceId resourceId
      edit baseUrl mCertificateStore resourceId'
    RefreshAll resTy ->
      refreshAll baseUrl mCertificateStore resTy

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
  = NotFound a
  | PreconditionFailed
  | Conflict
  | Created a
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
  let body' = Http.responseBody response
  case statusCode $ Http.responseStatus response of
    404 -> pure (Http.responseHeaders response, NotFound body')
    409 -> pure (Http.responseHeaders response, Conflict)
    412 -> pure (Http.responseHeaders response, PreconditionFailed)
    200 -> pure (Http.responseHeaders response, Ok body')
    201 -> pure (Http.responseHeaders response, Created body')
    _status -> do
      putStrLn $ ByteString.Lazy.Char8.unpack body'
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

httpPatch ::
  Http.Manager ->
  -- | URL
  String ->
  RequestHeaders ->
  LazyByteString ->
  IO (ResponseHeaders, Response LazyByteString)
httpPatch manager url = http manager url (fromString "PATCH")

httpRefresh ::
  Http.Manager ->
  -- | URL
  String ->
  RequestHeaders ->
  IO (ResponseHeaders, Response LazyByteString)
httpRefresh manager url headers = http manager url (fromString "REFRESH") headers mempty

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

resourceIdPath :: ResourceId -> String
resourceIdPath resourceId = resourceType resourceId ++ "/" ++ resourceName resourceId

transactionIdHeaders :: ByteString -> RequestHeaders
transactionIdHeaders xactId =
  [ (fromString "X-Blog-TransactionId", xactId)
  ]

begin :: String -> Maybe CertificateStore -> IO ()
begin baseUrl mCertificateStore = do
  xactId <- beginTransaction baseUrl mCertificateStore
  ByteString.Char8.putStrLn $ fromString "began " <> xactId

commit :: String -> Maybe CertificateStore -> ByteString -> IO ()
commit baseUrl mCertificateStore xactId = do
  commitTransaction baseUrl mCertificateStore xactId
  ByteString.Char8.putStrLn $ fromString "committed " <> xactId

rollback :: String -> Maybe CertificateStore -> ByteString -> IO ()
rollback baseUrl mCertificateStore xactId = do
  rollbackTransaction baseUrl mCertificateStore xactId
  ByteString.Char8.putStrLn $ fromString "rolled back " <> xactId

listTransactions :: String -> Maybe CertificateStore -> IO ()
listTransactions baseUrl mCertificateStore = do
  manager <- httpManager mCertificateStore

  (_responseHeaders, response) <- httpGet manager (baseUrl ++ "/.transaction") []

  case response of
    PreconditionFailed ->
      error "impossible"
    NotFound{} ->
      error "impossible"
    Created{} ->
      error "impossible"
    Conflict ->
      error "impossible"
    Ok a -> do
      ByteString.Lazy.Char8.putStr a

view ::
  String ->
  Maybe CertificateStore ->
  ViewTarget ->
  ResourceId ->
  IO ()
view baseUrl mCertificateStore viewTarget resourceId = do
  dataHome <- getDataHome
  pager <- getPager

  manager <- httpManager mCertificateStore

  let
    resourceDirLocal =
      case viewTarget of
        ViewMetadata ->
          dataHome </> "blog" </> resourceType resourceId </> propertiesPart (resourceName resourceId)
        ViewProperty _propName ->
          dataHome </> "blog" </> resourceType resourceId </> propertiesPart (resourceName resourceId)
        ViewContent -> dataHome </> "blog" </> resourceType resourceId
  createDirectoryIfMissing True resourceDirLocal

  let
    resourcePathLocal =
      case viewTarget of
        ViewMetadata -> resourceDirLocal </> "metadata"
        ViewProperty propName -> resourceDirLocal </> propName
        ViewContent -> resourceDirLocal </> resourceName resourceId

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

      url =
        case viewTarget of
          ViewMetadata ->
            baseUrl ++ "/.resource/" ++ resourceType resourceId ++ "/" ++ resourceName resourceId ++ "/metadata"
          ViewProperty propName ->
            baseUrl
              ++ "/.resource/"
              ++ resourceType resourceId
              ++ "/"
              ++ resourceName resourceId
              ++ "/property/"
              ++ propName
          ViewContent ->
            baseUrl ++ "/.resource/" ++ resourceType resourceId ++ "/" ++ resourceName resourceId

    httpGet manager url headers

  case rBody of
    Conflict ->
      error "impossible"
    Created{} ->
      error "impossible"
    PreconditionFailed -> do
      putStrLn "error: the local copy of this resource is out of date"
      exitFailure
    NotFound body -> do
      ByteString.Lazy.Char8.putStrLn body
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

  let resourceDirLocal = dataHome </> "blog" </> "resource" </> (resourceTyName ++ ":temp")
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
    Created{} ->
      error "impossible"
    PreconditionFailed -> do
      error "impossible"
    NotFound{} -> do
      putStrLn $ "error: resource type " ++ resourceTyName ++ " not found"
      exitFailure
    Ok body -> do
      LazyByteString.writeFile resourcePathLocal body

  callProcess pager [resourcePathLocal] `finally` removeFile resourcePathLocal

create ::
  -- | Base URL
  String ->
  Maybe CertificateStore ->
  -- | Transaction ID
  Maybe ByteString ->
  -- | Source file
  Maybe FilePath ->
  [Property] ->
  ResourceId ->
  IO ()
create baseUrl mCertificateStore mXactId mSrcFile properties resourceId = do
  manager <- httpManager mCertificateStore

  let
    withTransaction' f
      | isNothing mXactId && not (null properties) = withTransaction baseUrl mCertificateStore (f . Just)
      | otherwise = f mXactId

  withTransaction' $ \mXactId' -> do
    do
      let headers = resourceIdHeaders resourceId ++ foldMap transactionIdHeaders mXactId'
      (_responseHeaders, response) <- do
        body <-
          case mSrcFile of
            Nothing -> pure mempty
            Just srcFile -> LazyByteString.readFile srcFile
        httpPost manager (baseUrl ++ "/.resource") headers body

      case response of
        PreconditionFailed ->
          error "impossible"
        NotFound{} ->
          error "impossible"
        Ok{} ->
          error "impossible"
        Conflict -> do
          putStrLn $ "error: " ++ renderResourceId resourceId ++ " already exists"
          exitFailure
        Created a -> do
          ByteString.Lazy.Char8.putStrLn a

    unless (null properties) $ do
      let headers = foldMap transactionIdHeaders mXactId'
      (_responseHeaders, response) <- do
        let body = renderProperties properties
        httpPatch
          manager
          (baseUrl ++ "/.resource/" ++ resourceIdPath resourceId ++ "/property")
          headers
          body

      case response of
        PreconditionFailed ->
          error "impossible"
        NotFound{} ->
          error "impossible"
        Conflict{} ->
          error "impossible"
        Created{} -> do
          error "impossible"
        Ok a ->
          ByteString.Lazy.Char8.putStrLn a

beginTransaction ::
  String ->
  Maybe CertificateStore ->
  -- | Transaction ID
  IO ByteString
beginTransaction baseUrl mCertificateStore = do
  manager <- httpManager mCertificateStore

  (_responseHeaders, response) <- httpPost manager (baseUrl ++ "/.transaction/begin") [] mempty

  case response of
    PreconditionFailed ->
      error "impossible"
    NotFound{} ->
      error "impossible"
    Created{} ->
      error "impossible"
    Conflict ->
      error "impossible"
    Ok a -> do
      pure $ LazyByteString.toStrict a

commitTransaction ::
  String ->
  Maybe CertificateStore ->
  -- | Transaction ID
  ByteString ->
  IO ()
commitTransaction baseUrl mCertificateStore xactId = do
  manager <- httpManager mCertificateStore

  let headers = transactionIdHeaders xactId
  (_responseHeaders, response) <- httpPost manager (baseUrl ++ "/.transaction/commit") headers mempty

  case response of
    PreconditionFailed ->
      error "impossible"
    NotFound{} ->
      error "impossible"
    Conflict ->
      error "impossible"
    Created{} -> do
      error "impossible"
    Ok _ ->
      pure ()

rollbackTransaction ::
  String ->
  Maybe CertificateStore ->
  -- | Transaction ID
  ByteString ->
  IO ()
rollbackTransaction baseUrl mCertificateStore xactId = do
  manager <- httpManager mCertificateStore

  let headers = transactionIdHeaders xactId
  (_responseHeaders, response) <-
    httpPost manager (baseUrl ++ "/.transaction/rollback") headers mempty

  case response of
    PreconditionFailed ->
      error "impossible"
    NotFound{} ->
      error "impossible"
    Conflict ->
      error "impossible"
    Created{} -> do
      error "impossible"
    Ok _ ->
      pure ()

-- | Run an action in a transaction, committing on success and rolling back on exception/failure.
withTransaction ::
  -- | Base URL
  String ->
  Maybe CertificateStore ->
  {-| Arguments:

  * Transaction ID
  -}
  (ByteString -> IO b) ->
  IO b
withTransaction baseUrl mCertificateStore f = do
  (a, ()) <- generalBracket (beginTransaction baseUrl mCertificateStore) exit $ f
  pure a
  where
    exit xactId (ExitCaseSuccess _a) = commitTransaction baseUrl mCertificateStore xactId
    exit xactId (ExitCaseException _err) = rollbackTransaction baseUrl mCertificateStore xactId
    exit xactId ExitCaseAbort = rollbackTransaction baseUrl mCertificateStore xactId

createAll :: String -> Maybe CertificateStore -> FilePath -> String -> IO ()
createAll baseUrl mCertificateStore srcDir resTy = do
  entries <- listDirectory srcDir
  when (null entries) $ do
    putStrLn $ "error: " ++ srcDir ++ " is empty"
    exitFailure
  withTransaction baseUrl mCertificateStore $ \xactId ->
    for_ entries $ \entry -> do
      let path = srcDir </> entry
      isFile <- doesFileExist path
      if isFile
        then do
          let resourceId = ResourceId resTy entry
          create baseUrl mCertificateStore (Just xactId) (Just path) [] resourceId
        else do
          putStrLn $ "warning: " ++ path ++ " is not a file (ignoring)"

update ::
  -- | Base URL
  String ->
  Maybe CertificateStore ->
  -- | Source file
  Maybe FilePath ->
  -- | Properties to set
  [Property] ->
  ResourceId ->
  IO ()
update baseUrl mCertificateStore mSrcFile properties resourceId = do
  manager <- httpManager mCertificateStore

  let
    doBody mXactId srcFile = do
      (_responseHeaders, response) <- do
        body <- LazyByteString.readFile srcFile
        let headers = foldMap transactionIdHeaders mXactId ++ resourceIdHeaders resourceId
        httpPut manager (baseUrl ++ "/.resource") headers body

      case response of
        PreconditionFailed ->
          error "impossible"
        NotFound{} ->
          error "impossible"
        Created{} ->
          error "impossible"
        Conflict -> do
          error "impossible"
        Ok body -> do
          ByteString.Lazy.Char8.putStrLn body

    doProperties mXactId ps = do
      (_responseHeaders, response) <- do
        let body = renderProperties ps
        let headers = foldMap transactionIdHeaders mXactId ++ resourceIdHeaders resourceId
        httpPatch
          manager
          (baseUrl ++ "/.resource/" ++ resourceIdPath resourceId ++ "/property")
          headers
          body

      case response of
        PreconditionFailed ->
          error "impossible"
        NotFound{} ->
          error "impossible"
        Created{} ->
          error "impossible"
        Conflict -> do
          error "impossible"
        Ok body -> do
          ByteString.Lazy.Char8.putStrLn body

  case (mSrcFile, properties) of
    (Nothing, []) -> do
      putStrLn "nothing to do"
    (Nothing, _ : _) -> do
      doProperties Nothing properties
    (Just srcFile, []) -> do
      doBody Nothing srcFile
    (Just srcFile, _ : _) ->
      withTransaction baseUrl mCertificateStore $ \xactId -> do
        doBody (Just xactId) srcFile
        doProperties (Just xactId) properties

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
      Created{} ->
        error "impossible"
      PreconditionFailed -> do
        putStrLn "error: the local copy of this resource is out of date"
        exitFailure
      NotFound{} -> do
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
    NotFound{} -> do
      error "impossible"
    PreconditionFailed -> do
      putStrLn "error: the server has a newer copy of the resource (update aborted)"
      exitFailure
    Created body -> do
      ByteString.Lazy.Char8.putStrLn body
      removeFile resourcePathLocal
    Ok body -> do
      ByteString.Lazy.Char8.putStrLn body
      removeFile resourcePathLocal

refreshAll :: String -> Maybe CertificateStore -> String -> IO ()
refreshAll baseUrl mCertificateStore resTy = do
  manager <- httpManager mCertificateStore

  let headers = []
  (_responseHeaders, response) <- do
    httpRefresh manager (baseUrl ++ "/.resource/" ++ resTy) headers

  case response of
    PreconditionFailed ->
      error "impossible"
    NotFound{} ->
      error "impossible"
    Conflict{} -> do
      error "impossible"
    Created{} -> do
      error "impossible"
    Ok a ->
      ByteString.Lazy.Char8.putStrLn a
