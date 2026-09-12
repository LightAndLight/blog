module Main (main) where

import Blog (ResourceId (..), propertiesPart, renderResourceId, resourceIdParser)
import qualified Blog.ID as ID
import Blog.Password (hashPassword)
import Blog.Session (sessionIdCookieName)
import Control.Applicative (many, optional, (<**>), (<|>))
import Control.Exception (bracket, catch, finally, throwIO)
import Control.Monad (unless, void, when)
import Control.Monad.Catch (ExitCase (..), generalBracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Foldable (for_)
import Data.List (find)
import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import Data.Time.Format (defaultTimeLocale, formatTime, rfc822DateFormat)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.X509.CertificateStore (CertificateStore, readCertificateStore)
import GHC.Stack (HasCallStack)
import Network.Connection (TLSSettings (..))
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Client.TLS as Http.Tls
import Network.HTTP.Types.Header (RequestHeaders, ResponseHeaders, hContentType, hIfUnmodifiedSince)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.TLS as Tls
import Network.TLS.Extra.Cipher (ciphersuite_default)
import qualified Options.Applicative as Options
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getModificationTime
  , listDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.IO.Error (isDoesNotExistError)
import System.Posix.IO (OpenMode (..), closeFd, defaultFileFlags, openFd)
import System.Posix.Terminal
  ( TerminalMode (..)
  , TerminalState (..)
  , getTerminalAttributes
  , setTerminalAttributes
  , withoutMode
  )
import System.Process (callProcess)
import qualified Text.Diagnostic as Diagnostic
import qualified Text.Diagnostic.Sage
import qualified Text.Sage as Sage
import qualified Toml
import Web.FormUrlEncoded (urlEncodeAsFormStable)

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
  = Login
  | Begin
      -- | Defer rules until commit
      Bool
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
      -- | Transaction ID
      (Maybe String)
      -- | Source file
      (Maybe FilePath)
      -- | Properties to create
      [String]
      -- | ID of resource to create
      String
  | CreateAll
      -- | Transaction ID
      (Maybe String)
      -- | Source directory
      FilePath
      -- | Resource type to create
      String
  | Update
      -- | Transaction ID
      (Maybe String)
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
  | Import
      -- | Archive to import
      FilePath
  | SeedUser
      -- | Directory in which to create the user
      FilePath

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
      ( Options.command "login" (Options.info loginParser $ Options.progDesc "Authenticate with the server")
          <> Options.command "begin" (Options.info beginParser $ Options.progDesc "Begin a transaction")
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
          <> Options.command
            "import"
            (Options.info importParser $ Options.progDesc "Import an archive")
          <> Options.command
            "seed-user"
            (Options.info seedUserParser $ Options.progDesc "Generate a user locally")
      )
  where
    loginParser =
      pure Login

    beginParser =
      Begin
        <$> Options.switch (Options.long "defer" <> Options.help "Defer rules until commit")

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
              Options.long "transaction-id" <> Options.metavar "ID" <> Options.help "ID of transaction to update"
          )
        <*> optional
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
        <$> optional
          ( Options.strOption $
              Options.long "transaction-id" <> Options.metavar "ID" <> Options.help "ID of transaction to update"
          )
        <*> Options.strOption
          (Options.long "from" <> Options.short 'f' <> Options.metavar "DIR" <> Options.help "Source directory")
        <*> Options.strArgument
          (Options.metavar "TYPE" <> Options.help "Type of resource to create")

    updateParser =
      Update
        <$> optional
          ( Options.strOption $
              Options.long "transaction-id" <> Options.metavar "ID" <> Options.help "ID of transaction to update"
          )
        <*> optional
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

    importParser =
      Import
        <$> Options.strArgument
          (Options.metavar "FILE" <> Options.help "Archive to import")

    seedUserParser =
      SeedUser
        <$> Options.strArgument
          (Options.metavar "DIR" <> Options.help "Directory in which to create the user")

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
  manager <- httpManager mCertificateStore
  case cliCommand cli of
    Login ->
      login baseUrl manager
    Begin defer ->
      begin baseUrl manager defer
    Commit xactId ->
      commit baseUrl manager $ fromString xactId
    Rollback xactId ->
      rollback baseUrl manager $ fromString xactId
    ListTransactions ->
      listTransactions baseUrl manager
    View viewTarget resourceId -> do
      resourceId' <- parseResourceId resourceId
      view baseUrl manager viewTarget resourceId'
    List resourceTyName ->
      list baseUrl manager resourceTyName
    Create mXactId mSrcFile properties resourceId -> do
      let mXactId' = fmap fromString mXactId
      resourceId' <- parseResourceId resourceId
      properties' <- parseProperties properties
      create baseUrl manager mXactId' mSrcFile properties' resourceId'
    CreateAll mXactId srcDir resTy -> do
      let mXactId' = fmap fromString mXactId
      createAll baseUrl manager mXactId' srcDir resTy
    Update mXactId mSrcFile properties resourceId -> do
      let mXactId' = fmap fromString mXactId
      resourceId' <- parseResourceId resourceId
      properties' <- parseProperties properties
      update baseUrl manager mXactId' mSrcFile properties' resourceId'
    Edit resourceId -> do
      resourceId' <- parseResourceId resourceId
      edit baseUrl manager resourceId'
    RefreshAll resTy ->
      refreshAll baseUrl manager resTy
    Import path ->
      import_ baseUrl manager path
    SeedUser dir ->
      seedUser dir

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
  deriving (Show)

expectOk :: (HasCallStack, Show a) => Response a -> IO a
expectOk (Ok a) = pure a
expectOk x = unexpected x

unexpected :: (HasCallStack, Show a) => Response a -> IO b
unexpected x = error $ "unexpected response: " ++ show x

http ::
  Http.Manager ->
  -- | URL
  String ->
  -- | Method
  ByteString ->
  RequestHeaders ->
  LazyByteString ->
  IO (ResponseHeaders, Response LazyByteString)
http manager baseUrl method headers body = do
  (_cookies, responseHeaders, response) <- httpWithCookies manager mempty baseUrl method headers body
  pure (responseHeaders, response)

httpWithCookies ::
  Http.Manager ->
  [Http.Cookie] ->
  -- | URL
  String ->
  -- | Method
  ByteString ->
  RequestHeaders ->
  LazyByteString ->
  IO ([Http.Cookie], ResponseHeaders, Response LazyByteString)
httpWithCookies manager cookies url method headers body = do
  request <- do
    request <- Http.parseRequest url
    pure
      request
        { Http.cookieJar = Just $ Http.createCookieJar cookies
        , Http.method = method
        , Http.requestHeaders = headers
        , Http.requestBody = Http.RequestBodyLBS body
        }
  response <- Http.httpLbs request manager
  let body' = Http.responseBody response
  let cookies' = Http.destroyCookieJar $ Http.responseCookieJar response
  case statusCode $ Http.responseStatus response of
    404 -> pure (cookies', Http.responseHeaders response, NotFound body')
    409 -> pure (cookies', Http.responseHeaders response, Conflict)
    412 -> pure (cookies', Http.responseHeaders response, PreconditionFailed)
    200 -> pure (cookies', Http.responseHeaders response, Ok body')
    201 -> pure (cookies', Http.responseHeaders response, Created body')
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

httpPostWithCookies ::
  Http.Manager ->
  [Http.Cookie] ->
  -- | URL
  String ->
  RequestHeaders ->
  LazyByteString ->
  IO ([Http.Cookie], ResponseHeaders, Response LazyByteString)
httpPostWithCookies manager cookies url = httpWithCookies manager cookies url (fromString "POST")

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
  dir <-
    case mDataHome of
      Just dataHome ->
        pure dataHome
      Nothing -> do
        home <- requireEnv "HOME"
        pure $ home </> ".local" </> "share"
  pure $ dir </> "blog"

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

requestInput ::
  -- | Prompt
  String ->
  IO String
requestInput prompt = putStr prompt *> hFlush stdout *> getLine

requestInputSensitive ::
  -- | Prompt
  String ->
  IO String
requestInputSensitive prompt =
  bracket (openFd "/dev/tty" ReadWrite defaultFileFlags) closeFd $ \tty -> do
    attrs <- getTerminalAttributes tty
    setTerminalAttributes tty (withoutMode attrs EnableEcho) Immediately

    (putStr prompt *> hFlush stdout *> getLine)
      `finally` (setTerminalAttributes tty attrs Immediately <* putChar '\n')

login :: String -> Http.Manager -> IO ()
login baseUrl manager = do
  username <- requestInput "username: "
  password <- requestInputSensitive "password: "

  let headers = [(hContentType, fromString "application/xxx-form-urlencoded")]
  let body = urlEncodeAsFormStable [("username", username), ("password", password)]
  (cookies, _responseHeaders, response) <-
    httpPostWithCookies manager [] (baseUrl ++ "/.login") headers body
  body' <- expectOk response

  case find ((fromString sessionIdCookieName ==) . Http.cookie_name) cookies of
    Nothing -> do
      putStrLn $ "error: log in failed (no session cookie received)"
      exitFailure
    Just sessionCookie -> do
      dataHome <- getDataHome
      let dir = dataHome </> ":session"

      do
        exists <- doesDirectoryExist dir
        when exists $ removeDirectoryRecursive dir
      createDirectoryIfMissing True dir

      ByteString.writeFile (dir </> "id") $ Http.cookie_value sessionCookie
      writeFile (dir </> "expires") $ iso8601Show (Http.cookie_expiry_time sessionCookie)
      ByteString.Lazy.Char8.putStrLn body'

begin :: String -> Http.Manager -> Bool -> IO ()
begin baseUrl manager defer = do
  xactId <- beginTransaction baseUrl manager defer
  ByteString.Char8.putStrLn $ fromString "began " <> xactId

commit :: String -> Http.Manager -> ByteString -> IO ()
commit baseUrl manager xactId = do
  commitTransaction baseUrl manager xactId
  ByteString.Char8.putStrLn $ fromString "committed " <> xactId

rollback :: String -> Http.Manager -> ByteString -> IO ()
rollback baseUrl manager xactId = do
  rollbackTransaction baseUrl manager xactId
  ByteString.Char8.putStrLn $ fromString "rolled back " <> xactId

listTransactions :: String -> Http.Manager -> IO ()
listTransactions baseUrl manager = do
  (_responseHeaders, response) <- httpGet manager (baseUrl ++ "/.transaction") []
  ByteString.Lazy.Char8.putStr =<< expectOk response

view ::
  String ->
  Http.Manager ->
  ViewTarget ->
  ResourceId ->
  IO ()
view baseUrl manager viewTarget resourceId = do
  dataHome <- getDataHome
  pager <- getPager

  let
    resourceDirLocal =
      case viewTarget of
        ViewMetadata ->
          dataHome </> resourceType resourceId </> propertiesPart (resourceName resourceId)
        ViewProperty _propName ->
          dataHome </> resourceType resourceId </> propertiesPart (resourceName resourceId)
        ViewContent -> dataHome </> resourceType resourceId
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
    PreconditionFailed -> do
      putStrLn "error: the local copy of this resource is out of date"
      exitFailure
    NotFound body -> do
      ByteString.Lazy.Char8.putStrLn body
      exitFailure
    Ok body -> do
      LazyByteString.writeFile resourcePathLocal body
    _ ->
      unexpected rBody

  callProcess pager [resourcePathLocal] `finally` removeFile resourcePathLocal

list ::
  String ->
  Http.Manager ->
  -- | Resource type
  String ->
  IO ()
list baseUrl manager resourceTyName = do
  dataHome <- getDataHome
  pager <- getPager

  let resourceDirLocal = dataHome </> "resource" </> (resourceTyName ++ ":temp")
  createDirectoryIfMissing True resourceDirLocal

  let resourcePathLocal = resourceDirLocal </> "list"

  (_responseHeaders, rBody) <- do
    let
      headers = []
      url = baseUrl ++ "/.resource/" ++ resourceTyName

    httpGet manager url headers

  case rBody of
    NotFound{} -> do
      putStrLn $ "error: resource type " ++ resourceTyName ++ " not found"
      exitFailure
    Ok body -> do
      LazyByteString.writeFile resourcePathLocal body
    _ ->
      unexpected rBody

  callProcess pager [resourcePathLocal] `finally` removeFile resourcePathLocal

withTransaction' :: String -> Http.Manager -> Maybe ByteString -> (ByteString -> IO a) -> IO a
withTransaction' baseUrl manager Nothing f = withTransaction baseUrl manager f
withTransaction' _baseUrl _manager (Just xactId) f = f xactId

create ::
  -- | Base URL
  String ->
  Http.Manager ->
  -- | Transaction ID
  Maybe ByteString ->
  -- | Source file
  Maybe FilePath ->
  [Property] ->
  ResourceId ->
  IO ()
create baseUrl manager mXactId mSrcFile properties resourceId = do
  let
    withTransaction'' f
      | isNothing mXactId && not (null properties) = withTransaction baseUrl manager (f . Just)
      | otherwise = f mXactId

  withTransaction'' $ \mXactId' -> do
    do
      let headers = resourceIdHeaders resourceId ++ foldMap transactionIdHeaders mXactId'
      (_responseHeaders, response) <- do
        body <-
          case mSrcFile of
            Nothing -> pure mempty
            Just srcFile -> LazyByteString.readFile srcFile
        httpPost manager (baseUrl ++ "/.resource") headers body

      case response of
        Conflict -> do
          putStrLn $ "error: " ++ renderResourceId resourceId ++ " already exists"
          exitFailure
        Created a -> do
          ByteString.Lazy.Char8.putStrLn a
        _ ->
          unexpected response

    unless (null properties) $ do
      let headers = foldMap transactionIdHeaders mXactId'
      (_responseHeaders, response) <- do
        let body = renderProperties properties
        httpPatch
          manager
          (baseUrl ++ "/.resource/" ++ resourceIdPath resourceId ++ "/property")
          headers
          body

      ByteString.Lazy.Char8.putStrLn =<< expectOk response

beginTransaction ::
  -- | Base URL
  String ->
  Http.Manager ->
  -- | Defer rules until commit
  Bool ->
  -- | Transaction ID
  IO ByteString
beginTransaction baseUrl manager defer = do
  let headers = [(fromString "X-Blog-Transaction-Defer", fromString "true") | defer]
  (_responseHeaders, response) <- httpPost manager (baseUrl ++ "/.transaction/begin") headers mempty

  LazyByteString.toStrict <$> expectOk response

commitTransaction ::
  -- | Base URL
  String ->
  Http.Manager ->
  -- | Transaction ID
  ByteString ->
  IO ()
commitTransaction baseUrl manager xactId = do
  let headers = transactionIdHeaders xactId
  (_responseHeaders, response) <- httpPost manager (baseUrl ++ "/.transaction/commit") headers mempty

  void $ expectOk response

rollbackTransaction ::
  String ->
  Http.Manager ->
  -- | Transaction ID
  ByteString ->
  IO ()
rollbackTransaction baseUrl manager xactId = do
  let headers = transactionIdHeaders xactId
  (_responseHeaders, response) <-
    httpPost manager (baseUrl ++ "/.transaction/rollback") headers mempty

  void $ expectOk response

-- | Run an action in a transaction, committing on success and rolling back on exception/failure.
withTransaction ::
  -- | Base URL
  String ->
  Http.Manager ->
  {-| Arguments:

  * Transaction ID
  -}
  (ByteString -> IO b) ->
  IO b
withTransaction baseUrl manager f = do
  (a, ()) <- generalBracket (beginTransaction baseUrl manager False) exit $ f
  pure a
  where
    exit xactId (ExitCaseSuccess _a) = commitTransaction baseUrl manager xactId
    exit xactId (ExitCaseException _err) = rollbackTransaction baseUrl manager xactId
    exit xactId ExitCaseAbort = rollbackTransaction baseUrl manager xactId

createAll ::
  -- | Base URL
  String ->
  Http.Manager ->
  -- | Transaction ID
  Maybe ByteString ->
  FilePath ->
  String ->
  IO ()
createAll baseUrl manager mXactId srcDir resTy = do
  entries <- listDirectory srcDir
  when (null entries) $ do
    putStrLn $ "error: " ++ srcDir ++ " is empty"
    exitFailure

  withTransaction' baseUrl manager mXactId $ \xactId ->
    for_ entries $ \entry -> do
      let path = srcDir </> entry
      isFile <- doesFileExist path
      if isFile
        then do
          let resourceId = ResourceId resTy entry
          create baseUrl manager (Just xactId) (Just path) [] resourceId
        else do
          putStrLn $ "warning: " ++ path ++ " is not a file (ignoring)"

update ::
  -- | Base URL
  String ->
  Http.Manager ->
  -- | Transaction ID
  Maybe ByteString ->
  -- | Source file
  Maybe FilePath ->
  -- | Properties to set
  [Property] ->
  ResourceId ->
  IO ()
update baseUrl manager mXactId mSrcFile properties resourceId = do
  let
    doBody mXactId' srcFile = do
      (_responseHeaders, response) <- do
        body <- LazyByteString.readFile srcFile
        let headers = foldMap transactionIdHeaders mXactId' ++ resourceIdHeaders resourceId
        httpPut manager (baseUrl ++ "/.resource") headers body

      ByteString.Lazy.Char8.putStrLn =<< expectOk response

    doProperties mXactId' ps = do
      (_responseHeaders, response) <- do
        let body = renderProperties ps
        let headers = foldMap transactionIdHeaders mXactId' ++ resourceIdHeaders resourceId
        httpPatch
          manager
          (baseUrl ++ "/.resource/" ++ resourceIdPath resourceId ++ "/property")
          headers
          body

      ByteString.Lazy.Char8.putStrLn =<< expectOk response

  case (mSrcFile, properties) of
    (Nothing, []) -> do
      putStrLn "nothing to do"
    (Nothing, _ : _) -> do
      doProperties mXactId properties
    (Just srcFile, []) -> do
      doBody mXactId srcFile
    (Just srcFile, _ : _) ->
      withTransaction' baseUrl manager mXactId $ \xactId -> do
        doBody (Just xactId) srcFile
        doProperties (Just xactId) properties

edit :: String -> Http.Manager -> ResourceId -> IO ()
edit baseUrl manager resourceId = do
  dataHome <- getDataHome
  editor <- getEditor

  let resourceDirLocal = dataHome </> resourceType resourceId
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
      PreconditionFailed -> do
        putStrLn "error: the local copy of this resource is out of date"
        exitFailure
      NotFound{} -> do
        when (isNothing mLocalModificationTime) $ writeFile resourcePathLocal ""
        pure False
      Ok body -> do
        when (isNothing mLocalModificationTime) $ LazyByteString.writeFile resourcePathLocal body
        pure True
      _ ->
        unexpected rBody

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
    PreconditionFailed -> do
      putStrLn "error: the server has a newer copy of the resource (update aborted)"
      exitFailure
    Created body -> do
      ByteString.Lazy.Char8.putStrLn body
      removeFile resourcePathLocal
    Ok body -> do
      ByteString.Lazy.Char8.putStrLn body
      removeFile resourcePathLocal
    _ ->
      unexpected response

refreshAll :: String -> Http.Manager -> String -> IO ()
refreshAll baseUrl manager resTy = do
  let headers = []
  (_responseHeaders, response) <- do
    httpRefresh manager (baseUrl ++ "/.resource/" ++ resTy) headers

  ByteString.Lazy.Char8.putStrLn =<< expectOk response

import_ :: String -> Http.Manager -> FilePath -> IO ()
import_ baseUrl manager path = do
  let headers = []
  (_responseHeaders, response) <- do
    content <- LazyByteString.readFile path
    httpPut manager (baseUrl ++ "/.import") headers content

  ByteString.Lazy.Char8.putStrLn =<< expectOk response

seedUser ::
  -- | Directory in which to create the user
  FilePath ->
  IO ()
seedUser dir = do
  username <- requestInput "username: "

  password <- requestInputSensitive "password: "
  password' <- requestInputSensitive "confirm password: "

  unless (password == password') $ do
    putStrLn "error: passwords don't match"
    exitFailure

  salt <- ID.generate

  createDirectoryIfMissing True dir
  let file = dir </> username
  Text.writeFile file $
    hashPassword (ByteString.pack $ ID.toBytes salt) (fromString password)

  putStrLn $ "created " ++ file
