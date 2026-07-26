{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blog.Metadata
  ( lookupResourceMetadata
  , resourceMetadataDecoder
  , parseResourceMetadata
  , Path
  , pathToList
  , PathItem (..)
  , pathItem
  , metadataValueToTempleExpr
  )
where

import Blog (MetadataValue (..), ResourceType, cfgMetadata, metadataTypeDecoder, resourceTypeConfig, renderResourceId, resourceTypeName)
import Blog.Diagnostic (DiagnosticReports, tomlResult)
import Control.Exception (catch, throwIO)
import Control.Monad.Error.Class (MonadError)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import IO (WithCallStack (..))
import qualified IO
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import qualified Temple
import qualified Toml
import Blog (ResourceId(..))
import qualified Data.Text as Text
import Data.String (fromString)

lookupResourceMetadata ::
  -- | Resource type directory
  FilePath ->
  -- | Resource name
  String ->
  IO (Maybe LazyByteString)
lookupResourceMetadata resTy resName =
  fmap Just (IO.readFile $ resTy </> (resName ++ ".d") </> "metadata")
    `catch` \err@(WithCallStack _cs err') -> if isDoesNotExistError err' then pure Nothing else throwIO err

resourceMetadataDecoder ::
  ResourceType ->
  Toml.Decoder (Map Text MetadataValue)
resourceMetadataDecoder resTy =
  Map.fromList
    <$> traverse
      ( \(key, type_) ->
          (,) key <$> Toml.key key (metadataTypeDecoder type_)
      )
      (Map.toList . cfgMetadata $ resourceTypeConfig resTy)

parseResourceMetadata ::
  MonadError DiagnosticReports m =>
  ResourceType ->
  -- | Resource name
  String ->
  LazyByteString ->
  m (Map Text MetadataValue)
parseResourceMetadata resTy resName content = do
  let decoder = resourceMetadataDecoder resTy
  let resourceFile = fromString $ "(" ++ renderResourceId (ResourceId (Text.unpack $ resourceTypeName resTy) resName) ++ ")"
  let content' = LazyByteString.toStrict content
  toml <- tomlResult resourceFile content $ Toml.parse content'
  tomlResult resourceFile content $ Toml.decode toml decoder

newtype Path = Path [PathItem]
  deriving (Show, Semigroup, Monoid)

pathToList :: Path -> [PathItem]
pathToList (Path xs) = xs

pathItem :: PathItem -> Path
pathItem = Path . pure

data PathItem
  = ArrayItem !Int
  | RecordField !Text
  | ConstructorArg !Text !Int
  deriving Show

metadataValueToTempleExpr :: Path -> MetadataValue -> Temple.Expr Path
metadataValueToTempleExpr _path (VString s) =
  Temple.String [Temple.PartText s]
metadataValueToTempleExpr path (VList xs) =
  Temple.Array $
    fmap
      ( \(ix, item) ->
          let path' = path <> pathItem (ArrayItem ix)
          in Temple.Located
               path'
               (metadataValueToTempleExpr path' item)
      )
      (zip [0 ..] xs)
