{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blog.Metadata
  ( lookupResourceMetadata
  , resourceMetadataDecoder
  , parseResourceMetadata
  , Path
  , pathToList
  , pathUncons
  , renderPath
  , PathItem (..)
  , pathItem
  , metadataValueToTempleExpr
  )
where

import Blog
  ( MetadataValue (..)
  , ResourceId (..)
  , ResourceType
  , cfgMetadata
  , metaCfgDefault
  , metaCfgOptional
  , metaCfgType
  , metadataTypeDecoder
  , renderResourceId
  , resourceTypeConfig
  , resourceTypeName
  )
import Blog.Diagnostic (DiagnosticReports, tomlResult)
import Control.Exception (catch, throwIO)
import Control.Monad.Error.Class (MonadError)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import IO (WithCallStack (..))
import qualified IO
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import qualified Temple
import qualified Toml

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
      ( \(key, metaCfg) ->
          (,) key
            <$> case (metaCfgOptional metaCfg, metaCfgDefault metaCfg) of
              (False, _) ->
                Toml.key key (metadataTypeDecoder $ metaCfgType metaCfg)
              (True, Nothing) ->
                maybe
                  (VConstructor (fromString "None") [])
                  (VConstructor (fromString "Some") . pure)
                  <$> Toml.optionalKey key (metadataTypeDecoder $ metaCfgType metaCfg)
              (True, Just def) ->
                fromMaybe def
                  <$> Toml.optionalKey key (metadataTypeDecoder $ metaCfgType metaCfg)
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
  let resourceFile =
        fromString $
          "(" ++ renderResourceId (ResourceId (Text.unpack $ resourceTypeName resTy) resName) ++ ")"
  let content' = LazyByteString.toStrict content
  toml <- tomlResult resourceFile content $ Toml.parse content'
  tomlResult resourceFile content $ Toml.decode toml decoder

newtype Path = Path [PathItem]
  deriving (Show, Semigroup, Monoid)

pathToList :: Path -> [PathItem]
pathToList (Path xs) = xs

pathUncons :: Path -> Maybe (PathItem, Path)
pathUncons (Path []) = Nothing
pathUncons (Path (x : xs)) = Just (x, Path xs)

renderPath :: Path -> String
renderPath (Path []) = "(root)"
renderPath (Path ps) = go ps
  where
    go [] = ""
    go (p' : ps') =
      ( case p' of
          RecordField name -> Text.unpack name
          ArrayItem ix -> "[" ++ show ix ++ "]"
          ConstructorArg _name ix -> show ix
      )
        ++ ( case ps' of
               RecordField{} : _ -> "."
               ConstructorArg{} : _ -> "."
               _ -> ""
           )
        ++ go ps'

pathItem :: PathItem -> Path
pathItem = Path . pure

data PathItem
  = ArrayItem !Int
  | RecordField !Text
  | ConstructorArg !Text !Int
  deriving (Show)

metadataValueToTempleExpr :: Path -> MetadataValue -> Temple.Expr Path
metadataValueToTempleExpr _path VTrue = Temple.Bool True
metadataValueToTempleExpr _path VFalse = Temple.Bool False
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
metadataValueToTempleExpr path (VConstructor name args) =
  Temple.Constructor name $
    fmap
      ( \(ix, arg) ->
          let path' = path <> pathItem (ConstructorArg name ix)
          in Temple.Located path' $ metadataValueToTempleExpr path' arg
      )
      (zip [0 ..] args)
