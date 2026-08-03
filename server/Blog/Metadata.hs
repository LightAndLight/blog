{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blog.Metadata
  ( resourceMetadataDecoder
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
  , ResourceConfig
  , ResourceId (..)
  , cfgMetadata
  , metaCfgDefault
  , metaCfgOptional
  , metaCfgType
  , metadataTypeDecoder
  , renderResourceId
  )
import Blog.Diagnostic (DiagnosticReports)
import Blog.Error (tomlResult)
import Control.Monad.Error.Class (MonadError)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Temple
import qualified Toml

resourceMetadataDecoder ::
  ResourceConfig ->
  Toml.Decoder (Map Text MetadataValue)
resourceMetadataDecoder config =
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
      (Map.toList $ cfgMetadata config)

parseResourceMetadata ::
  MonadError DiagnosticReports m =>
  ResourceConfig ->
  -- | Resource type
  String ->
  -- | Resource name
  String ->
  LazyByteString ->
  m (Map Text MetadataValue)
parseResourceMetadata config resTyName resName content = do
  let decoder = resourceMetadataDecoder config
  let resourceFile =
        fromString $
          "(" ++ renderResourceId (ResourceId resTyName resName) ++ ")"
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
