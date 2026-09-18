{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Blog.Metadata
  ( MetadataKeyRequirement (..)
  , metadataKeyRequirement
  , resourceMetadataDecoder
  , parseResourceMetadata
  , metadataValueFromToml
  , renderMetadataValueToml

    -- * Value decoding
  , MetadataValueDecoder
  , runMetadataValueDecoder
  , list
  , text
  , value

    -- ** Errors
  , Part (..)
  , DecodeError (..)
  )
where

import Blog
  ( MetadataConfig
  , MetadataValue (..)
  , Name
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
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Foldable (fold)
import Data.List (find, intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified Toml

data MetadataKeyRequirement
  = -- | The key must be present
    Required
  | -- | The key may be omitted, in which case the default value is used instead.
    Defaulted !MetadataValue
  | -- | The key may be omitted. When missing, its value is @None@. When present, its value is @Some(value)@.
    Optional

metadataKeyRequirement :: MetadataConfig -> MetadataKeyRequirement
metadataKeyRequirement metaCfg =
  case (metaCfgOptional metaCfg, metaCfgDefault metaCfg) of
    (False, _) -> Required
    (True, Nothing) -> Optional
    (True, Just def) -> Defaulted def

resourceMetadataDecoder ::
  ResourceConfig ->
  Toml.Decoder (Map Text MetadataValue)
resourceMetadataDecoder config =
  Map.fromList
    <$> traverse
      ( \(key, metaCfg) ->
          let
            valueDecoder = metadataTypeDecoder $ metaCfgType metaCfg
          in
            (,) key
              <$> case metadataKeyRequirement metaCfg of
                Required ->
                  Toml.key key valueDecoder
                Optional ->
                  maybe
                    (VConstructor (fromString "None") [])
                    (VConstructor (fromString "Some") . pure)
                    <$> Toml.optionalKey key valueDecoder
                Defaulted def ->
                  fromMaybe def <$> Toml.optionalKey key valueDecoder
      )
      (Map.toList $ cfgMetadata config)

parseResourceMetadata ::
  MonadError DiagnosticReports m =>
  ResourceConfig ->
  -- | Resource type
  Name ->
  -- | Resource name
  Name ->
  ByteString ->
  m (Map Text MetadataValue)
parseResourceMetadata config resTyName resName content = do
  let decoder = resourceMetadataDecoder config
  let resourceFile =
        fromString $
          "(" ++ renderResourceId (ResourceId resTyName resName) ++ ")"
  toml <- tomlResult resourceFile content $ Toml.parse content
  tomlResult resourceFile content $ Toml.decode toml decoder

renderMetadataValueToml :: MetadataValue -> LazyByteString
renderMetadataValueToml VTrue = fromString "true"
renderMetadataValueToml VFalse = fromString "false"
renderMetadataValueToml (VString s) =
  fromString "\"" <> foldMap escape (Text.unpack s) <> fromString "\""
  where
    escape '"' = fromString "\\\""
    escape '\n' = fromString "\\n"
    escape c = Text.Lazy.Encoding.encodeUtf8 $ LazyText.singleton c
renderMetadataValueToml (VList xs) =
  fromString "["
    <> fold (intersperse (fromString ", ") (fmap renderMetadataValueToml xs))
    <> fromString "]"
renderMetadataValueToml (VRecord fields) =
  fromString "{"
    <> fold
      ( intersperse (fromString ", ") $
          fmap
            ( \(field, val) ->
                Text.Lazy.Encoding.encodeUtf8 (LazyText.fromStrict field)
                  <> fromString " = "
                  <> renderMetadataValueToml val
            )
            fields
      )
    <> fromString "}"
renderMetadataValueToml (VConstructor name args) =
  fromString "{"
    <> fromString "__ctor = "
    <> renderMetadataValueToml (VString name)
    <> fromString ", "
    <> fromString "__args = "
    <> renderMetadataValueToml (VList args)
    <> fromString "}"

metadataValueFromToml :: Toml.TomlValue -> MetadataValue
metadataValueFromToml Toml.VTrue = VTrue
metadataValueFromToml Toml.VFalse = VFalse
metadataValueFromToml (Toml.VString s) = VString s
metadataValueFromToml Toml.VInt{} = error "TODO: support TOML numbers"
metadataValueFromToml (Toml.VArray xs) = VList $ fmap (metadataValueFromToml . Toml.locatedValue) xs
metadataValueFromToml (Toml.VRecord fields) =
  let
    findField field = find (\(key, _value) -> Toml.locatedValue key == fromString field) fields
  in
    case (,) <$> findField "__ctor" <*> findField "__args" of
      Just (name, args)
        | (_, Toml.Located _offset (Toml.VString name')) <- name
        , (_, Toml.Located _offset (Toml.VArray args')) <- args ->
            VConstructor name' (fmap (metadataValueFromToml . Toml.locatedValue) args')
      _ ->
        error "TODO: support TOML records"

data Part
  = PIndex Int

newtype MetadataValueDecoder a = MetadataValueDecoder ([Part] -> MetadataValue -> Either DecodeError a)

data DecodeError
  = DecodeError [Part] String

runMetadataValueDecoder :: MetadataValueDecoder a -> [Part] -> MetadataValue -> Either DecodeError a
runMetadataValueDecoder (MetadataValueDecoder f) = f

list :: MetadataValueDecoder a -> MetadataValueDecoder [a]
list item =
  MetadataValueDecoder $
    \path val ->
      case val of
        VList xs ->
          traverse
            (\(ix, x) -> runMetadataValueDecoder item (path <> pure (PIndex ix)) x)
            (zip [0 ..] xs)
        _ -> Left $ DecodeError path "not a list"

text :: MetadataValueDecoder Text
text =
  MetadataValueDecoder $
    \path val ->
      case val of
        VString s -> pure s
        _ -> Left $ DecodeError path "not a string"

value :: MetadataValueDecoder MetadataValue
value = MetadataValueDecoder $ \_path -> pure
