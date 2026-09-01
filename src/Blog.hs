module Blog
  ( nameParser
  , ResourceId (..)
  , resourceTypeParser
  , resourceNameParser
  , resourceIdParser
  , readResourceId
  , renderResourceId
  , propertyParser
  , ResourceType (..)
  , ResourceConfig (..)
  , resourceConfigDecoder
  , propertiesPart
  , MetadataConfig (..)
  , MetadataType (..)
  , metadataTypeDecoder
  , MetadataValue (..)
  , metadataValueString
  )
where

import Control.Applicative (many, optional, some, (<|>))
import qualified Data.Char as Char
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.String (fromString)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import qualified Temple
import qualified Text.Sage as Sage
import qualified Toml

data ResourceId
  = ResourceId
  { resourceType :: !String
  , resourceName :: !String
  }
  deriving (Show, Eq, Ord)

nameParser :: Sage.Parser String
nameParser =
  some $ Sage.satisfy ((||) <$> Char.isAlphaNum <*> (`elem` "-_."))

resourceTypeParser :: Sage.Parser String
resourceTypeParser = (:) <$> Sage.satisfy Char.isAlpha <*> many (Sage.satisfy Char.isAlphaNum)

resourceNameParser :: Sage.Parser String
resourceNameParser =
  nameParser

propertyParser :: Sage.Parser String
propertyParser =
  nameParser

resourceIdParser :: Sage.Parser ResourceId
resourceIdParser = ResourceId <$> resourceTypeParser <* Sage.char ':' <*> resourceNameParser

renderResourceId :: ResourceId -> String
renderResourceId (ResourceId type_ name) = type_ ++ ":" ++ name

readResourceId :: HasCallStack => String -> ResourceId
readResourceId input =
  case Sage.parse (resourceIdParser <* Sage.eof) $ fromString input of
    Left{} -> error $ "invalid resource ID '" ++ input ++ "'"
    Right x -> x

data ResourceType
  = ResourceType
  { resourceTypeName :: !Text
  , resourceTypeConfig :: !ResourceConfig
  }
  deriving (Show)

data ResourceConfig
  = ResourceConfig
  { cfgContentType :: !Text
  , cfgMetadata :: !(Map Text MetadataConfig)
  }
  deriving (Show)

propertiesPart ::
  -- | Resource name
  String ->
  FilePath
propertiesPart resName = resName ++ ":properties"

data MetadataConfig
  = MetadataConfig
  { metaCfgType :: !MetadataType
  , metaCfgOptional :: !Bool
  , metaCfgDefault :: !(Maybe MetadataValue)
  }
  deriving (Show)

data MetadataType
  = TBool
  | TString
  | TList MetadataType
  | TRecord [(Text, MetadataType)]
  deriving (Show)

resourceConfigDecoder :: Toml.Decoder ResourceConfig
resourceConfigDecoder =
  ResourceConfig
    <$> Toml.key (fromString "content-type") Toml.text
    <*> Toml.table
      (fromString "metadata")
      ( Toml.keys $
          noDefault
            <$> Toml.pstring metadataTypeParser
              `Toml.alt` withDefault
      )
  where
    noDefault ty = MetadataConfig{metaCfgType = ty, metaCfgOptional = False, metaCfgDefault = Nothing}

    withDefault =
      Toml.record $
        ( \ty opt def ->
            let
              -- providing `default` implies `optional = true`
              opt' = fromMaybe (isJust def) opt
            in
              MetadataConfig ty opt' def
        )
          <$> Toml.recordKey (fromString "type") (Toml.pstring metadataTypeParser)
          <*> optional (Toml.recordKey (fromString "optional") Toml.bool)
          <*> optional (Toml.recordKey (fromString "default") (fmap tomlValueToMetadataValue Toml.value))

    tomlValueToMetadataValue Toml.VTrue = VTrue
    tomlValueToMetadataValue Toml.VFalse = VFalse
    tomlValueToMetadataValue (Toml.VString s) = VString s
    tomlValueToMetadataValue (Toml.VArray s) = VList $ tomlValueToMetadataValue . Toml.locatedValue <$> s
    tomlValueToMetadataValue val = error $ "TODO: " ++ show val

metadataTypeParser :: Sage.Parser MetadataType
metadataTypeParser =
  TBool <$ Sage.string (fromString "bool")
    <|> TString <$ Sage.string (fromString "string")
    <|> TList <$ Sage.string (fromString "list") <* Sage.char '(' <*> metadataTypeParser <* Sage.char ')'
    <|> TRecord
      <$ Sage.string (fromString "record")
      <* Sage.char '('
      <*> Sage.sepBy
        ((,) <$> Temple.identParser <* Temple.symbolic ':' <*> metadataTypeParser)
        (Temple.symbolic ',')
      <* Sage.char ')'

data MetadataValue
  = VTrue
  | VFalse
  | VString !Text
  | VList ![MetadataValue]
  | VConstructor !Text ![MetadataValue]
  | VRecord ![(Text, MetadataValue)]
  deriving (Show)

metadataValueString :: HasCallStack => MetadataValue -> Text
metadataValueString (VString s) = s
metadataValueString v = error $ "not a string: " ++ show v

metadataTypeDecoder :: MetadataType -> Toml.ValueDecoder MetadataValue
metadataTypeDecoder TBool = (\b -> if b then VTrue else VFalse) <$> Toml.bool
metadataTypeDecoder TString = VString <$> Toml.text
metadataTypeDecoder (TList ty) = fmap VList . Toml.list $ metadataTypeDecoder ty
metadataTypeDecoder (TRecord fields) =
  VRecord
    <$> Toml.record
      (traverse (\(field, ty) -> (,) field <$> Toml.recordKey field (metadataTypeDecoder ty)) fields)
