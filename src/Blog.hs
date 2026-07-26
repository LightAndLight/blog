module Blog
  ( ResourceId (..)
  , resourceTypeParser
  , resourceNameParser
  , resourceIdParser
  , readResourceId
  , renderResourceId
  , ResourceType (..)
  , ResourceConfig (..)
  , resourceConfigDecoder
  , MetadataType (..)
  , metadataTypeDecoder
  , MetadataValue (..)
  , metadataValueString
  )
where

import Control.Applicative (some, (<|>))
import qualified Data.Char as Char
import Data.Map (Map)
import Data.String (fromString)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import qualified Text.Sage as Sage
import qualified Toml

data ResourceId
  = ResourceId
  { resourceType :: !String
  , resourceName :: !String
  }
  deriving (Show, Eq, Ord)

resourceTypeParser :: Sage.Parser String
resourceTypeParser = some $ Sage.satisfy Char.isAlpha

resourceNameParser :: Sage.Parser String
resourceNameParser =
  some $ Sage.satisfy ((||) <$> Char.isAlphaNum <*> (`elem` "-_."))

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
  , cfgMetadata :: !(Map Text MetadataType)
  }
  deriving (Show)

data MetadataType
  = TString
  | TList MetadataType
  deriving (Show)

resourceConfigDecoder :: Toml.Decoder ResourceConfig
resourceConfigDecoder =
  ResourceConfig
    <$> Toml.key (fromString "content-type") Toml.text
    <*> Toml.table (fromString "metadata") (Toml.keys $ Toml.pstring metadataTypeParser)

metadataTypeParser :: Sage.Parser MetadataType
metadataTypeParser =
  TString <$ Sage.string (fromString "string")
    <|> TList <$ Sage.string (fromString "list") <* Sage.char '(' <*> metadataTypeParser <* Sage.char ')'

data MetadataValue
  = VString !Text
  | VList ![MetadataValue]
  deriving (Show)

metadataValueString :: HasCallStack => MetadataValue -> Text
metadataValueString (VString s) = s
metadataValueString v = error $ "not a string: " ++ show v

metadataTypeDecoder :: MetadataType -> Toml.ValueDecoder MetadataValue
metadataTypeDecoder TString = VString <$> Toml.text
metadataTypeDecoder (TList ty) = fmap VList . Toml.list $ metadataTypeDecoder ty
