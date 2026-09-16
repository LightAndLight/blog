{-# LANGUAGE BangPatterns #-}

module Blog
  ( Name
  , mkName
  , unsafeName
  , nameParser
  , renderName
  , nameToPath
  , pathToName
  , ResourceId (..)
  , resourceTypeParser
  , resourceNameParser
  , resourceIdParser
  , readResourceId
  , renderResourceId
  , resourceIdToPath
  , pathToResourceId
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

import Control.Applicative (optional, some, (<|>))
import qualified Data.Char as Char
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.String (fromString)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import qualified Temple
import qualified Text.Sage as Sage
import qualified Toml

newtype Name = Name String
  deriving (Show, Eq, Ord)

-- | Precondition: the name is non-empty
mkName :: String -> Maybe Name
mkName "" = Nothing
mkName n = Just $ Name n

-- | Precondition: the name is non-empty
unsafeName :: HasCallStack => String -> Name
unsafeName n = fromMaybe (error $ "invalid name: " ++ show n) $ mkName n

renderName :: Name -> String
renderName (Name n) = n

nameToPath :: Name -> FilePath
nameToPath (Name n)
  -- Disallow the name "."
  | "." <- n = "%2E"
  -- Disallow anything prefixed with ".." (avoids "..", "...", etc.)
  | '.' : '.' : cs <- n = "%2E%2E" ++ go cs
  | otherwise = go n
  where
    -- Not allowed in Unix file paths
    go ('\NUL' : cs) = "%00" ++ go cs
    -- Prevent directory traversal
    go ('/' : cs) = "%2F" ++ go cs
    -- "%" is the escape character
    go ('%' : cs) = "%25" ++ go cs
    -- ":" has a special meaning on disk
    go (':' : cs) = "%3A" ++ go cs
    go (c : cs) = c : go cs
    go [] = ""

-- | Precondition: the path is non-empty
pathToName :: HasCallStack => FilePath -> Name
pathToName = unsafeName . go
  where
    go ('%' : cs) =
      let
        (prefix, suffix) = splitAt 2 cs
        !c =
          case prefix of
            "00" -> '\NUL'
            "2E" -> '.'
            "2F" -> '/'
            "25" -> '%'
            "3A" -> ':'
            _ -> error $ "invalid percent-encoding: %" ++ prefix
      in
        c : go suffix
    go (c : cs) = c : go cs
    go [] = ""

data ResourceId
  = ResourceId
  { resourceType :: !Name
  , resourceName :: !Name
  }
  deriving (Show, Eq, Ord)

nameParser :: Sage.Parser Name
nameParser =
  fmap Name
    . some
    $ Sage.satisfy ((||) <$> Char.isAlphaNum <*> (`elem` "-_."))

resourceTypeParser :: Sage.Parser Name
resourceTypeParser =
  nameParser

resourceNameParser :: Sage.Parser Name
resourceNameParser =
  nameParser

propertyParser :: Sage.Parser Name
propertyParser =
  nameParser

resourceIdParser :: Sage.Parser ResourceId
resourceIdParser = ResourceId <$> resourceTypeParser <* Sage.char ':' <*> resourceNameParser

renderResourceId :: ResourceId -> String
renderResourceId (ResourceId type_ name) = renderName type_ ++ ":" ++ renderName name

resourceIdToPath :: ResourceId -> FilePath
resourceIdToPath (ResourceId resTyName resName) = nameToPath resTyName ++ ":" ++ nameToPath resName

pathToResourceId :: HasCallStack => FilePath -> ResourceId
pathToResourceId input =
  let (prefix, suffix) = break (== ':') input
  in case suffix of
       ':' : rest -> ResourceId (pathToName prefix) (pathToName rest)
       _ -> error $ "invalid resource ID: " ++ show input

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
  Name ->
  FilePath
propertiesPart resName = nameToPath resName ++ ":properties"

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
  deriving (Show, Eq)

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
