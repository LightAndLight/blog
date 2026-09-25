{-# LANGUAGE ScopedTypeVariables #-}

module Blog.Route
  ( Routes
  , Entry (..)
  , empty
  , null
  , singleton
  , insert
  , delete
  , lookup
  , unionWith
  , RouteEntry (..)
  , routeEntryParser
  , renderRouteEntry
  , RedirectEntry (..)
  , redirectEntryParser
  , renderRedirectEntry
  , parsePath
  )
where

import Blog (ResourceId, renderResourceId, resourceIdParser)
import Control.Applicative (many, optional, some, (<|>))
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified Text.Sage as Sage
import Prelude hiding (lookup, null)

data Routes = Routes !(Maybe Entry) !(Map Text Routes)

data Entry
  = EntryResourceId !ResourceId
  | EntryRedirect
      -- | Destination URL
      !ByteString

instance Semigroup Routes where
  rs <> rs' = unionWith (\l _r -> l) rs rs'

instance Monoid Routes where
  mempty = empty

empty :: Routes
empty = Routes Nothing mempty

null :: Routes -> Bool
null (Routes root rest) = isNothing root && Map.null rest

singleton :: [Text] -> Entry -> Routes
singleton [] value =
  Routes (Just value) mempty
singleton (p : ps) value =
  Routes Nothing $ Map.singleton p (singleton ps value)

insert :: [Text] -> Entry -> Routes -> Routes
insert [] value (Routes _ rest) = Routes (Just value) rest
insert (p : ps) value (Routes root rest) = Routes root (Map.alter f p rest)
  where
    f :: Maybe Routes -> Maybe Routes
    f Nothing = Just $ singleton ps value
    f (Just routes) = Just $ insert ps value routes

delete :: [Text] -> Routes -> Routes
delete [] (Routes _ rest) = Routes Nothing rest
delete (p : ps) (Routes root rest) = Routes root (Map.alter f p rest)
  where
    f :: Maybe Routes -> Maybe Routes
    f Nothing = Nothing
    f (Just routes) = do
      let routes' = delete ps routes
      guard . not $ null routes'
      pure routes'

lookup :: [Text] -> Routes -> Maybe Entry
lookup [] (Routes root _) = root
lookup (p : ps) (Routes _ rest) = do
  routes <- Map.lookup p rest
  lookup ps routes

unionWith :: (Entry -> Entry -> Entry) -> Routes -> Routes -> Routes
unionWith f (Routes root rest) (Routes root' rest') =
  Routes
    ( f <$> root <*> root'
        <|> root
        <|> root'
    )
    (Map.unionWith (unionWith f) rest rest')

data RouteEntry
  = RouteEntry
      -- | Path
      [Text]
      -- | Target
      ResourceId

routeEntryParser :: Sage.Parser RouteEntry
routeEntryParser =
  RouteEntry
    <$> pathParser
    <* spaces
    <* Sage.string (fromString "->")
    <* spaces
    <*> resourceIdParser
  where
    spaces =
      Sage.skipSome (Sage.satisfy Char.isSpace)

renderRouteEntry :: RouteEntry -> LazyByteString
renderRouteEntry (RouteEntry path resId) =
  renderPath path <> fromString " -> " <> fromString (renderResourceId resId)

data RedirectEntry
  = RedirectEntry
      -- | Path
      [Text]
      -- | Target
      !ByteString

redirectEntryParser :: Sage.Parser RedirectEntry
redirectEntryParser =
  RedirectEntry
    <$> pathParser
    <* spaces
    <* Sage.string (fromString "->")
    <* spaces
    <*> urlParser
  where
    spaces =
      Sage.skipSome (Sage.satisfy Char.isSpace)

    urlParser =
      {- TODO: proper URI encoding

      I would use `urlEncode` from `http-types`, but it has separate rules for
      path parts and query parameters. For that to work, this parser would
      need to parse the destination URL into hostname, path, and query parameters,
      then apply the appropriate URI encoding to each part.
      -}
      (\http mS sep rest -> Text.Encoding.encodeUtf8 $ http <> fold mS <> sep <> rest)
        <$> Sage.string (fromString "http")
        <*> optional (Sage.string $ fromString "s")
        <*> Sage.string (fromString "://")
        <*> fmap Text.pack (many $ Sage.satisfy (not . Char.isSpace))
        <* Sage.skipSome (Sage.satisfy Char.isSpace)

renderRedirectEntry :: RedirectEntry -> LazyByteString
renderRedirectEntry (RedirectEntry path dest) =
  renderPath path <> fromString " -> " <> LazyByteString.fromStrict dest

parsePath :: ByteString -> Either Sage.ParseError [Text]
parsePath = Sage.parse (pathParser <* Sage.eof)

pathParser :: Sage.Parser [Text]
pathParser =
  Sage.char '/' *> Sage.sepBy partParser (Sage.char '/')
  where
    partParser =
      Text.pack <$> some (Sage.satisfy $ (||) <$> Char.isAlphaNum <*> (`elem` "-._~")) Sage.<?> "url part"

renderPath :: [Text] -> LazyByteString
renderPath [] =
  fromString "/"
renderPath ps@(_ : _) =
  foldMap ((fromString "/" <>) . Text.Lazy.Encoding.encodeUtf8 . LazyText.fromStrict) ps
