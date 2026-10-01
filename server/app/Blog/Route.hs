{-# LANGUAGE BangPatterns #-}
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
  , foldlWithKey'
  , RouteEntry (..)
  , Visibility (..)
  , routeEntryParser
  , renderRouteEntry
  , RedirectEntry (..)
  , redirectEntryParser
  , renderRedirectEntry
  , parsePath
  , renderPath
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

data Routes a = Routes !(Maybe a) !(Map Text (Routes a))

data Entry
  = EntryResourceId
      !Visibility
      !ResourceId
  | EntryRedirect
      -- | Destination URL
      !ByteString

instance Semigroup (Routes a) where
  rs <> rs' = unionWith (\l _r -> l) rs rs'

instance Monoid (Routes a) where
  mempty = empty

empty :: Routes a
empty = Routes Nothing mempty

null :: Routes a -> Bool
null (Routes root rest) = isNothing root && Map.null rest

singleton :: [Text] -> a -> Routes a
singleton [] value =
  Routes (Just value) mempty
singleton (p : ps) value =
  Routes Nothing $ Map.singleton p (singleton ps value)

insert :: forall a. [Text] -> a -> Routes a -> Routes a
insert [] value (Routes _ rest) = Routes (Just value) rest
insert (p : ps) value (Routes root rest) = Routes root (Map.alter f p rest)
  where
    f :: Maybe (Routes a) -> Maybe (Routes a)
    f Nothing = Just $ singleton ps value
    f (Just routes) = Just $ insert ps value routes

delete :: [Text] -> Routes a -> Routes a
delete [] (Routes _ rest) = Routes Nothing rest
delete (p : ps) (Routes root rest) = Routes root (Map.alter f p rest)
  where
    f :: Maybe (Routes a) -> Maybe (Routes a)
    f Nothing = Nothing
    f (Just routes) = do
      let routes' = delete ps routes
      guard . not $ null routes'
      pure routes'

lookup :: [Text] -> Routes a -> Maybe a
lookup [] (Routes root _) = root
lookup (p : ps) (Routes _ rest) = do
  routes <- Map.lookup p rest
  lookup ps routes

unionWith :: (a -> a -> a) -> Routes a -> Routes a -> Routes a
unionWith f (Routes root rest) (Routes root' rest') =
  Routes
    ( f <$> root <*> root'
        <|> root
        <|> root'
    )
    (Map.unionWith (unionWith f) rest rest')

foldlWithKey' :: (b -> [Text] -> a -> b) -> b -> Routes a -> b
foldlWithKey' f !z = go [] z
  where
    go path acc (Routes ma rest) =
      Map.foldlWithKey'
        (\acc' part -> go (path ++ [part]) acc')
        (maybe acc (f acc path) ma)
        rest

data RouteEntry
  = RouteEntry
      Visibility
      -- | Path
      [Text]
      -- | Target
      ResourceId

data Visibility = Public | Private

routeEntryParser :: Sage.Parser RouteEntry
routeEntryParser =
  RouteEntry
    <$> (Private <$ Sage.string (fromString "private") <* spaces <|> pure Public)
    <*> pathParser
    <* spaces
    <* Sage.string (fromString "->")
    <* spaces
    <*> resourceIdParser
  where
    spaces =
      Sage.skipSome (Sage.satisfy Char.isSpace)

renderRouteEntry :: RouteEntry -> LazyByteString
renderRouteEntry (RouteEntry visibility path resId) =
  (case visibility of Private -> fromString "private "; Public -> mempty)
    <> renderPath path
    <> fromString " -> "
    <> fromString (renderResourceId resId)

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
