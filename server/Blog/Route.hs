{-# LANGUAGE ScopedTypeVariables #-}

module Blog.Route
  ( Routes
  , empty
  , null
  , singleton
  , insert
  , delete
  , lookup
  , unionWith
  , RouteEntry (..)
  , routeEntryParser
  , parsePath
  , renderRouteEntry
  )
where

import Blog (ResourceId, renderResourceId, resourceIdParser)
import Control.Applicative (some, (<|>))
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified Text.Sage as Sage
import Prelude hiding (lookup, null)

data Routes a = Routes !(Maybe a) !(Map Text (Routes a))

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

parsePath :: ByteString -> Either Sage.ParseError [Text]
parsePath = Sage.parse (pathParser <* Sage.eof)

pathParser :: Sage.Parser [Text]
pathParser =
  Sage.char '/' *> Sage.sepBy partParser (Sage.char '/')
  where
    partParser =
      Text.pack <$> some (Sage.satisfy $ (||) <$> Char.isAlphaNum <*> (`elem` "-._~")) Sage.<?> "url part"

renderRouteEntry :: RouteEntry -> LazyByteString
renderRouteEntry (RouteEntry path resId) =
  renderPath path <> fromString " -> " <> fromString (renderResourceId resId)
  where
    renderPath [] =
      fromString "/"
    renderPath ps@(_ : _) =
      foldMap ((fromString "/" <>) . Text.Lazy.Encoding.encodeUtf8 . LazyText.fromStrict) ps
