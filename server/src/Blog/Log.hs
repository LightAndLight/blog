{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Blog.Log
  ( -- * Logging
    MonadLog (..)

    -- * Monad transformer
  , LogT (..)
  , runLogT

    -- * Re-exports
  , Json.toJSON
  , Json.object
  , (Json..=)
  )
where

import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.State.Strict (StateT, get, modify, put, runStateT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Lazy (LazyByteString)
import Data.Text (Text)

-- TODO: ideally we wouldn't use `aeson` for this (it's such a heavy dependency),
-- but we already depend on it for `pandoc` so might as well.
class Monad m => MonadLog m where
  attach :: ToJSON a => Text -> a -> m ()
  default attach :: (m ~ t n, MonadTrans t, MonadLog n) => ToJSON a => Text -> a -> m ()
  attach key value = lift $ attach key value

  scope :: Text -> m a -> m a
  default scope :: (m ~ t n, MFunctor t, MonadLog n) => Text -> m a -> m a
  scope key = hoist (scope key)

instance MonadLog m => MonadLog (ExceptT e m)

newtype LogT m a = LogT (StateT ([(Json.Object, Text)], Json.Object) m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow, MonadCatch, MonadMask)

runLogT ::
  Monad m =>
  -- | How to emit a log item
  (LazyByteString -> m ()) ->
  LogT m a ->
  m a
runLogT emit (LogT ma) = do
  (a, (_stack, obj)) <- runStateT ma mempty
  unless (KeyMap.null obj) . emit $ Json.encode obj
  pure a

instance Monad m => MonadLog (LogT m) where
  attach key value =
    LogT . modify $ \(stack, obj) ->
      let !obj' = KeyMap.insert (Key.fromText key) (Json.toJSON value) obj
      in (stack, obj')

  scope key (LogT ma) = LogT $ do
    do
      (stack, obj) <- get
      let
        !next =
          case KeyMap.lookup (Key.fromText key) obj of
            Just (Json.Object value) -> value
            _ -> mempty
      put ((obj, key) : stack, next)

    a <- ma

    do
      (stack, obj) <- get
      case stack of
        [] -> undefined
        ((prev, key') : stack') ->
          put (stack', KeyMap.insert (Key.fromText key') (Json.Object obj) prev)

    pure a
