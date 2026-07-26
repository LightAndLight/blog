{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module IO (readFile, writeFile, listDirectory, WithCallStack (..)) where

import Control.Exception (Exception, catch, throwIO)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import qualified System.Directory as Directory
import Prelude hiding (readFile, writeFile)

data WithCallStack a = WithCallStack CallStack a

instance Show a => Show (WithCallStack a) where
  show (WithCallStack cs err) = show err ++ "\n" ++ prettyCallStack cs

instance Exception a => Exception (WithCallStack a)

readFile :: HasCallStack => FilePath -> IO LazyByteString
readFile path = LazyByteString.readFile path `catch` (throwIO . WithCallStack @IOError callStack)

writeFile :: HasCallStack => FilePath -> LazyByteString -> IO ()
writeFile path content = LazyByteString.writeFile path content `catch` (throwIO . WithCallStack @IOError callStack)

listDirectory :: HasCallStack => FilePath -> IO [String]
listDirectory path = Directory.listDirectory path `catch` (throwIO . WithCallStack @IOError callStack)
