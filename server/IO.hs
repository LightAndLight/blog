{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module IO
  ( readFile
  , writeFile
  , copyFile
  , removeFile
  , createDirectoryIfMissing
  , removeDirectory
  , removeDirectoryRecursive
  , listDirectory
  , getModificationTime
  , WithCallStack (..)
  ) where

import Control.Exception (Exception, catch, throwIO)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Time.Clock (UTCTime)
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

copyFile :: HasCallStack => FilePath -> FilePath -> IO ()
copyFile from to = Directory.copyFile from to `catch` (throwIO . WithCallStack @IOError callStack)

removeFile :: HasCallStack => FilePath -> IO ()
removeFile path = Directory.removeFile path `catch` (throwIO . WithCallStack @IOError callStack)

createDirectoryIfMissing :: HasCallStack => Bool -> FilePath -> IO ()
createDirectoryIfMissing parent path =
  Directory.createDirectoryIfMissing parent path
    `catch` (throwIO . WithCallStack @IOError callStack)

removeDirectory :: HasCallStack => FilePath -> IO ()
removeDirectory path = Directory.removeDirectory path `catch` (throwIO . WithCallStack @IOError callStack)

removeDirectoryRecursive :: HasCallStack => FilePath -> IO ()
removeDirectoryRecursive path = Directory.removeDirectoryRecursive path `catch` (throwIO . WithCallStack @IOError callStack)

listDirectory :: HasCallStack => FilePath -> IO [String]
listDirectory path = Directory.listDirectory path `catch` (throwIO . WithCallStack @IOError callStack)

getModificationTime :: HasCallStack => FilePath -> IO UTCTime
getModificationTime path = Directory.getModificationTime path `catch` (throwIO . WithCallStack @IOError callStack)
