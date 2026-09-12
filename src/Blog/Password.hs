module Blog.Password (hashPassword) where

import Control.Exception (throw)
import qualified Crypto.Argon2 as Argon2
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Short as ShortText

hashPassword ::
  -- | Salt
  ByteString ->
  -- | Password
  ByteString ->
  Text
hashPassword salt password =
  either throw ShortText.toText $ Argon2.hashEncoded options password salt
  where
    options =
      -- OWASP recommendations as of 2026/09/11 (https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html)
      Argon2.defaultHashOptions
        { Argon2.hashVariant = Argon2.Argon2id
        , Argon2.hashIterations = 2
        , -- 64MiB expressed in KiB
          Argon2.hashMemory = 64 * 2 ^ (10 :: Int)
        , Argon2.hashParallelism = 1
        }
