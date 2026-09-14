module Blog.Password (HashOptions (..), Argon2Variant (..), defaultHashOptions, hashPassword) where

import Control.Exception (throw)
import Crypto.Argon2 (Argon2Variant (..), HashOptions (..))
import qualified Crypto.Argon2 as Argon2
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Short as ShortText

{-| OWASP recommendations as of 2026/09/11 (https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html)

Tests may use weaker options for speed.
-}
defaultHashOptions :: HashOptions
defaultHashOptions =
  Argon2.defaultHashOptions
    { hashVariant = Argon2id
    , hashIterations = 2
    , -- 64MiB expressed in KiB
      hashMemory = 64 * 2 ^ (10 :: Int)
    , hashParallelism = 1
    }

hashPassword ::
  -- | Use 'defaultHashOptions' everywhere except for tests.
  HashOptions ->
  -- | Salt
  ByteString ->
  -- | Password
  ByteString ->
  Text
hashPassword options salt password =
  either throw ShortText.toText $ Argon2.hashEncoded options password salt
