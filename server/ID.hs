{-# LANGUAGE BinaryLiterals #-}

-- | <https://www.rfc-editor.org/info/rfc9562/#section-5.4>
module ID
  ( ID
  , toString
  , fromString
  , generate
  )
where

import Control.Monad ((<=<))
import Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteString as ByteString
import Data.Maybe (fromMaybe)
import Data.Word (Word64, Word8)
import Numeric (readHex, showHex)
import System.Entropy (getEntropy)

data ID = ID !Word64 !Word64

toString :: ID -> String
toString (ID a b) =
  foldMap
    (\byte -> showHex byte "")
    [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15]
  where
    from word ix =
      fromIntegral (word `shiftR` (8 * ix)) :: Word8

    b0 = from a 7
    b1 = from a 6
    b2 = from a 5
    b3 = from a 4
    b4 = from a 3
    b5 = from a 2
    b6 = from a 1
    b7 = from a 0

    b8 = from b 7
    b9 = from b 6
    b10 = from b 5
    b11 = from b 4
    b12 = from b 3
    b13 = from b 2
    b14 = from b 1
    b15 = from b 0

fromBytes :: [Word8] -> Maybe ID
fromBytes bytes = do
  case bytes of
    [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15] ->
      pure $!
        ID
          ( b0 `at` 7
              .|. b1 `at` 6
              .|. b2 `at` 5
              .|. b3 `at` 4
              .|. b4 `at` 3
              .|. b5 `at` 2
              .|. b6 `at` 1
              .|. b7 `at` 0
          )
          ( b8 `at` 7
              .|. b9 `at` 6
              .|. b10 `at` 5
              .|. b11 `at` 4
              .|. b12 `at` 3
              .|. b13 `at` 2
              .|. b14 `at` 1
              .|. b15 `at` 0
          )
    _ -> Nothing
  where
    at :: Integral a => a -> Int -> Word64
    at byte index =
      (fromIntegral byte :: Word64) `shiftL` (8 * index)

fromString :: String -> Maybe ID
fromString = fromBytes <=< go
  where
    go :: String -> Maybe [Word8]
    go "" = Just []
    go input' = do
      let (prefix, suffix) = splitAt 2 input'
      case readHex prefix of
        [(byte, "")] -> (byte :) <$> go suffix
        _ -> Nothing

generate :: IO ID
generate =
  fromMaybe undefined . fromBytes . ByteString.unpack <$> getEntropy 16
