{-# LANGUAGE OverloadedStrings #-}

-- | BPC.Core.Hash - Cryptographic hashing utilities
--
-- Provides SHA-256 hashing and Base32 encoding for BPC.
module BPC.Core.Hash
  ( -- * SHA-256 Hashing
    sha256
  , sha256Hex

    -- * Base32 Encoding (RFC4648, no padding)
  , base32NoPad
  ) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray (convert)
import Data.ByteArray.Encoding (Base (Base32), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Compute SHA-256 hash of input bytes.
--
-- Returns raw 32-byte hash.
sha256 :: ByteString -> ByteString
sha256 bs = convert (hash bs :: Digest SHA256)

-- | Compute SHA-256 hash and return as lowercase hex string.
--
-- >>> sha256Hex "{\"a\":1,\"b\":2}"
-- "43258cff783fe7036d8a43033f830adfc60ec037382473548ac742b888292777"
sha256Hex :: ByteString -> Text
sha256Hex bs = TE.decodeUtf8 $ toHex $ sha256 bs

-- | Encode bytes to Base32 without padding (RFC4648).
--
-- Output contains only [A-Z2-7] characters.
base32NoPad :: ByteString -> Text
base32NoPad bs =
  let encoded = convertToBase Base32 bs :: ByteString
   in T.toUpper $ TE.decodeUtf8 $ BS8.filter (/= '=') encoded

-- Internal helper

toHex :: ByteString -> ByteString
toHex = BS.concatMap toHexByte
  where
    toHexByte b =
      let (hi, lo) = b `divMod` 16
       in BS.pack [hexDigit hi, hexDigit lo]
    hexDigit n
      | n < 10 = 0x30 + n -- '0' + n
      | otherwise = 0x61 + n - 10 -- 'a' + n - 10
