{-# LANGUAGE OverloadedStrings #-}

-- | BPC.Core.CanonicalJson - Canonical JSON Encoding (BPC-CJSON-1)
--
-- Implements deterministic JSON encoding per SSOT Section 7.1.
--
-- Properties:
--   * Object keys sorted by UTF-8 byte order
--   * No whitespace (compact encoding)
--   * Floats and exponential notation rejected
--   * Same input always produces identical bytes
module BPC.Core.CanonicalJson
  ( -- * Encoding
    canonicalEncode

    -- * Decoding
  , canonicalDecode
  ) where

import BPC.Core.Error (CanonicalError (..))
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

-- | Encode a JSON value to canonical bytes.
--
-- Returns 'Left' if the value contains floats or exponential notation.
--
-- >>> canonicalEncode (Object [("b", Number 2), ("a", Number 1)])
-- Right "{\"a\":1,\"b\":2}"
canonicalEncode :: Value -> Either CanonicalError ByteString
canonicalEncode val = do
  builder <- encodeValue val
  pure $ LBS.toStrict $ Builder.toLazyByteString builder

-- | Decode canonical JSON bytes to a Value.
--
-- Note: This is a lenient decoder that accepts any valid JSON.
-- For strict canonical validation, re-encode and compare.
canonicalDecode :: ByteString -> Either CanonicalError Value
canonicalDecode bs =
  case Aeson.decodeStrict bs of
    Nothing -> Left $ CanonicalParseError "Invalid JSON"
    Just val -> Right val

-- Internal encoding functions

encodeValue :: Value -> Either CanonicalError Builder.Builder
encodeValue val = case val of
  Null -> Right "null"
  Bool True -> Right "true"
  Bool False -> Right "false"
  Number n -> encodeNumber n
  String s -> Right $ encodeString s
  Array arr -> encodeArray arr
  Object obj -> encodeObject obj

encodeNumber :: Scientific -> Either CanonicalError Builder.Builder
encodeNumber n
  | not (Scientific.isInteger n) =
      Left $ CanonicalNumberNotAllowed $ "Float not allowed: " <> T.pack (show n)
  | otherwise =
      let int = Scientific.coefficient n * (10 ^ Scientific.base10Exponent n)
       in Right $ Builder.integerDec int

encodeString :: Text -> Builder.Builder
encodeString s =
  Builder.char7 '"' <> encodeStringContent s <> Builder.char7 '"'

encodeStringContent :: Text -> Builder.Builder
encodeStringContent = TE.encodeUtf8Builder . escapeString

escapeString :: Text -> Text
escapeString = T.concatMap escapeChar
  where
    escapeChar c = case c of
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      _ | c < ' ' -> "\\u" <> T.pack (pad4 (fromEnum c))
        | otherwise -> T.singleton c
    pad4 n = let s = showHex n "" in replicate (4 - length s) '0' ++ s
    showHex n acc
      | n == 0 = if null acc then "0" else acc
      | otherwise =
          let (q, r) = n `divMod` 16
              c = "0123456789abcdef" !! r
           in showHex q (c : acc)

encodeArray :: V.Vector Value -> Either CanonicalError Builder.Builder
encodeArray arr = do
  builders <- mapM encodeValue (V.toList arr)
  let contents = mconcat $ intersperse (Builder.char7 ',') builders
  pure $ Builder.char7 '[' <> contents <> Builder.char7 ']'

encodeObject :: KM.KeyMap Value -> Either CanonicalError Builder.Builder
encodeObject obj = do
  let pairs = sortBy (comparing fst) $ map keyToText $ KM.toList obj
  encodedPairs <- mapM encodePair pairs
  let contents = mconcat $ intersperse (Builder.char7 ',') encodedPairs
  pure $ Builder.char7 '{' <> contents <> Builder.char7 '}'
  where
    keyToText (k, v) = (Key.toText k, v)

encodePair :: (Text, Value) -> Either CanonicalError Builder.Builder
encodePair (k, v) = do
  vBuilder <- encodeValue v
  pure $ encodeString k <> Builder.char7 ':' <> vBuilder

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x : xs) = x : sep : intersperse sep xs
