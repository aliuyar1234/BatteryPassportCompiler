{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | BPC.Core.QR - QR Payload Generation (BPC-QR-1)
--
-- Generates compact QR payload strings for physical battery labels.
-- Format: @BPC1|pv=<uuid>|ph=<b32>|pr=<b32>|rh=<b32>@
module BPC.Core.QR
  ( -- * QR Types
    QrInput (..)
  , QrPayload (..)

    -- * QR Generation
  , buildQrPayload
  , formatQrString

    -- * Parsing (for verification)
  , parseQrPayload
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import BPC.Core.Hash (base32NoPad)

-- | Input for QR payload generation.
data QrInput = QrInput
  { qiPassportVersionId :: !UUID    -- ^ Passport version UUID
  , qiPayloadHash :: !Text          -- ^ SHA-256 hex of payload
  , qiProofHash :: !Text            -- ^ SHA-256 hex of proof
  , qiReceiptHash :: !Text          -- ^ SHA-256 hex of receipt
  }
  deriving stock (Eq, Show)

-- | Parsed QR payload components.
data QrPayload = QrPayload
  { qpVersion :: !Text              -- ^ "BPC1"
  , qpPassportVersionId :: !UUID    -- ^ pv field
  , qpPayloadHashB32 :: !Text       -- ^ ph field (Base32)
  , qpProofHashB32 :: !Text         -- ^ pr field (Base32)
  , qpReceiptHashB32 :: !Text       -- ^ rh field (Base32)
  }
  deriving stock (Eq, Show)

-- | Build a QR payload from input.
--
-- Converts hex hashes to Base32 without padding for compact encoding.
-- The resulting string fits within alphanumeric QR code mode.
buildQrPayload :: QrInput -> QrPayload
buildQrPayload QrInput{..} = QrPayload
  { qpVersion = "BPC1"
  , qpPassportVersionId = qiPassportVersionId
  , qpPayloadHashB32 = hexToBase32 qiPayloadHash
  , qpProofHashB32 = hexToBase32 qiProofHash
  , qpReceiptHashB32 = hexToBase32 qiReceiptHash
  }

-- | Format QR payload as string.
--
-- Format: @BPC1|pv=<uuid>|ph=<b32>|pr=<b32>|rh=<b32>@
--
-- Example: @BPC1|pv=123e4567-e89b-12d3-a456-426614174000|ph=ABCD...|pr=EFGH...|rh=IJKL...@
formatQrString :: QrPayload -> Text
formatQrString QrPayload{..} = T.intercalate "|"
  [ qpVersion
  , "pv=" <> UUID.toText qpPassportVersionId
  , "ph=" <> qpPayloadHashB32
  , "pr=" <> qpProofHashB32
  , "rh=" <> qpReceiptHashB32
  ]

-- | Parse a QR payload string.
parseQrPayload :: Text -> Either Text QrPayload
parseQrPayload input = do
  let parts = T.splitOn "|" input
  case parts of
    [version, pvPart, phPart, prPart, rhPart] -> do
      if version /= "BPC1"
        then Left $ "Unknown version: " <> version
        else Right ()

      pv <- parseField "pv" pvPart >>= parseUUID
      ph <- parseField "ph" phPart
      pr <- parseField "pr" prPart
      rh <- parseField "rh" rhPart

      Right QrPayload
        { qpVersion = version
        , qpPassportVersionId = pv
        , qpPayloadHashB32 = ph
        , qpProofHashB32 = pr
        , qpReceiptHashB32 = rh
        }
    _ -> Left "Invalid QR format: expected 5 pipe-separated parts"

-- | Parse a field with prefix "name=value".
parseField :: Text -> Text -> Either Text Text
parseField name part =
  case T.stripPrefix (name <> "=") part of
    Just value -> Right value
    Nothing -> Left $ "Expected " <> name <> "= prefix"

-- | Parse UUID from text.
parseUUID :: Text -> Either Text UUID
parseUUID t = case UUID.fromText t of
  Just uuid -> Right uuid
  Nothing -> Left $ "Invalid UUID: " <> t

-- | Convert hex string to Base32 without padding.
hexToBase32 :: Text -> Text
hexToBase32 hexStr =
  case B16.decode (TE.encodeUtf8 hexStr) of
    Right bytes -> base32NoPad bytes
    Left _ -> ""  -- Invalid hex

-- | Validate that Base32 contains only [A-Z2-7] characters.
isValidBase32 :: Text -> Bool
isValidBase32 = T.all isBase32Char
  where
    isBase32Char c = (c >= 'A' && c <= 'Z') || (c >= '2' && c <= '7')

-- | Validate that Base32 has no padding.
hasNoPadding :: Text -> Bool
hasNoPadding = not . T.isInfixOf "="

-- | Compute expected QR string length.
--
-- Format breakdown:
-- - "BPC1" = 4
-- - "|pv=" + UUID (36) = 40
-- - "|ph=" + Base32(32 bytes) ≈ 56
-- - "|pr=" + Base32(32 bytes) ≈ 56
-- - "|rh=" + Base32(32 bytes) ≈ 56
-- Total ≈ 212 characters (fits in alphanumeric QR)
expectedQrLength :: Int
expectedQrLength = 4 + 40 + 56 + 56 + 56  -- ~212
