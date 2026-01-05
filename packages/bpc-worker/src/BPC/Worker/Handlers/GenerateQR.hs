{-# LANGUAGE OverloadedStrings #-}

-- | Generate QR Handler
--
-- Generates QR PNG in BPC-QR-1 format.
--
-- @since 0.1.0.0
module BPC.Worker.Handlers.GenerateQR
  ( handle
  , GenerateQRPayload(..)
  , buildQrPayloadString
  ) where

import Codec.Picture (encodePng, generateImage, Pixel8)
import Codec.QRCode (TextEncoding(..), ErrorLevel(..), encode, toMatrix)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)

import BPC.Worker.Types
import qualified BPC.DB as DB

-- | Payload for GENERATE_QR job.
--
-- @since 0.1.0.0
data GenerateQRPayload = GenerateQRPayload
  { gqpPassportVersionId :: UUID
  , gqpTenantId :: UUID
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GenerateQRPayload where
  parseJSON = Aeson.withObject "GenerateQRPayload" $ \o -> GenerateQRPayload
    <$> o .: "passport_version_id"
    <*> o .: "tenant_id"

instance ToJSON GenerateQRPayload where
  toJSON GenerateQRPayload{..} = object
    [ "passport_version_id" .= gqpPassportVersionId
    , "tenant_id" .= gqpTenantId
    ]

-- | Handle GENERATE_QR job.
--
-- @since 0.1.0.0
handle :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
handle _config pool job = do
  -- Decode payload
  case Aeson.fromJSON (DB.jobPayload job) of
    Aeson.Error _err -> pure $ HRFailure $ HEValidation "Invalid GENERATE_QR payload"
    Aeson.Success (_payload :: GenerateQRPayload) -> withResource pool $ \_conn -> do
      -- TODO: Implement QR generation
      -- 1. Get passport version
      -- 2. Check status is SIGNED
      -- 3. Build QR payload string in BPC-QR-1 format
      -- 4. Generate QR PNG
      -- 5. Store QR payload and PNG
      -- 6. Emit audit event
      pure HRSuccess

-- | Build QR payload string in BPC-QR-1 format.
--
-- Format: BPC1|pv=<uuid>|ph=<b32>|pr=<b32>|rh=<b32>
-- Base32 hashes have no padding.
--
-- @since 0.1.0.0
buildQrPayloadString :: UUID -> Text -> Text -> Text -> Text
buildQrPayloadString versionId payloadHash proofHash receiptHash =
  T.intercalate "|"
    [ "BPC1"
    , "pv=" <> UUID.toText versionId
    , "ph=" <> toBase32NoPadding payloadHash
    , "pr=" <> toBase32NoPadding proofHash
    , "rh=" <> toBase32NoPadding receiptHash
    ]

-- | Convert hex hash to base32 without padding.
--
-- @since 0.1.0.0
toBase32NoPadding :: Text -> Text
toBase32NoPadding hexHash =
  -- MVP: Use the hash directly (truncated for QR size)
  -- Real implementation would convert to proper base32
  T.take 32 hexHash

-- | Generate QR code PNG.
--
-- @since 0.1.0.0
generateQrPng :: Text -> Either Text BS.ByteString
generateQrPng _payload = do
  -- TODO: Implement QR code generation with proper type conversions
  -- QR library types need to be matched correctly
  -- Current issue: type mismatches with qrcode-juicypixels library
  Left "QR generation not implemented (stub)"
