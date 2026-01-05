{-# LANGUAGE OverloadedStrings #-}

-- | Export Passport Handler
--
-- Exports passports in various formats (JSON, PDF).
--
-- @since 0.1.0.0
module BPC.Worker.Handlers.ExportPassport
  ( handle
  , ExportPassportPayload(..)
  , ExportFormat(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?))
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

-- | Export format.
--
-- @since 0.1.0.0
data ExportFormat
  = ExportJSON
  | ExportPDF
  deriving stock (Show, Eq, Generic)

instance FromJSON ExportFormat where
  parseJSON = Aeson.withText "ExportFormat" $ \t ->
    case T.toUpper t of
      "JSON" -> pure ExportJSON
      "PDF" -> pure ExportPDF
      _ -> fail $ "Unknown export format: " ++ T.unpack t

instance ToJSON ExportFormat where
  toJSON ExportJSON = "JSON"
  toJSON ExportPDF = "PDF"

-- | Payload for EXPORT_PASSPORT job.
--
-- @since 0.1.0.0
data ExportPassportPayload = ExportPassportPayload
  { eppPassportVersionId :: UUID
  , eppTenantId :: UUID
  , eppFormat :: ExportFormat
  , eppOutputPath :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ExportPassportPayload where
  parseJSON = Aeson.withObject "ExportPassportPayload" $ \o -> ExportPassportPayload
    <$> o .: "passport_version_id"
    <*> o .: "tenant_id"
    <*> o .: "format"
    <*> o .:? "output_path"

instance ToJSON ExportPassportPayload where
  toJSON ExportPassportPayload{..} = object
    [ "passport_version_id" .= eppPassportVersionId
    , "tenant_id" .= eppTenantId
    , "format" .= eppFormat
    , "output_path" .= eppOutputPath
    ]

-- | Handle EXPORT_PASSPORT job.
--
-- @since 0.1.0.0
handle :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
handle _config pool job = do
  -- Decode payload
  case Aeson.fromJSON (DB.jobPayload job) of
    Aeson.Error _err -> pure $ HRFailure $ HEValidation "Invalid EXPORT_PASSPORT payload"
    Aeson.Success (_payload :: ExportPassportPayload) -> withResource pool $ \_conn -> do
      -- TODO: Implement passport export
      -- 1. Get passport version
      -- 2. Check status is ACTIVE
      -- 3. Generate export (JSON or PDF)
      -- 4. Store to object storage
      -- 5. Emit audit event
      pure HRSuccess

-- | Export passport to JSON.
--
-- @since 0.1.0.0
exportToJson :: DB.PassportVersion -> IO (Either Text BS.ByteString)
exportToJson version = do
  -- Build export JSON
  -- TODO: Add public_key field to PassportVersion table and DB.PassportVersion type
  let exportData = object
        [ "passport_version_id" .= DB.pvId version
        , "passport_id" .= DB.pvPassportId version
        , "status" .= show (DB.pvStatus version)
        , "payload_hash" .= DB.pvPayloadHash version
        , "proof_hash" .= DB.pvProofHash version
        , "receipt_hash" .= DB.pvReceiptHash version
        , "signature" .= DB.pvSignature version
        -- , "public_key" .= DB.pvPublicKey version  -- Field doesn't exist yet
        , "qr_payload" .= DB.pvQrPayload version
        , "created_at" .= DB.pvCreatedAt version
        ]
  pure $ Right $ LBS.toStrict $ Aeson.encode exportData

-- | Export passport to PDF.
--
-- @since 0.1.0.0
exportToPdf :: DB.PassportVersion -> IO (Either Text BS.ByteString)
exportToPdf _version = do
  -- MVP: PDF export not implemented
  pure $ Left "PDF export not implemented in MVP"

-- | Get file extension for format.
--
-- @since 0.1.0.0
formatExt :: ExportFormat -> String
formatExt ExportJSON = "json"
formatExt ExportPDF = "pdf"
