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

import Control.Monad (void)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), decode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
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
  let mPayload = decode (LBS.fromStrict $ DB.jobPayload job) :: Maybe ExportPassportPayload
  case mPayload of
    Nothing -> pure $ HRFailure $ HEValidation "Invalid EXPORT_PASSPORT payload"
    Just payload -> withResource pool $ \conn -> do
      -- Get passport version
      mVersion <- DB.getPassportVersion conn (eppTenantId payload) (eppPassportVersionId payload)
      case mVersion of
        Nothing -> pure $ HRFailure $ HENotFound "Passport version not found"
        Just version -> do
          -- Check status is ACTIVE
          case DB.pvStatus version of
            DB.PassportVersionActive -> do
              -- Generate export based on format
              exportResult <- case eppFormat payload of
                ExportJSON -> exportToJson version
                ExportPDF -> exportToPdf version

              case exportResult of
                Left err -> pure $ HRFailure $ HENonRetryable err
                Right exportBytes -> do
                  -- Store export result (MVP: just emit event with path)
                  let outputPath = maybe
                        (T.pack $ "exports/" ++ UUID.toString (eppPassportVersionId payload) ++ "." ++ formatExt (eppFormat payload))
                        id
                        (eppOutputPath payload)

                  -- In real implementation, would write to object storage
                  -- For MVP, just emit audit event
                  now <- getCurrentTime
                  void $ DB.appendEvent conn (eppTenantId payload) DB.AppendEventInput
                    { DB.aeiAggregateType = "PassportVersion"
                    , DB.aeiAggregateId = eppPassportVersionId payload
                    , DB.aeiEventType = "PASSPORT_EXPORTED"
                    , DB.aeiEventData = Aeson.encode $ object
                        [ "passport_version_id" .= eppPassportVersionId payload
                        , "format" .= eppFormat payload
                        , "output_path" .= outputPath
                        , "size_bytes" .= BS.length exportBytes
                        ]
                    , DB.aeiActorId = Nothing
                    }

                  pure HRSuccess

            _ -> pure $ HRFailure $ HEPrecondition "Passport version not ACTIVE"

-- | Export passport to JSON.
--
-- @since 0.1.0.0
exportToJson :: DB.PassportVersion -> IO (Either Text BS.ByteString)
exportToJson version = do
  -- Build export JSON
  let exportData = object
        [ "passport_version_id" .= DB.pvId version
        , "passport_id" .= DB.pvPassportId version
        , "status" .= show (DB.pvStatus version)
        , "payload_hash" .= DB.pvPayloadHash version
        , "proof_hash" .= DB.pvProofHash version
        , "receipt_hash" .= DB.pvReceiptHash version
        , "signature" .= DB.pvSignature version
        , "public_key" .= DB.pvPublicKey version
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
