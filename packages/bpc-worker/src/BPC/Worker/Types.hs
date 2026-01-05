{-# LANGUAGE OverloadedStrings #-}

-- | Worker Types
--
-- Core types for the job processing worker.
--
-- @since 0.1.0.0
module BPC.Worker.Types
  ( -- * Configuration
    WorkerConfig(..)
  , WorkerEnv(..)
    -- * Handler Types
  , HandlerError(..)
  , HandlerResult(..)
    -- * Job Types
  , JobType(..)
  , JobPayload(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)

-- | Worker configuration.
--
-- @since 0.1.0.0
data WorkerConfig = WorkerConfig
  { wcPollIntervalMs :: Int
  -- ^ Job polling interval in milliseconds (BPC_JOBS_POLL_INTERVAL_MS)
  , wcLeaseRenewSeconds :: Int
  -- ^ Lease renewal interval in seconds (BPC_JOBS_LEASE_RENEW_SECONDS)
  , wcLeaseTimeoutSeconds :: Int
  -- ^ Lease timeout in seconds (BPC_JOBS_LEASE_TIMEOUT_SECONDS)
  , wcMaxAttempts :: Int
  -- ^ Maximum retry attempts (BPC_JOBS_MAX_ATTEMPTS_DEFAULT = 10)
  , wcWorkerId :: Text
  -- ^ Unique worker identifier
  , wcSigningKeyBase64 :: Maybe Text
  -- ^ ED25519 signing key (BPC_SIGNING_ED25519_PRIVATE_KEY_BASE64)
  , wcWebhookTimeoutSeconds :: Int
  -- ^ Webhook delivery timeout (default 30)
  }
  deriving stock (Show, Eq, Generic)

-- | Worker environment.
--
-- @since 0.1.0.0
data WorkerEnv = WorkerEnv
  { weConfig :: WorkerConfig
  , wePool :: Pool Connection
  }

-- | Handler error types.
--
-- @since 0.1.0.0
data HandlerError
  = HERetryable Text
  -- ^ Retryable error with message
  | HENonRetryable Text
  -- ^ Non-retryable error with message
  | HEValidation Text
  -- ^ Validation error
  | HENotFound Text
  -- ^ Resource not found
  | HEPrecondition Text
  -- ^ Precondition not met (wrong status, etc.)
  | HEInternal Text
  -- ^ Internal error
  deriving stock (Show, Eq, Generic)

instance ToJSON HandlerError where
  toJSON (HERetryable msg) = object ["type" .= ("retryable" :: Text), "message" .= msg]
  toJSON (HENonRetryable msg) = object ["type" .= ("non_retryable" :: Text), "message" .= msg]
  toJSON (HEValidation msg) = object ["type" .= ("validation" :: Text), "message" .= msg]
  toJSON (HENotFound msg) = object ["type" .= ("not_found" :: Text), "message" .= msg]
  toJSON (HEPrecondition msg) = object ["type" .= ("precondition" :: Text), "message" .= msg]
  toJSON (HEInternal msg) = object ["type" .= ("internal" :: Text), "message" .= msg]

-- | Handler result.
--
-- @since 0.1.0.0
data HandlerResult
  = HRSuccess
  -- ^ Job completed successfully
  | HRFailure HandlerError
  -- ^ Job failed (check error for retry vs permanent)
  | HRRetry NominalDiffTime
  -- ^ Explicit retry request with delay
  deriving stock (Show, Eq, Generic)

-- | Job types supported by the worker.
--
-- @since 0.1.0.0
data JobType
  = JTIngestDocument
  | JTParseFacts
  | JTBuildSnapshot
  | JTCompilePassport
  | JTSignPassport
  | JTGenerateQR
  | JTExportPassport
  | JTRunRuleTests
  | JTDeliverWebhook
  deriving stock (Show, Eq, Generic, Enum, Bounded)

instance ToJSON JobType where
  toJSON JTIngestDocument = "INGEST_DOCUMENT"
  toJSON JTParseFacts = "PARSE_FACTS"
  toJSON JTBuildSnapshot = "BUILD_SNAPSHOT"
  toJSON JTCompilePassport = "COMPILE_PASSPORT"
  toJSON JTSignPassport = "SIGN_PASSPORT"
  toJSON JTGenerateQR = "GENERATE_QR"
  toJSON JTExportPassport = "EXPORT_PASSPORT"
  toJSON JTRunRuleTests = "RUN_RULE_TESTS"
  toJSON JTDeliverWebhook = "DELIVER_WEBHOOK"

instance FromJSON JobType where
  parseJSON = Aeson.withText "JobType" $ \t ->
    case t of
      "INGEST_DOCUMENT" -> pure JTIngestDocument
      "PARSE_FACTS" -> pure JTParseFacts
      "BUILD_SNAPSHOT" -> pure JTBuildSnapshot
      "COMPILE_PASSPORT" -> pure JTCompilePassport
      "SIGN_PASSPORT" -> pure JTSignPassport
      "GENERATE_QR" -> pure JTGenerateQR
      "EXPORT_PASSPORT" -> pure JTExportPassport
      "RUN_RULE_TESTS" -> pure JTRunRuleTests
      "DELIVER_WEBHOOK" -> pure JTDeliverWebhook
      _ -> fail $ "Unknown job type: " ++ T.unpack t

-- | Job payload (JSON blob).
--
-- @since 0.1.0.0
newtype JobPayload = JobPayload { unJobPayload :: Value }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)
