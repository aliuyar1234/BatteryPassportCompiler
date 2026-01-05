{-# LANGUAGE OverloadedStrings #-}

-- | Error types for BPC database operations.
--
-- All database errors are represented as sum types with constructors
-- for specific error conditions. This enables pattern matching for
-- error handling and provides clear error messages.
module BPC.DB.Error
  ( -- * Main Error Type
    DBError(..)
    -- * Event Store Errors
  , EventError(..)
  , ChainError(..)
    -- * Snapshot Errors
  , SnapshotError(..)
  , SealError(..)
    -- * Job Errors
  , JobError(..)
    -- * Document Errors
  , DocumentError(..)
  , UploadError(..)
    -- * Fact Errors
  , FactError(..)
    -- * Rule Errors
  , RuleError(..)
  , PublishError(..)
    -- * Passport Errors
  , PassportError(..)
  , ActivateError(..)
  , RevokeError(..)
    -- * Auth Errors
  , AuthError(..)
    -- * Webhook Errors
  , WebhookError(..)
  ) where

import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

-- | Top-level database error type.
data DBError
  = DB_UNAVAILABLE Text
  -- ^ Database connection unavailable
  | DB_TIMEOUT
  -- ^ Query timeout exceeded
  | DB_CONSTRAINT_VIOLATION Text
  -- ^ Constraint violation (unique, foreign key, etc.)
  | DB_SERIALIZATION_FAILURE
  -- ^ Transaction serialization failure (retry)
  | DB_INTERNAL Text
  -- ^ Internal database error
  | DBEventError EventError
  -- ^ Event store specific error
  | DBSnapshotError SnapshotError
  -- ^ Snapshot specific error
  | DBJobError JobError
  -- ^ Job queue specific error
  | DBDocumentError DocumentError
  -- ^ Document specific error
  | DBFactError FactError
  -- ^ Fact specific error
  | DBRuleError RuleError
  -- ^ Rule specific error
  | DBPassportError PassportError
  -- ^ Passport specific error
  | DBAuthError AuthError
  -- ^ Authentication specific error
  | DBWebhookError WebhookError
  -- ^ Webhook specific error
  deriving stock (Show, Eq, Generic)

-- | Event store errors per BPC-EVENT-1 specification.
data EventError
  = EVENT_VERSION_CONFLICT
  -- ^ Optimistic concurrency failure (another writer won)
  | EVENT_CHAIN_BROKEN UUID
  -- ^ Hash chain verification failed at event ID
  | EVENT_INVALID_HASH Text
  -- ^ Invalid hash format
  | EVENT_NOT_FOUND UUID
  -- ^ Event not found
  deriving stock (Show, Eq, Generic)

-- | Chain verification errors.
data ChainError
  = ChainBroken
    { ceEventId :: UUID
    -- ^ Event where chain broke
    , ceExpectedHash :: Text
    -- ^ Expected previous hash
    , ceActualHash :: Text
    -- ^ Actual previous hash found
    }
  | ChainEmpty
  -- ^ No events in chain
  | ChainInvalidGenesis
  -- ^ Genesis event has non-null prev_event_hash
  deriving stock (Show, Eq, Generic)

-- | Snapshot operation errors.
data SnapshotError
  = SNAPSHOT_SEALED UUID
  -- ^ Attempt to modify sealed snapshot
  | SNAPSHOT_NOT_READY UUID
  -- ^ Snapshot not in READY state for sealing
  | SNAPSHOT_NOT_FOUND UUID
  -- ^ Snapshot not found
  | SNAPSHOT_EMPTY UUID
  -- ^ Attempt to seal empty snapshot
  deriving stock (Show, Eq, Generic)

-- | Snapshot sealing errors.
data SealError
  = SealSnapshotNotFound UUID
  | SealSnapshotAlreadySealed UUID
  | SealSnapshotEmpty UUID
  | SealSnapshotNotBuilding UUID
  deriving stock (Show, Eq, Generic)

-- | Job queue errors.
data JobError
  = JOB_NOT_FOUND UUID
  -- ^ Job not found
  | LEASE_EXPIRED UUID
  -- ^ Job lease has expired
  | LEASE_NOT_HELD UUID Text
  -- ^ Worker does not hold lease (wrong worker_id)
  | JOB_ALREADY_COMPLETED UUID
  -- ^ Job already in terminal state
  | JOB_CANCELLED UUID
  -- ^ Job was cancelled
  deriving stock (Show, Eq, Generic)

-- | Document operation errors.
data DocumentError
  = DOCUMENT_NOT_FOUND UUID
  -- ^ Document not found
  | DOCUMENT_VERSION_NOT_FOUND UUID
  -- ^ Document version not found
  | DOCUMENT_DUPLICATE_HASH Text
  -- ^ Content hash already exists
  deriving stock (Show, Eq, Generic)

-- | Document upload errors.
data UploadError
  = UploadDocumentNotFound UUID
  | UploadDuplicateHash Text
  | UploadSizeLimitExceeded Int Int
  -- ^ (actual, max) size in bytes
  deriving stock (Show, Eq, Generic)

-- | Fact operation errors.
data FactError
  = FACT_NOT_FOUND UUID
  -- ^ Fact not found
  | FACT_DUPLICATE_HASH Text
  -- ^ Payload hash already exists
  | FACT_INVALID_TYPE Text
  -- ^ Invalid fact type
  deriving stock (Show, Eq, Generic)

-- | Rule package errors.
data RuleError
  = RULE_PACKAGE_NOT_FOUND UUID
  -- ^ Rule package not found
  | RULE_VERSION_NOT_FOUND UUID
  -- ^ Rule package version not found
  | RULE_TESTS_FAILED Text
  -- ^ Test requirements not met (message)
  | RULE_ALREADY_PUBLISHED UUID
  -- ^ Rule version already published
  | RULE_PARSE_ERROR Text
  -- ^ DSL parse error
  | RULE_TYPECHECK_ERROR Text
  -- ^ DSL typecheck error
  deriving stock (Show, Eq, Generic)

-- | Rule publish errors.
data PublishError
  = PublishVersionNotFound UUID
  | PublishAlreadyPublished UUID
  | PublishTestsNotRun UUID
  -- ^ No test run recorded
  | PublishInsufficientTests UUID Int Int
  -- ^ (version_id, actual_cases, required_cases)
  | PublishTestsFailed UUID
  -- ^ Test run failed
  deriving stock (Show, Eq, Generic)

-- | Passport operation errors.
data PassportError
  = PASSPORT_NOT_FOUND UUID
  -- ^ Passport not found
  | PASSPORT_VERSION_NOT_FOUND UUID
  -- ^ Passport version not found
  | PASSPORT_ALREADY_ACTIVE UUID
  -- ^ Version already active
  | PASSPORT_ALREADY_REVOKED UUID
  -- ^ Version already revoked
  deriving stock (Show, Eq, Generic)

-- | Passport activation errors.
data ActivateError
  = ActivateVersionNotFound UUID
  | ActivateAlreadyActive UUID
  | ActivateAlreadyRevoked UUID
  deriving stock (Show, Eq, Generic)

-- | Passport revocation errors.
data RevokeError
  = RevokeVersionNotFound UUID
  | RevokeAlreadyRevoked UUID
  | RevokeNotActive UUID
  deriving stock (Show, Eq, Generic)

-- | Authentication and authorization errors.
data AuthError
  = AUTH_TENANT_NOT_FOUND UUID
  -- ^ Tenant not found
  | AUTH_ACTOR_NOT_FOUND UUID
  -- ^ Actor not found
  | AUTH_API_KEY_INVALID
  -- ^ API key invalid (not found or hash mismatch)
  | AUTH_API_KEY_REVOKED
  -- ^ API key has been revoked
  | AUTH_API_KEY_EXPIRED
  -- ^ API key has expired (past expires_at timestamp)
  | AUTH_PERMISSION_DENIED Text
  -- ^ Permission denied (resource)
  | AUTH_ROLE_NOT_FOUND UUID
  -- ^ Role not found
  deriving stock (Show, Eq, Generic)

-- | Webhook operation errors.
data WebhookError
  = WEBHOOK_ENDPOINT_NOT_FOUND UUID
  -- ^ Endpoint not found
  | WEBHOOK_SUBSCRIPTION_NOT_FOUND UUID
  -- ^ Subscription not found
  | WEBHOOK_DELIVERY_FAILED UUID Text
  -- ^ Delivery failed (delivery_id, reason)
  deriving stock (Show, Eq, Generic)
