{-# LANGUAGE OverloadedStrings #-}

-- | API Error Types and Error Envelope
--
-- All API errors are represented as sum types and converted to
-- JSON ErrorEnvelope format per SSOT 10.5.
--
-- @since 0.1.0.0
module BPC.API.Error
  ( -- * Error Types
    AppError(..)
    -- * Error Envelope
  , ErrorEnvelope(..)
  , ErrorDetail(..)
    -- * Conversion
  , toErrorResponse
  , appErrorStatus
  , errorCode
  ) where

import Data.Aeson (ToJSON(..), Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (Status, status400, status401, status403,
                                   status404, status409, status422,
                                   status429, status500, status503)

-- | Application error type.
--
-- Maps to HTTP status codes and error codes per SSOT 10.5.
data AppError
  -- Authentication errors (401)
  = Unauthorized Text
  | ApiKeyInvalid
  | ApiKeyRevoked
  | ApiKeyExpired

  -- Authorization errors (403)
  | Forbidden Text
  | PermissionDenied Text

  -- Not found errors (404)
  | NotFound Text
  | DocumentNotFound UUID
  | SnapshotNotFound UUID
  | PassportNotFound UUID
  | RulePackageNotFound UUID

  -- Conflict errors (409)
  | Conflict Text
  | SnapshotSealed UUID
  | IdempotencyConflict Text
  | ReplayMismatch Text

  -- Validation errors (422)
  | ValidationError [(Text, Text)]
  | InvalidRequest Text

  -- Rate limit errors (429)
  | RateLimitExceeded

  -- Internal errors (500)
  | InternalError Text
  | DatabaseError Text

  -- Service unavailable (503)
  | ServiceUnavailable Text
  deriving stock (Show, Eq, Generic)

-- | Error envelope per SSOT 10.5.
--
-- JSON format:
-- {
--   "error": {
--     "code": "ERROR_CODE",
--     "message": "Human readable message",
--     "correlation_id": "uuid",
--     "details": { ... }
--   }
-- }
data ErrorEnvelope = ErrorEnvelope
  { eeCode :: Text
  -- ^ Machine-readable error code
  , eeMessage :: Text
  -- ^ Human-readable message
  , eeCorrelationId :: UUID
  -- ^ Request correlation ID
  , eeDetails :: Maybe Value
  -- ^ Optional additional details
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ErrorEnvelope where
  toJSON ErrorEnvelope{..} = object
    [ "error" .= object
      ( [ "code" .= eeCode
        , "message" .= eeMessage
        , "correlation_id" .= eeCorrelationId
        ] ++ maybe [] (\d -> ["details" .= d]) eeDetails
      )
    ]

-- | Error detail for validation errors.
data ErrorDetail = ErrorDetail
  { edField :: Text
  , edMessage :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ErrorDetail where
  toJSON ErrorDetail{..} = object
    [ "field" .= edField
    , "message" .= edMessage
    ]

-- | Convert AppError to HTTP status.
appErrorStatus :: AppError -> Status
appErrorStatus = \case
  Unauthorized _ -> status401
  ApiKeyInvalid -> status401
  ApiKeyRevoked -> status401
  ApiKeyExpired -> status401

  Forbidden _ -> status403
  PermissionDenied _ -> status403

  NotFound _ -> status404
  DocumentNotFound _ -> status404
  SnapshotNotFound _ -> status404
  PassportNotFound _ -> status404
  RulePackageNotFound _ -> status404

  Conflict _ -> status409
  SnapshotSealed _ -> status409
  IdempotencyConflict _ -> status409
  ReplayMismatch _ -> status409

  ValidationError _ -> status422
  InvalidRequest _ -> status422

  RateLimitExceeded -> status429

  InternalError _ -> status500
  DatabaseError _ -> status500

  ServiceUnavailable _ -> status503

-- | Get error code from AppError.
errorCode :: AppError -> Text
errorCode = \case
  Unauthorized _ -> "UNAUTHORIZED"
  ApiKeyInvalid -> "API_KEY_INVALID"
  ApiKeyRevoked -> "API_KEY_REVOKED"
  ApiKeyExpired -> "API_KEY_EXPIRED"

  Forbidden _ -> "FORBIDDEN"
  PermissionDenied _ -> "PERMISSION_DENIED"

  NotFound _ -> "NOT_FOUND"
  DocumentNotFound _ -> "DOCUMENT_NOT_FOUND"
  SnapshotNotFound _ -> "SNAPSHOT_NOT_FOUND"
  PassportNotFound _ -> "PASSPORT_NOT_FOUND"
  RulePackageNotFound _ -> "RULE_PACKAGE_NOT_FOUND"

  Conflict _ -> "CONFLICT"
  SnapshotSealed _ -> "SNAPSHOT_SEALED"
  IdempotencyConflict _ -> "IDEMPOTENCY_CONFLICT"
  ReplayMismatch _ -> "REPLAY_MISMATCH"

  ValidationError _ -> "VALIDATION_ERROR"
  InvalidRequest _ -> "INVALID_REQUEST"

  RateLimitExceeded -> "RATE_LIMIT_EXCEEDED"

  InternalError _ -> "INTERNAL_ERROR"
  DatabaseError _ -> "DATABASE_ERROR"

  ServiceUnavailable _ -> "SERVICE_UNAVAILABLE"

-- | Convert AppError to ErrorEnvelope.
toErrorResponse :: AppError -> UUID -> ErrorEnvelope
toErrorResponse err correlationId = ErrorEnvelope
  { eeCode = errorCode err
  , eeMessage = errorMessage err
  , eeCorrelationId = correlationId
  , eeDetails = errorDetails err
  }

-- | Get human-readable message from error.
errorMessage :: AppError -> Text
errorMessage = \case
  Unauthorized msg -> msg
  ApiKeyInvalid -> "API key is invalid"
  ApiKeyRevoked -> "API key has been revoked"
  ApiKeyExpired -> "API key has expired"

  Forbidden msg -> msg
  PermissionDenied resource -> "Permission denied for resource: " <> resource

  NotFound resource -> "Resource not found: " <> resource
  DocumentNotFound uuid -> "Document not found: " <> T.pack (show uuid)
  SnapshotNotFound uuid -> "Snapshot not found: " <> T.pack (show uuid)
  PassportNotFound uuid -> "Passport not found: " <> T.pack (show uuid)
  RulePackageNotFound uuid -> "Rule package not found: " <> T.pack (show uuid)

  Conflict msg -> msg
  SnapshotSealed uuid -> "Snapshot is sealed and cannot be modified: " <> T.pack (show uuid)
  IdempotencyConflict key -> "Idempotency conflict for key: " <> key
  ReplayMismatch msg -> "Replay verification failed: " <> msg

  ValidationError _ -> "Request validation failed"
  InvalidRequest msg -> msg

  RateLimitExceeded -> "Rate limit exceeded. Please retry later."

  InternalError _ -> "An internal error occurred"
  DatabaseError _ -> "A database error occurred"

  ServiceUnavailable msg -> msg

-- | Get error details (for validation errors).
errorDetails :: AppError -> Maybe Value
errorDetails = \case
  ValidationError fields ->
    Just $ Aeson.toJSON $ map (\(f, m) -> ErrorDetail f m) fields
  _ -> Nothing
