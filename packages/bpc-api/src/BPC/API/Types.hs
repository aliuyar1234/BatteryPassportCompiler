{-# LANGUAGE OverloadedStrings #-}

-- | API Request and Response Types
--
-- Common types for pagination, authentication context, and
-- request/response structures.
--
-- @since 0.1.0.0
module BPC.API.Types
  ( -- * Authentication
    AuthContext(..)
  , Permission(..)
  , allPermissions

    -- * Pagination
  , PaginationParams(..)
  , Cursor(..)
  , CursorPage(..)
  , parseCursor
  , encodeCursor
  , defaultPaginationParams
  , defaultPagination

    -- * Common Responses
  , SuccessEnvelope(..)
  , CreatedResponse(..)
  , AcceptedResponse(..)

    -- * Replay
  , ReplayResult(..)
  , ReplayMismatch(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)

-- | Authentication context extracted from API key.
data AuthContext = AuthContext
  { acTenantId :: UUID
  -- ^ Authenticated tenant ID
  , acActorId :: UUID
  -- ^ Authenticated actor ID
  , acPermissions :: [Permission]
  -- ^ Actor's permissions
  }
  deriving stock (Show, Eq, Generic)

-- | Permission types per SSOT 10.3.
data Permission
  = PermDocumentRead
  | PermDocumentWrite
  | PermSnapshotRead
  | PermSnapshotWrite
  | PermSnapshotSeal
  | PermPassportRead
  | PermPassportWrite
  | PermPassportSign
  | PermPassportActivate
  | PermPassportRevoke
  | PermRuleRead
  | PermRuleWrite
  | PermRulePublish
  | PermAuditRead
  | PermWebhookManage
  | PermAdmin
  deriving stock (Show, Eq, Ord, Generic)

-- | All permissions (for admin role).
allPermissions :: [Permission]
allPermissions =
  [ PermDocumentRead, PermDocumentWrite
  , PermSnapshotRead, PermSnapshotWrite, PermSnapshotSeal
  , PermPassportRead, PermPassportWrite, PermPassportSign
  , PermPassportActivate, PermPassportRevoke
  , PermRuleRead, PermRuleWrite, PermRulePublish
  , PermAuditRead
  , PermWebhookManage
  , PermAdmin
  ]

instance ToJSON Permission where
  toJSON = Aeson.String . permissionToText

instance FromJSON Permission where
  parseJSON = Aeson.withText "Permission" $ \t ->
    case textToPermission t of
      Just p -> pure p
      Nothing -> fail $ "Unknown permission: " <> T.unpack t

permissionToText :: Permission -> Text
permissionToText = \case
  PermDocumentRead -> "document:read"
  PermDocumentWrite -> "document:write"
  PermSnapshotRead -> "snapshot:read"
  PermSnapshotWrite -> "snapshot:write"
  PermSnapshotSeal -> "snapshot:seal"
  PermPassportRead -> "passport:read"
  PermPassportWrite -> "passport:write"
  PermPassportSign -> "passport:sign"
  PermPassportActivate -> "passport:activate"
  PermPassportRevoke -> "passport:revoke"
  PermRuleRead -> "rule:read"
  PermRuleWrite -> "rule:write"
  PermRulePublish -> "rule:publish"
  PermAuditRead -> "audit:read"
  PermWebhookManage -> "webhook:manage"
  PermAdmin -> "admin"

textToPermission :: Text -> Maybe Permission
textToPermission = \case
  "document:read" -> Just PermDocumentRead
  "document:write" -> Just PermDocumentWrite
  "snapshot:read" -> Just PermSnapshotRead
  "snapshot:write" -> Just PermSnapshotWrite
  "snapshot:seal" -> Just PermSnapshotSeal
  "passport:read" -> Just PermPassportRead
  "passport:write" -> Just PermPassportWrite
  "passport:sign" -> Just PermPassportSign
  "passport:activate" -> Just PermPassportActivate
  "passport:revoke" -> Just PermPassportRevoke
  "rule:read" -> Just PermRuleRead
  "rule:write" -> Just PermRuleWrite
  "rule:publish" -> Just PermRulePublish
  "audit:read" -> Just PermAuditRead
  "webhook:manage" -> Just PermWebhookManage
  "admin" -> Just PermAdmin
  _ -> Nothing

-- | Pagination parameters.
data PaginationParams = PaginationParams
  { ppLimit :: Int
  -- ^ Items per page (1-200, default 20)
  , ppCursor :: Maybe Cursor
  -- ^ Cursor for next page
  }
  deriving stock (Show, Eq, Generic)

-- | Default pagination params.
defaultPaginationParams :: PaginationParams
defaultPaginationParams = PaginationParams
  { ppLimit = 20
  , ppCursor = Nothing
  }

-- | Alias for defaultPaginationParams.
defaultPagination :: PaginationParams
defaultPagination = defaultPaginationParams

-- | Cursor for pagination.
--
-- Encodes (timestamp, id) as base64 for opaque cursor.
data Cursor = Cursor
  { cursorTimestamp :: UTCTime
  , cursorId :: UUID
  }
  deriving stock (Show, Eq, Generic)

-- | Parse base64-encoded cursor.
parseCursor :: Text -> Either Text Cursor
parseCursor encoded = do
  bytes <- case B64.decode (TE.encodeUtf8 encoded) of
    Left err -> Left $ "Invalid cursor encoding: " <> T.pack err
    Right b -> Right b
  let parts = BS.split '|' bytes
  case parts of
    [tsBytes, idBytes] -> do
      timestamp <- case reads (BS.unpack tsBytes) of
        [(t, "")] -> Right t
        _ -> Left "Invalid timestamp in cursor"
      uuid <- case UUID.fromString (BS.unpack idBytes) of
        Just u -> Right u
        Nothing -> Left "Invalid UUID in cursor"
      Right $ Cursor timestamp uuid
    _ -> Left "Invalid cursor format"

-- | Encode cursor as base64.
encodeCursor :: Cursor -> Text
encodeCursor Cursor{..} =
  let bytes = BS.pack (show cursorTimestamp) <> "|" <>
              BS.pack (UUID.toString cursorId)
  in TE.decodeUtf8 $ B64.encode bytes

-- | Paginated response.
data CursorPage a = CursorPage
  { cpItems :: [a]
  -- ^ Items in this page
  , cpNextCursor :: Maybe Text
  -- ^ Cursor for next page (null if last page)
  , cpHasMore :: Bool
  -- ^ Whether more items exist
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON a => ToJSON (CursorPage a) where
  toJSON CursorPage{..} = object
    [ "items" .= cpItems
    , "next_cursor" .= cpNextCursor
    , "has_more" .= cpHasMore
    ]

-- | Success envelope for single resource responses.
data SuccessEnvelope a = SuccessEnvelope
  { seData :: a
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON a => ToJSON (SuccessEnvelope a) where
  toJSON SuccessEnvelope{..} = object ["data" .= seData]

-- | Response for created resources (201).
data CreatedResponse = CreatedResponse
  { crId :: UUID
  , crLocation :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CreatedResponse where
  toJSON CreatedResponse{..} = object
    [ "id" .= crId
    , "location" .= crLocation
    ]

-- | Response for accepted async operations (202).
data AcceptedResponse = AcceptedResponse
  { arJobId :: UUID
  , arStatus :: Text
  , arStatusUrl :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AcceptedResponse where
  toJSON AcceptedResponse{..} = object
    [ "job_id" .= arJobId
    , "status" .= arStatus
    , "status_url" .= arStatusUrl
    ]

-- | Result of passport replay verification.
data ReplayResult = ReplayResult
  { rrVerified :: Bool
  -- ^ True if all hashes match
  , rrMismatches :: [ReplayMismatch]
  -- ^ List of mismatches (empty if verified)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ReplayResult where
  toJSON ReplayResult{..} = object
    [ "verified" .= rrVerified
    , "mismatches" .= rrMismatches
    ]

-- | Single mismatch in replay verification.
data ReplayMismatch = ReplayMismatch
  { rmField :: Text
  -- ^ Field that mismatched
  , rmExpected :: Text
  -- ^ Expected hash
  , rmActual :: Text
  -- ^ Actual hash
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ReplayMismatch where
  toJSON ReplayMismatch{..} = object
    [ "field" .= rmField
    , "expected" .= rmExpected
    , "actual" .= rmActual
    ]
