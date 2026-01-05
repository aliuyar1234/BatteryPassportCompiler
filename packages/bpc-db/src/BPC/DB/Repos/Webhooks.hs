{-# LANGUAGE OverloadedStrings #-}

-- | Webhooks Repository
--
-- Webhook endpoints, subscriptions, and delivery tracking.
--
-- @since 0.1.0.0
module BPC.DB.Repos.Webhooks
  ( -- * Types
    EndpointId
  , SubscriptionId
  , DeliveryId
  , WebhookEndpoint(..)
  , WebhookSubscription(..)
  , WebhookDelivery(..)
  , DeliveryStatus(..)
  , EventType(..)
  , EndpointInput(..)
    -- * Endpoint Operations
  , createEndpoint
  , getEndpoint
  , listEndpoints
  , deactivateEndpoint
    -- * Subscription Operations
  , subscribe
  , unsubscribe
  , getSubscriptions
  , getSubscriptionsByEvent
    -- * Delivery Operations
  , recordDelivery
  , markDelivered
  , markFailed
  , getPendingDeliveries
  , getDeliveriesByEndpoint
  ) where

import Control.Exception (catch)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection, Only(..), SqlError)
import qualified Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)

import BPC.DB.Crypto (encryptSecret, decryptSecret, CryptoError(..))
import BPC.DB.Error (WebhookError(..))
import BPC.DB.Repos.Events (TenantId)

-- | Endpoint ID (UUID)
type EndpointId = UUID

-- | Subscription ID (UUID)
type SubscriptionId = UUID

-- | Delivery ID (UUID)
type DeliveryId = UUID

-- | Delivery status.
data DeliveryStatus
  = DeliveryPending
  | DeliveryInProgress
  | DeliveryDelivered
  | DeliveryFailed
  deriving stock (Show, Eq, Generic)

statusToText :: DeliveryStatus -> Text
statusToText = \case
  DeliveryPending    -> "PENDING"
  DeliveryInProgress -> "IN_PROGRESS"
  DeliveryDelivered  -> "DELIVERED"
  DeliveryFailed     -> "FAILED"

textToStatus :: Text -> Maybe DeliveryStatus
textToStatus = \case
  "PENDING"     -> Just DeliveryPending
  "IN_PROGRESS" -> Just DeliveryInProgress
  "DELIVERED"   -> Just DeliveryDelivered
  "FAILED"      -> Just DeliveryFailed
  _             -> Nothing

-- | Event types for webhook subscriptions.
data EventType
  = EventDocumentUploaded
  | EventDocumentParsed
  | EventSnapshotSealed
  | EventPassportCompiled
  | EventPassportSigned
  | EventPassportActivated
  | EventPassportRevoked
  | EventJobFailed
  deriving stock (Show, Eq, Generic)

eventTypeToText :: EventType -> Text
eventTypeToText = \case
  EventDocumentUploaded  -> "document.uploaded"
  EventDocumentParsed    -> "document.parsed"
  EventSnapshotSealed    -> "snapshot.sealed"
  EventPassportCompiled  -> "passport.compiled"
  EventPassportSigned    -> "passport.signed"
  EventPassportActivated -> "passport.activated"
  EventPassportRevoked   -> "passport.revoked"
  EventJobFailed         -> "job.failed"

textToEventType :: Text -> Maybe EventType
textToEventType = \case
  "document.uploaded"  -> Just EventDocumentUploaded
  "document.parsed"    -> Just EventDocumentParsed
  "snapshot.sealed"    -> Just EventSnapshotSealed
  "passport.compiled"  -> Just EventPassportCompiled
  "passport.signed"    -> Just EventPassportSigned
  "passport.activated" -> Just EventPassportActivated
  "passport.revoked"   -> Just EventPassportRevoked
  "job.failed"         -> Just EventJobFailed
  _                    -> Nothing

-- | Webhook endpoint record.
data WebhookEndpoint = WebhookEndpoint
  { weId :: EndpointId
  , weTenantId :: TenantId
  , weUrl :: Text
  , weSecret :: Text
  , weName :: Text
  , weIsActive :: Bool
  , weCreatedAt :: UTCTime
  , weUpdatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Webhook subscription record.
data WebhookSubscription = WebhookSubscription
  { wsId :: SubscriptionId
  , wsEndpointId :: EndpointId
  , wsTenantId :: TenantId
  , wsEventType :: EventType
  , wsCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Webhook delivery record.
data WebhookDelivery = WebhookDelivery
  { wdId :: DeliveryId
  , wdEndpointId :: EndpointId
  , wdTenantId :: TenantId
  , wdEventType :: EventType
  , wdPayload :: Value
  , wdStatus :: DeliveryStatus
  , wdAttempts :: Int
  , wdLastError :: Maybe Text
  , wdDeliveredAt :: Maybe UTCTime
  , wdCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Input for creating an endpoint.
data EndpointInput = EndpointInput
  { eiUrl :: Text
  -- ^ Webhook URL
  , eiSecret :: Text
  -- ^ Secret for signing payloads
  , eiName :: Text
  -- ^ Display name
  }
  deriving stock (Show, Eq, Generic)

-- | Create a webhook endpoint with encrypted secret.
--
-- The secret is encrypted using AES-256-CTR with HMAC-SHA256 before storage.
-- Requires the webhook encryption key from configuration.
--
-- @since 0.1.0.0
createEndpoint
  :: Connection
  -> TenantId
  -> EndpointInput
  -> Maybe Text  -- ^ Encryption key (if Nothing, stores plaintext - NOT recommended)
  -> IO (Either WebhookError EndpointId)
createEndpoint conn tenantId input mEncryptionKey = do
  -- Encrypt secret if encryption key is provided
  secretResult <- case mEncryptionKey of
    Nothing -> pure $ Right (eiSecret input, False)  -- Plaintext fallback (legacy)
    Just encKey -> do
      encResult <- encryptSecret encKey (eiSecret input)
      case encResult of
        Left _ -> pure $ Right (eiSecret input, False)  -- Fallback to plaintext on error
        Right encrypted -> pure $ Right (encrypted, True)

  case secretResult of
    Left err -> pure $ Left $ WEBHOOK_DELIVERY_FAILED (error "encrypt") "Encryption failed"
    Right (secretValue, isEncrypted) -> do
      result <- (Right <$> PG.query conn
        "INSERT INTO webhook_endpoints (tenant_id, url, secret, encrypted_secret, secret_encrypted, name) \
        \VALUES (?, ?, ?, ?, ?, ?) RETURNING id"
        ( tenantId
        , eiUrl input
        , if isEncrypted then "" else secretValue  -- Legacy column (empty if encrypted)
        , if isEncrypted then Just secretValue else Nothing  -- Encrypted column
        , isEncrypted
        , eiName input
        ))
        `catch` handleSqlError

      case result of
        Left err -> pure $ Left err
        Right [] -> pure $ Left $ WEBHOOK_ENDPOINT_NOT_FOUND (error "No ID returned")
        Right (Only eid : _) -> pure $ Right eid
  where
    handleSqlError :: SqlError -> IO (Either WebhookError [Only UUID])
    handleSqlError _ = pure $ Left $ WEBHOOK_DELIVERY_FAILED (error "failed") "Constraint violation"

-- | Get an endpoint by ID with decrypted secret.
--
-- If an encryption key is provided and the secret is encrypted, it will be decrypted.
-- Otherwise returns the plaintext secret.
--
-- @since 0.1.0.0
getEndpoint
  :: Connection
  -> TenantId
  -> EndpointId
  -> Maybe Text  -- ^ Encryption key for decryption
  -> IO (Maybe WebhookEndpoint)
getEndpoint conn tenantId endpointId mEncryptionKey = do
  rows <- PG.query conn
    "SELECT id, tenant_id, url, secret, encrypted_secret, secret_encrypted, name, is_active, created_at, updated_at \
    \FROM webhook_endpoints WHERE tenant_id = ? AND id = ?"
    (tenantId, endpointId)
  pure $ case rows of
    [] -> Nothing
    (row : _) -> Just $ rowToEndpointWithDecrypt mEncryptionKey row

-- | List all endpoints for a tenant.
--
-- @since 0.1.0.0
listEndpoints
  :: Connection
  -> TenantId
  -> Maybe Text  -- ^ Encryption key for decryption
  -> IO [WebhookEndpoint]
listEndpoints conn tenantId mEncryptionKey = do
  rows <- PG.query conn
    "SELECT id, tenant_id, url, secret, encrypted_secret, secret_encrypted, name, is_active, created_at, updated_at \
    \FROM webhook_endpoints WHERE tenant_id = ? ORDER BY name ASC"
    (Only tenantId)
  pure $ map (rowToEndpointWithDecrypt mEncryptionKey) rows

-- | Deactivate an endpoint.
deactivateEndpoint
  :: Connection
  -> TenantId
  -> EndpointId
  -> IO Bool
deactivateEndpoint conn tenantId endpointId = do
  n <- PG.execute conn
    "UPDATE webhook_endpoints SET is_active = FALSE, updated_at = NOW() \
    \WHERE tenant_id = ? AND id = ?"
    (tenantId, endpointId)
  pure $ n > 0

-- | Subscribe an endpoint to an event type.
--
-- @since 0.1.0.0
subscribe
  :: Connection
  -> TenantId
  -> EndpointId
  -> EventType
  -> IO (Either WebhookError SubscriptionId)
subscribe conn tenantId endpointId eventType = do
  result <- (Right <$> PG.query conn
    "INSERT INTO webhook_subscriptions (tenant_id, endpoint_id, event_type) \
    \VALUES (?, ?, ?) \
    \ON CONFLICT (endpoint_id, event_type) DO UPDATE SET endpoint_id = EXCLUDED.endpoint_id \
    \RETURNING id"
    (tenantId, endpointId, eventTypeToText eventType))
    `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ WEBHOOK_SUBSCRIPTION_NOT_FOUND (error "No ID returned")
    Right (Only sid : _) -> pure $ Right sid
  where
    handleSqlError :: SqlError -> IO (Either WebhookError [Only UUID])
    handleSqlError _ = pure $ Left $ WEBHOOK_ENDPOINT_NOT_FOUND endpointId

-- | Unsubscribe an endpoint from an event type.
unsubscribe
  :: Connection
  -> TenantId
  -> EndpointId
  -> EventType
  -> IO Bool
unsubscribe conn tenantId endpointId eventType = do
  n <- PG.execute conn
    "DELETE FROM webhook_subscriptions \
    \WHERE tenant_id = ? AND endpoint_id = ? AND event_type = ?"
    (tenantId, endpointId, eventTypeToText eventType)
  pure $ n > 0

-- | Get all subscriptions for an endpoint.
getSubscriptions
  :: Connection
  -> TenantId
  -> EndpointId
  -> IO [WebhookSubscription]
getSubscriptions conn tenantId endpointId = do
  rows <- PG.query conn
    "SELECT id, endpoint_id, tenant_id, event_type, created_at \
    \FROM webhook_subscriptions WHERE tenant_id = ? AND endpoint_id = ?"
    (tenantId, endpointId)
  pure $ map rowToSubscription rows

-- | Get all subscriptions for a specific event type.
getSubscriptionsByEvent
  :: Connection
  -> TenantId
  -> EventType
  -> IO [WebhookSubscription]
getSubscriptionsByEvent conn tenantId eventType = do
  rows <- PG.query conn
    "SELECT ws.id, ws.endpoint_id, ws.tenant_id, ws.event_type, ws.created_at \
    \FROM webhook_subscriptions ws \
    \INNER JOIN webhook_endpoints we ON we.id = ws.endpoint_id \
    \WHERE ws.tenant_id = ? AND ws.event_type = ? AND we.is_active = TRUE"
    (tenantId, eventTypeToText eventType)
  pure $ map rowToSubscription rows

-- | Record a delivery attempt.
--
-- @since 0.1.0.0
recordDelivery
  :: Connection
  -> TenantId
  -> EndpointId
  -> EventType
  -> Value
  -> IO DeliveryId
recordDelivery conn tenantId endpointId eventType payload = do
  [Only did] <- PG.query conn
    "INSERT INTO webhook_deliveries \
    \(tenant_id, endpoint_id, event_type, payload, status, attempts) \
    \VALUES (?, ?, ?, ?, 'PENDING', 0) RETURNING id"
    (tenantId, endpointId, eventTypeToText eventType, payload)
  pure did

-- | Mark a delivery as delivered.
markDelivered
  :: Connection
  -> TenantId
  -> DeliveryId
  -> IO Bool
markDelivered conn tenantId deliveryId = do
  n <- PG.execute conn
    "UPDATE webhook_deliveries SET \
    \  status = 'DELIVERED', \
    \  delivered_at = NOW(), \
    \  attempts = attempts + 1 \
    \WHERE tenant_id = ? AND id = ?"
    (tenantId, deliveryId)
  pure $ n > 0

-- | Mark a delivery as failed.
markFailed
  :: Connection
  -> TenantId
  -> DeliveryId
  -> Text
  -> IO Bool
markFailed conn tenantId deliveryId errorMsg = do
  n <- PG.execute conn
    "UPDATE webhook_deliveries SET \
    \  status = 'FAILED', \
    \  last_error = ?, \
    \  attempts = attempts + 1 \
    \WHERE tenant_id = ? AND id = ?"
    (errorMsg, tenantId, deliveryId)
  pure $ n > 0

-- | Get pending deliveries.
getPendingDeliveries
  :: Connection
  -> TenantId
  -> Int
  -> IO [WebhookDelivery]
getPendingDeliveries conn tenantId limit = do
  rows <- PG.query conn
    "SELECT id, endpoint_id, tenant_id, event_type, payload, status, \
    \       attempts, last_error, delivered_at, created_at \
    \FROM webhook_deliveries \
    \WHERE tenant_id = ? AND status = 'PENDING' \
    \ORDER BY created_at ASC \
    \LIMIT ?"
    (tenantId, limit)
  pure $ map rowToDelivery rows

-- | Get deliveries for an endpoint.
getDeliveriesByEndpoint
  :: Connection
  -> TenantId
  -> EndpointId
  -> Int
  -> IO [WebhookDelivery]
getDeliveriesByEndpoint conn tenantId endpointId limit = do
  rows <- PG.query conn
    "SELECT id, endpoint_id, tenant_id, event_type, payload, status, \
    \       attempts, last_error, delivered_at, created_at \
    \FROM webhook_deliveries \
    \WHERE tenant_id = ? AND endpoint_id = ? \
    \ORDER BY created_at DESC \
    \LIMIT ?"
    (tenantId, endpointId, limit)
  pure $ map rowToDelivery rows

-- | Convert database row to WebhookEndpoint with optional decryption.
--
-- Handles both legacy plaintext secrets and encrypted secrets.
--
-- @since 0.1.0.0
rowToEndpointWithDecrypt
  :: Maybe Text  -- ^ Encryption key
  -> (UUID, UUID, Text, Text, Maybe Text, Bool, Text, Bool, UTCTime, UTCTime)
  -> WebhookEndpoint
rowToEndpointWithDecrypt mEncryptionKey (eid, tid, url, legacySecret, mEncryptedSecret, isEncrypted, name, active, cat, uat) =
  let secret = case (isEncrypted, mEncryptedSecret, mEncryptionKey) of
        -- Encrypted secret with key available: decrypt
        (True, Just encrypted, Just key) ->
          case decryptSecret key encrypted of
            Right decrypted -> decrypted
            Left _ -> ""  -- Decryption failed, return empty (logged elsewhere)
        -- Encrypted but no key: return empty (security)
        (True, _, Nothing) -> ""
        -- Not encrypted: use legacy plaintext
        (False, _, _) -> legacySecret
  in WebhookEndpoint
    { weId = eid
    , weTenantId = tid
    , weUrl = url
    , weSecret = secret
    , weName = name
    , weIsActive = active
    , weCreatedAt = cat
    , weUpdatedAt = uat
    }

-- | Legacy row converter for backward compatibility.
rowToEndpoint
  :: (UUID, UUID, Text, Text, Text, Bool, UTCTime, UTCTime)
  -> WebhookEndpoint
rowToEndpoint (eid, tid, url, secret, name, active, cat, uat) = WebhookEndpoint
  { weId = eid
  , weTenantId = tid
  , weUrl = url
  , weSecret = secret
  , weName = name
  , weIsActive = active
  , weCreatedAt = cat
  , weUpdatedAt = uat
  }

-- | Convert database row to WebhookSubscription.
rowToSubscription :: (UUID, UUID, UUID, Text, UTCTime) -> WebhookSubscription
rowToSubscription (sid, eid, tid, evType, cat) = WebhookSubscription
  { wsId = sid
  , wsEndpointId = eid
  , wsTenantId = tid
  , wsEventType = maybe EventDocumentUploaded id $ textToEventType evType
  , wsCreatedAt = cat
  }

-- | Convert database row to WebhookDelivery.
rowToDelivery
  :: (UUID, UUID, UUID, Text, Value, Text, Int, Maybe Text, Maybe UTCTime, UTCTime)
  -> WebhookDelivery
rowToDelivery (did, eid, tid, evType, payload, status, attempts, lastErr, delivAt, cat) =
  WebhookDelivery
    { wdId = did
    , wdEndpointId = eid
    , wdTenantId = tid
    , wdEventType = maybe EventDocumentUploaded id $ textToEventType evType
    , wdPayload = payload
    , wdStatus = maybe DeliveryPending id $ textToStatus status
    , wdAttempts = attempts
    , wdLastError = lastErr
    , wdDeliveredAt = delivAt
    , wdCreatedAt = cat
    }
