# Implementation Plan: Advanced Features

**Branch**: `08-advanced-features` | **Date**: 2025-12-28 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `.specify/features/08-advanced-features/spec.md`
**Phase**: P4 | **Packages**: bpc-api, bpc-db, bpc-worker | **Status**: Planning

## Summary

Implement advanced features beyond core functionality: Policy Engine for fine-grained access control, Token Bucket Rate Limiting, Webhook subscriptions with HMAC signatures, HTTP Idempotency Store, Data Retention jobs, and Audit Events for denied access.

## Technical Context

**Language/Version**: Haskell GHC 9.6.4
**Packages**: Cross-cutting (bpc-api, bpc-db, bpc-worker)
**Primary Dependencies**: cryptonite (HMAC), postgresql-simple
**Storage**: PostgreSQL (policies, webhooks, rate_limit_buckets tables)
**Testing**: Integration tests for all features
**Performance Goals**: Rate limit check < 5ms
**Constraints**: Policy evaluation must be fast (< 10ms)
**Scale/Scope**: ~1500 LOC across packages

## Constitution Check

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Determinism | N/A | Policies are dynamic access control |
| II. Canonical Storage | ENFORCED | policy_canonical stored |
| III. Immutability | ENFORCED | Policy versions are immutable |
| IV. Audit Trail | CRITICAL | Denied access MUST generate event |
| V. Layered Architecture | ENFORCED | Features span packages appropriately |
| VI. Type-Safe Rules | N/A | Policies use different DSL |

## Project Structure

```text
packages/bpc-api/src/BPC/API/
├── Middleware/
│   ├── RateLimit.hs      # Token bucket implementation
│   └── Policy.hs         # Policy evaluation middleware
└── Handlers/
    └── Policies.hs       # Policy CRUD endpoints

packages/bpc-db/src/BPC/DB/Repos/
├── Policies.hs           # Policy storage
├── RateLimits.hs         # Token bucket storage
└── Webhooks.hs           # Webhook endpoints/subscriptions

packages/bpc-worker/src/BPC/Worker/Handlers/
├── DeliverWebhook.hs     # HMAC-signed delivery
└── Retention.hs          # Data retention job
```

## Implementation Phases

### Phase 1: Policy Engine

```haskell
module BPC.API.Middleware.Policy where

-- Policy evaluation result
data PolicyDecision = Allow | Deny Text

-- Evaluate policies for a request
evaluatePolicy :: AuthContext -> Request -> AppM PolicyDecision
evaluatePolicy ctx req = do
  -- 1. Load active policies for target
  policies <- withConn $ \conn ->
    getActivePolicies conn (acTenantId ctx) (requestTarget req)

  -- 2. Sort by priority (lowest number = highest priority)
  let sorted = sortBy (comparing pvPriority) policies

  -- 3. First match wins
  case find (matchesRequest req) sorted of
    Just policy -> pure $ if pvEffect policy == ALLOW then Allow else Deny (pvName policy)
    Nothing -> pure Allow  -- Fall back to RBAC

-- Policy matching
matchesRequest :: Request -> PolicyVersion -> Bool
matchesRequest req pv =
  matchesTarget (requestPath req) (pvTarget pv) &&
  matchesConditions req (pvConditions pv)
```

### Phase 2: Rate Limiting (BPC-RL-1)

```haskell
module BPC.API.Middleware.RateLimit where

-- Token bucket rate limiter
checkRateLimit :: ApiKeyHash -> AppM (Either RateLimitExceeded ())
checkRateLimit keyHash = do
  config <- asks envRateLimitConfig

  withConn $ \conn -> do
    -- Atomic consume with FOR UPDATE
    result <- execute conn [sql|
      UPDATE rate_limit_buckets
      SET tokens = GREATEST(0,
        LEAST(capacity, tokens + (EXTRACT(EPOCH FROM (now() - updated_at)) * refill_per_second)) - 1
      ),
      updated_at = now()
      WHERE tenant_id = ? AND key_hash = ?
      RETURNING tokens
    |] (tenantId, keyHash)

    case result of
      [Only tokens] | tokens >= 0 -> pure $ Right ()
      _ -> do
        retryAfter <- computeRetryAfter conn keyHash
        pure $ Left $ RateLimitExceeded retryAfter

-- Rate limit middleware
rateLimitMiddleware :: Middleware
rateLimitMiddleware app req respond = do
  case extractApiKeyHash req of
    Nothing -> app req respond  -- No rate limit for unauthenticated
    Just keyHash -> do
      result <- checkRateLimit keyHash
      case result of
        Right () -> app req respond
        Left (RateLimitExceeded retryAfter) ->
          respond $ rateLimitedResponse retryAfter
```

### Phase 3: Webhook Subscriptions

```haskell
module BPC.DB.Repos.Webhooks where

-- Create webhook endpoint
createWebhookEndpoint :: Connection -> TenantId -> CreateEndpointInput -> IO WebhookEndpointId

-- Subscribe to event type
createSubscription :: Connection -> TenantId -> WebhookEndpointId -> Text -> IO WebhookSubscriptionId

-- Event types (SSOT 10.6)
data WebhookEventType
  = PassportVersionSigned    -- passport.version.signed
  | PassportVersionActive    -- passport.version.active
  | PassportVersionRevoked   -- passport.version.revoked
  | RulesVersionPublished    -- rules.version.published
  | DocumentVersionValidated -- document.version.validated

-- Trigger webhook on event
triggerWebhook :: Connection -> TenantId -> EventId -> Text -> IO ()
triggerWebhook conn tenantId eventId eventType = do
  -- Find matching subscriptions
  subs <- getSubscriptionsByEventType conn tenantId eventType

  -- Create delivery jobs
  forM_ subs $ \sub ->
    enqueue conn tenantId EnqueueInput
      { eiType = DELIVER_WEBHOOK
      , eiPayload = object ["webhook_delivery_id" .= ...]
      , eiIdempotencyKey = "webhook-" <> show eventId <> "-" <> show (wsEndpointId sub)
      }
```

### Phase 4: Webhook HMAC Signature (BPC-WH-1)

```haskell
module BPC.Worker.Handlers.DeliverWebhook where

-- Compute HMAC-SHA256 signature
computeWebhookSignature :: ByteString -> ByteString -> ByteString
computeWebhookSignature secret body =
  HMAC.hmac secret body :: HMAC.HMAC SHA256

-- Deliver webhook with signature
deliverWebhook :: Pool Connection -> Job -> IO ()
deliverWebhook pool job = do
  -- ... load delivery, endpoint, event ...

  let body = encode eventPayload
      signature = computeWebhookSignature (weSecret endpoint) (toStrict body)
      signatureHeader = "sha256=" <> encodeHex signature

  response <- httpPost (weUrl endpoint)
    [ ("X-BPC-Signature", signatureHeader)
    , ("Content-Type", "application/json")
    ]
    body

  -- Update delivery status...
```

### Phase 5: HTTP Idempotency Store (BPC-IDEMP-1)

```haskell
module BPC.DB.Repos.Idempotency where

data IdempotencyEntry = IdempotencyEntry
  { ieKey           :: Text
  , ieRequestHash   :: ByteString
  , ieResponseStatus :: Int
  , ieResponseHeaders :: Value
  , ieResponseBody  :: ByteString
  , ieCreatedAt     :: UTCTime
  }

-- Lookup existing entry
lookupIdempotencyKey :: Connection -> TenantId -> Text -> IO (Maybe IdempotencyEntry)

-- Store new entry
storeIdempotencyKey :: Connection -> TenantId -> Text -> ByteString -> Response -> IO ()

-- Garbage collect old entries (> 24h)
cleanupIdempotencyKeys :: Connection -> IO Int
```

### Phase 6: Data Retention Job

```haskell
module BPC.Worker.Handlers.Retention where

-- Retention job runs daily
runRetention :: Pool Connection -> IO ()
runRetention pool = do
  config <- loadRetentionConfig

  -- Delete old events
  eventsDeleted <- withConn pool $ \conn ->
    deleteEventsOlderThan conn (rcEventsDays config)

  -- Delete old passport versions
  passportVersionsDeleted <- withConn pool $ \conn ->
    deletePassportVersionsOlderThan conn (rcPassportVersionsDays config)

  -- Delete old document versions
  documentVersionsDeleted <- withConn pool $ \conn ->
    deleteDocumentVersionsOlderThan conn (rcDocumentVersionsDays config)

  -- Emit RetentionApplied event
  withConn pool $ \conn ->
    appendEvent conn AppendEventInput
      { aeiAggregateType = "System"
      , aeiAggregateId = systemUUID
      , aeiEventType = "RetentionApplied"
      , aeiPayload = object
          [ "events_deleted" .= eventsDeleted
          , "passport_versions_deleted" .= passportVersionsDeleted
          , "document_versions_deleted" .= documentVersionsDeleted
          , "cutoff_date" .= cutoffDate
          ]
      }

-- Retention defaults (SSOT 4.6)
data RetentionConfig = RetentionConfig
  { rcEventsDays           :: Int  -- 5475 (15 years)
  , rcPassportVersionsDays :: Int  -- 5475 (15 years)
  , rcDocumentVersionsDays :: Int  -- 3650 (10 years)
  }
```

### Phase 7: Denied Access Audit Events

```haskell
module BPC.API.Middleware.Auth where

-- On denied access, emit audit event
denyAccess :: AuthContext -> Permission -> AppM a
denyAccess ctx permission = do
  correlationId <- asks envCorrelationId

  -- Emit AccessDenied event
  withConn $ \conn ->
    appendEvent conn AppendEventInput
      { aeiTenantId = acTenantId ctx
      , aeiAggregateType = "Actor"
      , aeiAggregateId = unActorId (acActorId ctx)
      , aeiEventType = "AccessDenied"
      , aeiActorId = Just (acActorId ctx)
      , aeiPayload = object
          [ "permission" .= permission
          , "correlation_id" .= correlationId
          ]
      }

  throwError $ FORBIDDEN permission
```

## Database Tables (SSOT 6.3-6.5)

```sql
-- Policies (Migration 003)
policies (policy_id, tenant_id, name)
policy_versions (policy_version_id, tenant_id, policy_id, version,
                 is_active, effect, target, priority,
                 policy_json, policy_canonical, policy_hash)

-- Webhooks (Migration 003)
webhook_endpoints (webhook_endpoint_id, tenant_id, url, secret_base64, is_active)
webhook_subscriptions (webhook_subscription_id, tenant_id, webhook_endpoint_id, event_type)
webhook_deliveries (webhook_delivery_id, tenant_id, webhook_endpoint_id, event_id,
                    attempt, status, last_error, delivered_at)

-- Rate Limits (Migration 004)
rate_limit_buckets (tenant_id, key_hash, capacity, tokens, refill_per_second, updated_at)
```

## Verification Checklist

- [ ] Policy Engine evaluates first-match by priority
- [ ] DENY policy blocks despite RBAC permission
- [ ] Rate limiter returns 429 with Retry-After
- [ ] Rate limit is per-API-key, not per-tenant
- [ ] Webhook signature is HMAC-SHA256
- [ ] Webhook receiver can verify signature
- [ ] Idempotency key replays same response
- [ ] Different request with same key returns 409
- [ ] Retention job deletes only expired data
- [ ] RetentionApplied event is emitted
- [ ] All denied access generates audit event
- [ ] Integration tests pass
