# Data Model: Advanced Features

**Feature**: 08-advanced-features
**Date**: 2025-12-28
**SSOT Reference**: Sections 6.4-6.5, 7.9-7.11, 9.5

## Policy Types

```haskell
data Policy = Policy
  { policyId  :: PolicyId
  , tenantId  :: TenantId
  , name      :: Text
  , createdAt :: UTCTime
  , createdBy :: Maybe ActorId
  }

data PolicyVersion = PolicyVersion
  { policyVersionId   :: PolicyVersionId
  , tenantId          :: TenantId
  , policyId          :: PolicyId
  , version           :: Int
  , isActive          :: Bool
  , effect            :: AccessDecision
  , target            :: Text           -- Path pattern
  , priority          :: Int            -- Lower = higher priority
  , policyJson        :: Value
  , policyCanonical   :: ByteString
  , policyHash        :: ByteString
  , createdAt         :: UTCTime
  }

data AccessDecision = ALLOW | DENY
  deriving (Eq, Show)

-- Policy evaluation input
data PolicyEvalInput = PolicyEvalInput
  { peiPath      :: Text
  , peiMethod    :: Text
  , peiActorId   :: ActorId
  , peiResource  :: Maybe UUID
  }

-- Policy evaluation result
data PolicyDecision
  = PolicyAllow
  | PolicyDeny Text  -- Reason/policy name
  | PolicyNoMatch    -- Fall back to RBAC
```

## Rate Limit Types

```haskell
data RateLimitBucket = RateLimitBucket
  { rlbTenantId        :: TenantId
  , rlbKeyHash         :: ByteString
  , rlbCapacity        :: Int
  , rlbTokens          :: Rational
  , rlbRefillPerSecond :: Rational
  , rlbUpdatedAt       :: UTCTime
  }

data RateLimitConfig = RateLimitConfig
  { rlcEnabled :: Bool
  , rlcRps     :: Int     -- Refill tokens per second
  , rlcBurst   :: Int     -- Bucket capacity
  }

data RateLimitResult
  = RateLimitAllowed
  | RateLimitExceeded { retryAfter :: Int }
```

## Webhook Types

```haskell
data WebhookEndpoint = WebhookEndpoint
  { weWebhookEndpointId :: WebhookEndpointId
  , weTenantId          :: TenantId
  , weUrl               :: Text
  , weSecretBase64      :: Text
  , weIsActive          :: Bool
  , weCreatedAt         :: UTCTime
  , weCreatedBy         :: Maybe ActorId
  }

data WebhookSubscription = WebhookSubscription
  { wsWebhookSubscriptionId :: WebhookSubscriptionId
  , wsTenantId              :: TenantId
  , wsWebhookEndpointId     :: WebhookEndpointId
  , wsEventType             :: Text
  , wsCreatedAt             :: UTCTime
  }

data WebhookDelivery = WebhookDelivery
  { wdWebhookDeliveryId   :: WebhookDeliveryId
  , wdTenantId            :: TenantId
  , wdWebhookEndpointId   :: WebhookEndpointId
  , wdEventId             :: EventId
  , wdAttempt             :: Int
  , wdStatus              :: WebhookDeliveryStatus
  , wdLastError           :: Maybe Value
  , wdCreatedAt           :: UTCTime
  , wdDeliveredAt         :: Maybe UTCTime
  }

data WebhookDeliveryStatus = PENDING | DELIVERED | FAILED
  deriving (Eq, Show)

-- Webhook event types (SSOT 10.6)
webhookEventTypes :: [Text]
webhookEventTypes =
  [ "passport.version.signed"
  , "passport.version.active"
  , "passport.version.revoked"
  , "rules.version.published"
  , "document.version.validated"
  ]
```

## Idempotency Types

```haskell
data IdempotencyEntry = IdempotencyEntry
  { ieKey             :: Text
  , ieRequestHash     :: ByteString
  , ieResponseStatus  :: Int
  , ieResponseHeaders :: Value
  , ieResponseBody    :: ByteString
  , ieCreatedAt       :: UTCTime
  }

data IdempotencyResult
  = IdempotencyNew                    -- First request
  | IdempotencyReplay IdempotencyEntry  -- Same request, replay response
  | IdempotencyConflict               -- Different request with same key
```

## Retention Types

```haskell
data RetentionConfig = RetentionConfig
  { rcEventsDays           :: Int  -- Default: 5475 (15 years)
  , rcPassportVersionsDays :: Int  -- Default: 5475 (15 years)
  , rcDocumentVersionsDays :: Int  -- Default: 3650 (10 years)
  }

data RetentionResult = RetentionResult
  { rrEventsDeleted           :: Int
  , rrPassportVersionsDeleted :: Int
  , rrDocumentVersionsDeleted :: Int
  , rrCutoffDate              :: UTCTime
  }

-- RetentionApplied event payload
data RetentionAppliedPayload = RetentionAppliedPayload
  { rapEventsDeleted           :: Int
  , rapPassportVersionsDeleted :: Int
  , rapDocumentVersionsDeleted :: Int
  , rapCutoffDate              :: UTCTime
  }
```

## Audit Event Types

```haskell
-- AccessDenied event payload
data AccessDeniedPayload = AccessDeniedPayload
  { adpPermission    :: Permission
  , adpReason        :: Text        -- "RBAC" | "Policy:<name>"
  , adpResource      :: Maybe Text
  , adpCorrelationId :: UUID
  }

-- PolicyDeny event payload
data PolicyDenyPayload = PolicyDenyPayload
  { pdpPolicyName    :: Text
  , pdpPolicyVersion :: Int
  , pdpTarget        :: Text
  , pdpCorrelationId :: UUID
  }
```
