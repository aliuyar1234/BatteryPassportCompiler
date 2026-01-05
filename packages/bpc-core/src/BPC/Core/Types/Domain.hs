{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | BPC.Core.Types.Domain - Domain-specific type wrappers
--
-- Newtype wrappers for all domain identifiers to provide
-- compile-time type safety and prevent ID mix-ups.
module BPC.Core.Types.Domain
  ( -- * Tenant & Actor IDs
    TenantId (..)
  , ActorId (..)
  , ApiKeyId (..)
  , RoleId (..)

    -- * Document IDs
  , DocumentId (..)
  , DocumentVersionId (..)
  , FactId (..)

    -- * Snapshot IDs
  , SnapshotId (..)

    -- * Rule IDs
  , RulePackageId (..)
  , RulePackageVersionId (..)
  , RuleFieldId (..)
  , RuleTestRunId (..)

    -- * Passport IDs
  , BatteryProductId (..)
  , PassportId (..)
  , PassportVersionId (..)

    -- * System IDs
  , EventId (..)
  , JobId (..)
  , IdempotencyKeyId (..)

    -- * Policy IDs
  , PolicyId (..)
  , PolicyVersionId (..)

    -- * Webhook IDs
  , WebhookEndpointId (..)
  , WebhookSubscriptionId (..)
  , WebhookDeliveryId (..)

    -- * Rate Limit IDs
  , RateLimitBucketId (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import GHC.Generics (Generic)

-- | Tenant identifier - root of multi-tenant isolation
newtype TenantId = TenantId { unTenantId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Actor identifier - users, API clients, or services
newtype ActorId = ActorId { unActorId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | API key identifier
newtype ApiKeyId = ApiKeyId { unApiKeyId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Role identifier
newtype RoleId = RoleId { unRoleId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Document identifier
newtype DocumentId = DocumentId { unDocumentId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Document version identifier
newtype DocumentVersionId = DocumentVersionId { unDocumentVersionId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Fact identifier
newtype FactId = FactId { unFactId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Snapshot identifier
newtype SnapshotId = SnapshotId { unSnapshotId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Rule package identifier
newtype RulePackageId = RulePackageId { unRulePackageId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Rule package version identifier
newtype RulePackageVersionId = RulePackageVersionId { unRulePackageVersionId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Rule field identifier
newtype RuleFieldId = RuleFieldId { unRuleFieldId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Rule test run identifier
newtype RuleTestRunId = RuleTestRunId { unRuleTestRunId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Battery product identifier
newtype BatteryProductId = BatteryProductId { unBatteryProductId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Passport identifier
newtype PassportId = PassportId { unPassportId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Passport version identifier
newtype PassportVersionId = PassportVersionId { unPassportVersionId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Event identifier (audit trail)
newtype EventId = EventId { unEventId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Job identifier (job queue)
newtype JobId = JobId { unJobId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Idempotency key identifier
newtype IdempotencyKeyId = IdempotencyKeyId { unIdempotencyKeyId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Policy identifier
newtype PolicyId = PolicyId { unPolicyId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Policy version identifier
newtype PolicyVersionId = PolicyVersionId { unPolicyVersionId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Webhook endpoint identifier
newtype WebhookEndpointId = WebhookEndpointId { unWebhookEndpointId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Webhook subscription identifier
newtype WebhookSubscriptionId = WebhookSubscriptionId { unWebhookSubscriptionId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Webhook delivery identifier
newtype WebhookDeliveryId = WebhookDeliveryId { unWebhookDeliveryId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Rate limit bucket identifier
newtype RateLimitBucketId = RateLimitBucketId { unRateLimitBucketId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)
