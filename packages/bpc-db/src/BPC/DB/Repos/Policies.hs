{-# LANGUAGE OverloadedStrings #-}

-- | Policy Repository
--
-- Fine-grained access control policies beyond RBAC.
--
-- @since 0.1.0.0
module BPC.DB.Repos.Policies
  ( -- * Types
    Policy(..)
  , PolicyVersion(..)
  , PolicyEffect(..)
  , PolicyInput(..)
  , PolicyVersionInput(..)
    -- * Policy Operations
  , createPolicy
  , getPolicy
  , listPolicies
  , updatePolicy
    -- * Version Operations
  , createPolicyVersion
  , getPolicyVersion
  , getActivePolicyVersions
  , getActivePolicies
  , deactivatePolicyVersion
    -- * Evaluation
  , matchesRequest
  ) where

import Control.Monad (void)
import Data.Aeson (Value, ToJSON(..), FromJSON(..), encode, decode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics (Generic)

import BPC.Core.Canonical (encode)
import BPC.DB.Error

-- | Policy effect (ALLOW or DENY).
data PolicyEffect = Allow | Deny
  deriving stock (Show, Eq, Generic)

instance ToJSON PolicyEffect where
  toJSON Allow = "ALLOW"
  toJSON Deny = "DENY"

instance FromJSON PolicyEffect where
  parseJSON = Aeson.withText "PolicyEffect" $ \t ->
    case t of
      "ALLOW" -> pure Allow
      "DENY" -> pure Deny
      _ -> fail $ "Unknown policy effect: " ++ T.unpack t

-- | Policy.
data Policy = Policy
  { policyId :: UUID
  , policyTenantId :: UUID
  , policyName :: Text
  , policyDescription :: Maybe Text
  , policyIsActive :: Bool
  , policyPriority :: Int
  , policyCreatedAt :: UTCTime
  , policyUpdatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromRow Policy where
  fromRow = Policy <$> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field

-- | Policy version.
data PolicyVersion = PolicyVersion
  { pvId :: UUID
  , pvPolicyId :: UUID
  , pvTenantId :: UUID
  , pvVersion :: Int
  , pvPolicyHash :: Text
  , pvRules :: Value
  , pvEffect :: PolicyEffect
  , pvResources :: [Text]
  , pvActions :: [Text]
  , pvConditions :: Maybe Value
  , pvIsActive :: Bool
  , pvCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Policy input.
data PolicyInput = PolicyInput
  { piName :: Text
  , piDescription :: Maybe Text
  , piPriority :: Int
  }
  deriving stock (Show, Eq, Generic)

-- | Policy version input.
data PolicyVersionInput = PolicyVersionInput
  { pviRules :: Value
  , pviEffect :: PolicyEffect
  , pviResources :: [Text]
  , pviActions :: [Text]
  , pviConditions :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

type TenantId = UUID

-- | Create a new policy.
createPolicy :: Connection -> TenantId -> PolicyInput -> IO UUID
createPolicy conn tenantId PolicyInput{..} = do
  [Only policyId] <- query conn
    "INSERT INTO policies (tenant_id, name, description, priority) \
    \VALUES (?, ?, ?, ?) RETURNING id"
    (tenantId, piName, piDescription, piPriority)
  pure policyId

-- | Get a policy by ID.
getPolicy :: Connection -> TenantId -> UUID -> IO (Maybe Policy)
getPolicy conn tenantId policyId = do
  results <- query conn
    "SELECT id, tenant_id, name, description, is_active, priority, created_at, updated_at \
    \FROM policies WHERE tenant_id = ? AND id = ?"
    (tenantId, policyId)
  pure $ case results of
    [p] -> Just p
    _ -> Nothing

-- | List policies with cursor pagination.
--
-- Fetches policies ordered by (created_at DESC, id DESC).
-- If cursor is provided (timestamp, id), returns policies created before that point.
--
-- @since 0.1.0.0
listPolicies :: Connection -> TenantId -> Int -> Maybe (UTCTime, UUID) -> IO [Policy]
listPolicies conn tenantId limit cursor = do
  rows <- case cursor of
    Nothing ->
      query conn
        "SELECT id, tenant_id, name, description, is_active, priority, created_at, updated_at \
        \FROM policies \
        \WHERE tenant_id = ? \
        \ORDER BY created_at DESC, id DESC \
        \LIMIT ?"
        (tenantId, limit)
    Just (timestamp, cursorId) ->
      query conn
        "SELECT id, tenant_id, name, description, is_active, priority, created_at, updated_at \
        \FROM policies \
        \WHERE tenant_id = ? AND (created_at, id) < (?, ?) \
        \ORDER BY created_at DESC, id DESC \
        \LIMIT ?"
        (tenantId, timestamp, cursorId, limit)
  pure rows

-- | Update a policy.
updatePolicy :: Connection -> TenantId -> UUID -> PolicyInput -> IO Bool
updatePolicy conn tenantId policyId PolicyInput{..} = do
  rowCount <- execute conn
    "UPDATE policies SET name = ?, description = ?, priority = ?, updated_at = NOW() \
    \WHERE tenant_id = ? AND id = ?"
    (piName, piDescription, piPriority, tenantId, policyId)
  pure $ rowCount > 0

-- | Create a new policy version.
createPolicyVersion :: Connection -> TenantId -> UUID -> PolicyVersionInput -> IO UUID
createPolicyVersion conn tenantId policyId PolicyVersionInput{..} = do
  -- Get next version number
  [Only nextVersion] <- query conn
    "SELECT COALESCE(MAX(version), 0) + 1 FROM policy_versions WHERE policy_id = ?"
    (Only policyId)

  -- Compute policy hash from canonical JSON
  let rulesBytes = LBS.toStrict $ Aeson.encode pviRules
  let policyHash = computeHash rulesBytes

  [Only versionId] <- query conn
    "INSERT INTO policy_versions \
    \(policy_id, tenant_id, version, policy_hash, rules, effect, resources, actions, conditions) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id"
    (policyId, tenantId, nextVersion :: Int, policyHash, pviRules
    , effectToText pviEffect, pviResources, pviActions, pviConditions)
  pure versionId

-- | Get a policy version by ID.
getPolicyVersion :: Connection -> TenantId -> UUID -> IO (Maybe PolicyVersion)
getPolicyVersion conn tenantId versionId = do
  results <- query conn
    "SELECT id, policy_id, tenant_id, version, policy_hash, rules, effect, \
    \resources, actions, conditions, is_active, created_at \
    \FROM policy_versions WHERE tenant_id = ? AND id = ?"
    (tenantId, versionId)
  pure $ case results of
    [row] -> Just $ rowToPolicyVersion row
    _ -> Nothing

-- | Get active policy versions for a resource/action.
getActivePolicies :: Connection -> TenantId -> Text -> IO [PolicyVersion]
getActivePolicies conn tenantId resource = do
  rows <- query conn
    "SELECT pv.id, pv.policy_id, pv.tenant_id, pv.version, pv.policy_hash, pv.rules, \
    \pv.effect, pv.resources, pv.actions, pv.conditions, pv.is_active, pv.created_at \
    \FROM policy_versions pv \
    \JOIN policies p ON pv.policy_id = p.id \
    \WHERE pv.tenant_id = ? AND pv.is_active = TRUE AND p.is_active = TRUE \
    \AND (? = ANY(pv.resources) OR '*' = ANY(pv.resources)) \
    \ORDER BY p.priority ASC"
    (tenantId, resource)
  pure $ map rowToPolicyVersion rows

-- | Get all active policy versions for a tenant.
getActivePolicyVersions :: Connection -> TenantId -> IO [PolicyVersion]
getActivePolicyVersions conn tenantId = do
  rows <- query conn
    "SELECT pv.id, pv.policy_id, pv.tenant_id, pv.version, pv.policy_hash, pv.rules, \
    \pv.effect, pv.resources, pv.actions, pv.conditions, pv.is_active, pv.created_at \
    \FROM policy_versions pv \
    \JOIN policies p ON pv.policy_id = p.id \
    \WHERE pv.tenant_id = ? AND pv.is_active = TRUE AND p.is_active = TRUE \
    \ORDER BY p.priority ASC"
    (Only tenantId)
  pure $ map rowToPolicyVersion rows

-- | Deactivate a policy version.
deactivatePolicyVersion :: Connection -> TenantId -> UUID -> IO Bool
deactivatePolicyVersion conn tenantId versionId = do
  rowCount <- execute conn
    "UPDATE policy_versions SET is_active = FALSE WHERE tenant_id = ? AND id = ?"
    (tenantId, versionId)
  pure $ rowCount > 0

-- | Check if a policy matches a request.
matchesRequest :: Text -> Text -> PolicyVersion -> Bool
matchesRequest resource action pv =
  let resourceMatch = resource `elem` pvResources pv || "*" `elem` pvResources pv
      actionMatch = action `elem` pvActions pv || "*" `elem` pvActions pv
  in resourceMatch && actionMatch

-- Helper functions

effectToText :: PolicyEffect -> Text
effectToText Allow = "ALLOW"
effectToText Deny = "DENY"

textToEffect :: Text -> PolicyEffect
textToEffect "ALLOW" = Allow
textToEffect "DENY" = Deny
textToEffect _ = Deny  -- Default to deny for safety

rowToPolicyVersion :: (UUID, UUID, UUID, Int, Text, Value, Text, [Text], [Text], Maybe Value, Bool, UTCTime) -> PolicyVersion
rowToPolicyVersion (id', policyId, tenantId, version, hash, rules, effect, resources, actions, conditions, isActive, createdAt) =
  PolicyVersion id' policyId tenantId version hash rules (textToEffect effect) resources actions conditions isActive createdAt

computeHash :: BS.ByteString -> Text
computeHash bs = T.pack $ show bs  -- MVP: simple hash, real impl would use SHA-256
