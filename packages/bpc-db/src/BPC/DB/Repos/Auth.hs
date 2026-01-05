{-# LANGUAGE OverloadedStrings #-}

-- | Auth Repository
--
-- Tenant, actor, API key, role, and permission management.
-- API keys use SHA-256(key + pepper) for secure storage.
--
-- @since 0.1.0.0
module BPC.DB.Repos.Auth
  ( -- * Types
    ActorId
  , RoleId
  , ApiKeyId
  , Tenant(..)
  , Actor(..)
  , Role(..)
  , Permission(..)
  , ApiKey(..)
  , TenantInput(..)
  , ActorInput(..)
  , ApiKeyInput(..)
    -- * Tenant Operations
  , getTenant
  , getTenantBySlug
  , createTenant
  , listTenants
    -- * Actor Operations
  , createActor
  , getActor
  , getActorByEmail
  , listActors
    -- * API Key Operations
  , createApiKey
  , verifyApiKey
  , revokeApiKey
  , getApiKeysByActor
    -- * Role Operations
  , createRole
  , getRole
  , assignRole
  , removeRole
  , getActorRoles
    -- * Permission Operations
  , getActorPermissions
  , hasPermission
  ) where

import Control.Exception (catch)
import Crypto.Hash (SHA256(..), hashWith)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection, Only(..), SqlError)
import qualified Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)

import BPC.DB.Error (AuthError(..))
import BPC.DB.Repos.Events (TenantId)

-- | Actor ID (UUID)
type ActorId = UUID

-- | Role ID (UUID)
type RoleId = UUID

-- | API Key ID (UUID)
type ApiKeyId = UUID

-- | Tenant record.
data Tenant = Tenant
  { tenId :: TenantId
  , tenSlug :: Text
  , tenName :: Text
  , tenIsActive :: Bool
  , tenCreatedAt :: UTCTime
  , tenUpdatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Actor record.
data Actor = Actor
  { actId :: ActorId
  , actTenantId :: TenantId
  , actEmail :: Text
  , actDisplayName :: Text
  , actIsActive :: Bool
  , actCreatedAt :: UTCTime
  , actUpdatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Role record.
data Role = Role
  { roleId :: RoleId
  , roleTenantId :: TenantId
  , roleName :: Text
  , roleDescription :: Maybe Text
  , roleCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Permission record.
data Permission = Permission
  { permResource :: Text
  , permAction :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | API Key record.
data ApiKey = ApiKey
  { akId :: ApiKeyId
  , akTenantId :: TenantId
  , akActorId :: ActorId
  , akPrefix :: Text
  , akName :: Text
  , akExpiresAt :: Maybe UTCTime
  , akRevokedAt :: Maybe UTCTime
  , akLastUsedAt :: Maybe UTCTime
  , akCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Input for creating a tenant.
data TenantInput = TenantInput
  { tiSlug :: Text
  , tiName :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Input for creating an actor.
data ActorInput = ActorInput
  { aiEmail :: Text
  , aiDisplayName :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Input for creating an API key.
data ApiKeyInput = ApiKeyInput
  { akiName :: Text
  , akiExpiresAt :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | API key pepper for hashing (should be in config in production).
apiKeyPepper :: Text
apiKeyPepper = "bpc-api-key-pepper-v1"

-- | Calculate API key hash: SHA-256(key + pepper)
hashApiKey :: Text -> Text
hashApiKey key =
  let bytes = TE.encodeUtf8 $ key <> apiKeyPepper
      digest = hashWith SHA256 bytes
      hexBytes = convert digest :: BS.ByteString
  in TE.decodeUtf8 $ BS.concatMap toHex hexBytes
  where
    toHex :: Word8 -> BS.ByteString
    toHex w =
      let (hi, lo) = w `divMod` 16
      in BS.pack [hexChar hi, hexChar lo]

    hexChar :: Word8 -> Word8
    hexChar n
      | n < 10    = 48 + n
      | otherwise = 87 + n

-- | Get a tenant by ID.
getTenant
  :: Connection
  -> TenantId
  -> IO (Maybe Tenant)
getTenant conn tenantId = do
  rows <- PG.query conn
    "SELECT id, slug, name, is_active, created_at, updated_at \
    \FROM tenants WHERE id = ?"
    (Only tenantId)
  pure $ case rows of
    [] -> Nothing
    (row : _) -> Just $ rowToTenant row

-- | Get a tenant by slug.
getTenantBySlug
  :: Connection
  -> Text
  -> IO (Maybe Tenant)
getTenantBySlug conn slug = do
  rows <- PG.query conn
    "SELECT id, slug, name, is_active, created_at, updated_at \
    \FROM tenants WHERE slug = ?"
    (Only slug)
  pure $ case rows of
    [] -> Nothing
    (row : _) -> Just $ rowToTenant row

-- | Create a new tenant.
createTenant
  :: Connection
  -> TenantInput
  -> IO (Either AuthError TenantId)
createTenant conn input = do
  result <- (Right <$> PG.query conn
    "INSERT INTO tenants (slug, name) VALUES (?, ?) RETURNING id"
    (tiSlug input, tiName input))
    `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ AUTH_TENANT_NOT_FOUND (error "No ID returned")
    Right (Only tid : _) -> pure $ Right tid
  where
    handleSqlError :: SqlError -> IO (Either AuthError [Only UUID])
    handleSqlError _ = pure $ Left $ AUTH_PERMISSION_DENIED "Tenant creation failed"

-- | List all tenants.
listTenants
  :: Connection
  -> IO [Tenant]
listTenants conn = do
  rows <- PG.query_ conn
    "SELECT id, slug, name, is_active, created_at, updated_at \
    \FROM tenants ORDER BY name ASC"
  pure $ map rowToTenant rows

-- | Create a new actor.
createActor
  :: Connection
  -> TenantId
  -> ActorInput
  -> IO (Either AuthError ActorId)
createActor conn tenantId input = do
  result <- (Right <$> PG.query conn
    "INSERT INTO actors (tenant_id, email, display_name) \
    \VALUES (?, ?, ?) RETURNING id"
    (tenantId, aiEmail input, aiDisplayName input))
    `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ AUTH_ACTOR_NOT_FOUND (error "No ID returned")
    Right (Only aid : _) -> pure $ Right aid
  where
    handleSqlError :: SqlError -> IO (Either AuthError [Only UUID])
    handleSqlError _ = pure $ Left $ AUTH_PERMISSION_DENIED "Actor creation failed"

-- | Get an actor by ID.
getActor
  :: Connection
  -> TenantId
  -> ActorId
  -> IO (Maybe Actor)
getActor conn tenantId actorId = do
  rows <- PG.query conn
    "SELECT id, tenant_id, email, display_name, is_active, created_at, updated_at \
    \FROM actors WHERE tenant_id = ? AND id = ?"
    (tenantId, actorId)
  pure $ case rows of
    [] -> Nothing
    (row : _) -> Just $ rowToActor row

-- | Get an actor by email.
getActorByEmail
  :: Connection
  -> TenantId
  -> Text
  -> IO (Maybe Actor)
getActorByEmail conn tenantId email = do
  rows <- PG.query conn
    "SELECT id, tenant_id, email, display_name, is_active, created_at, updated_at \
    \FROM actors WHERE tenant_id = ? AND email = ?"
    (tenantId, email)
  pure $ case rows of
    [] -> Nothing
    (row : _) -> Just $ rowToActor row

-- | List all actors for a tenant.
listActors
  :: Connection
  -> TenantId
  -> IO [Actor]
listActors conn tenantId = do
  rows <- PG.query conn
    "SELECT id, tenant_id, email, display_name, is_active, created_at, updated_at \
    \FROM actors WHERE tenant_id = ? ORDER BY email ASC"
    (Only tenantId)
  pure $ map rowToActor rows

-- | Create an API key.
--
-- Returns (ApiKeyId, plaintext key). The plaintext key should be shown
-- to the user once and never stored.
--
-- @since 0.1.0.0
createApiKey
  :: Connection
  -> TenantId
  -> ActorId
  -> ApiKeyInput
  -> Text
  -> IO (Either AuthError (ApiKeyId, Text))
createApiKey conn tenantId actorId input plaintextKey = do
  let prefix = T.take 8 plaintextKey
  let keyHash = hashApiKey plaintextKey

  result <- (Right <$> PG.query conn
    "INSERT INTO api_keys (tenant_id, actor_id, prefix, key_hash, name, expires_at) \
    \VALUES (?, ?, ?, ?, ?, ?) RETURNING id"
    (tenantId, actorId, prefix, keyHash, akiName input, akiExpiresAt input))
    `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left AUTH_API_KEY_INVALID
    Right (Only akid : _) -> pure $ Right (akid, plaintextKey)
  where
    handleSqlError :: SqlError -> IO (Either AuthError [Only UUID])
    handleSqlError _ = pure $ Left AUTH_API_KEY_INVALID

-- | Verify an API key.
--
-- Looks up by prefix, then verifies hash. Checks:
-- 1. Key exists and hash matches
-- 2. Key is not revoked
-- 3. Key is not expired (expires_at is NULL or in the future)
--
-- Returns (TenantId, ActorId) if valid.
--
-- @since 0.1.0.0
verifyApiKey
  :: Connection
  -> Text
  -> IO (Either AuthError (TenantId, ActorId))
verifyApiKey conn plaintextKey = do
  let prefix = T.take 8 plaintextKey
  let keyHash = hashApiKey plaintextKey

  -- Get current time for expiry check
  now <- getCurrentTime

  rows <- PG.query conn
    "SELECT tenant_id, actor_id, revoked_at, expires_at \
    \FROM api_keys WHERE prefix = ? AND key_hash = ?"
    (prefix, keyHash)

  case rows of
    [] -> pure $ Left AUTH_API_KEY_INVALID
    ((tenantId, actorId, revokedAt, expiresAt) : _) ->
      case revokedAt of
        Just _ -> pure $ Left AUTH_API_KEY_REVOKED
        Nothing ->
          -- Check expiry: NULL means never expires
          case expiresAt of
            Just expiry
              | expiry < now -> pure $ Left AUTH_API_KEY_EXPIRED
            _ -> do
              -- Update last_used_at timestamp
              _ <- PG.execute conn
                "UPDATE api_keys SET last_used_at = ? WHERE prefix = ? AND key_hash = ?"
                (now, prefix, keyHash)
              pure $ Right (tenantId, actorId)

-- | Revoke an API key.
revokeApiKey
  :: Connection
  -> TenantId
  -> ApiKeyId
  -> IO Bool
revokeApiKey conn tenantId keyId = do
  n <- PG.execute conn
    "UPDATE api_keys SET revoked_at = NOW() WHERE tenant_id = ? AND id = ?"
    (tenantId, keyId)
  pure $ n > 0

-- | Get all API keys for an actor.
getApiKeysByActor
  :: Connection
  -> TenantId
  -> ActorId
  -> IO [ApiKey]
getApiKeysByActor conn tenantId actorId = do
  rows <- PG.query conn
    "SELECT id, tenant_id, actor_id, prefix, name, expires_at, \
    \       revoked_at, last_used_at, created_at \
    \FROM api_keys WHERE tenant_id = ? AND actor_id = ? \
    \ORDER BY created_at DESC"
    (tenantId, actorId)
  pure $ map rowToApiKey rows

-- | Create a role.
createRole
  :: Connection
  -> TenantId
  -> Text
  -> Maybe Text
  -> IO (Either AuthError RoleId)
createRole conn tenantId name description = do
  result <- (Right <$> PG.query conn
    "INSERT INTO roles (tenant_id, name, description) \
    \VALUES (?, ?, ?) RETURNING id"
    (tenantId, name, description))
    `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ AUTH_ROLE_NOT_FOUND (error "No ID returned")
    Right (Only rid : _) -> pure $ Right rid
  where
    handleSqlError :: SqlError -> IO (Either AuthError [Only UUID])
    handleSqlError _ = pure $ Left $ AUTH_PERMISSION_DENIED "Role creation failed"

-- | Get a role by ID.
getRole
  :: Connection
  -> TenantId
  -> RoleId
  -> IO (Maybe Role)
getRole conn tenantId roleId = do
  rows <- PG.query conn
    "SELECT id, tenant_id, name, description, created_at \
    \FROM roles WHERE tenant_id = ? AND id = ?"
    (tenantId, roleId)
  pure $ case rows of
    [] -> Nothing
    ((rid, tid, name, desc, cat) : _) ->
      Just Role
        { roleId = rid
        , roleTenantId = tid
        , roleName = name
        , roleDescription = desc
        , roleCreatedAt = cat
        }

-- | Assign a role to an actor.
assignRole
  :: Connection
  -> TenantId
  -> ActorId
  -> RoleId
  -> IO Bool
assignRole conn tenantId actorId roleId = do
  n <- PG.execute conn
    "INSERT INTO actor_roles (tenant_id, actor_id, role_id) \
    \VALUES (?, ?, ?) ON CONFLICT DO NOTHING"
    (tenantId, actorId, roleId)
  pure $ n > 0

-- | Remove a role from an actor.
removeRole
  :: Connection
  -> TenantId
  -> ActorId
  -> RoleId
  -> IO Bool
removeRole conn tenantId actorId roleId = do
  n <- PG.execute conn
    "DELETE FROM actor_roles \
    \WHERE tenant_id = ? AND actor_id = ? AND role_id = ?"
    (tenantId, actorId, roleId)
  pure $ n > 0

-- | Get all roles for an actor.
getActorRoles
  :: Connection
  -> TenantId
  -> ActorId
  -> IO [Role]
getActorRoles conn tenantId actorId = do
  rows <- PG.query conn
    "SELECT r.id, r.tenant_id, r.name, r.description, r.created_at \
    \FROM roles r \
    \INNER JOIN actor_roles ar ON ar.role_id = r.id \
    \WHERE ar.tenant_id = ? AND ar.actor_id = ?"
    (tenantId, actorId)
  pure $ map rowToRole rows
  where
    rowToRole (rid, tid, name, desc, cat) = Role
      { roleId = rid
      , roleTenantId = tid
      , roleName = name
      , roleDescription = desc
      , roleCreatedAt = cat
      }

-- | Get all permissions for an actor (via roles).
getActorPermissions
  :: Connection
  -> TenantId
  -> ActorId
  -> IO [Permission]
getActorPermissions conn tenantId actorId = do
  rows <- PG.query conn
    "SELECT DISTINCT rp.resource, rp.action \
    \FROM role_permissions rp \
    \INNER JOIN actor_roles ar ON ar.role_id = rp.role_id \
    \WHERE ar.tenant_id = ? AND ar.actor_id = ?"
    (tenantId, actorId)
  pure $ map (\(res, act) -> Permission res act) rows

-- | Check if an actor has a specific permission.
hasPermission
  :: Connection
  -> TenantId
  -> ActorId
  -> Text
  -> Text
  -> IO Bool
hasPermission conn tenantId actorId resource action = do
  [Only count] <- PG.query conn
    "SELECT COUNT(*) FROM role_permissions rp \
    \INNER JOIN actor_roles ar ON ar.role_id = rp.role_id \
    \WHERE ar.tenant_id = ? AND ar.actor_id = ? \
    \  AND rp.resource = ? AND rp.action = ?"
    (tenantId, actorId, resource, action)
  pure $ (count :: Int) > 0

-- | Convert database row to Tenant.
rowToTenant :: (UUID, Text, Text, Bool, UTCTime, UTCTime) -> Tenant
rowToTenant (tid, slug, name, active, cat, uat) = Tenant
  { tenId = tid
  , tenSlug = slug
  , tenName = name
  , tenIsActive = active
  , tenCreatedAt = cat
  , tenUpdatedAt = uat
  }

-- | Convert database row to Actor.
rowToActor :: (UUID, UUID, Text, Text, Bool, UTCTime, UTCTime) -> Actor
rowToActor (aid, tid, email, displayName, active, cat, uat) = Actor
  { actId = aid
  , actTenantId = tid
  , actEmail = email
  , actDisplayName = displayName
  , actIsActive = active
  , actCreatedAt = cat
  , actUpdatedAt = uat
  }

-- | Convert database row to ApiKey.
rowToApiKey
  :: (UUID, UUID, UUID, Text, Text, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime, UTCTime)
  -> ApiKey
rowToApiKey (akid, tid, aid, prefix, name, expiresAt, revokedAt, lastUsedAt, cat) = ApiKey
  { akId = akid
  , akTenantId = tid
  , akActorId = aid
  , akPrefix = prefix
  , akName = name
  , akExpiresAt = expiresAt
  , akRevokedAt = revokedAt
  , akLastUsedAt = lastUsedAt
  , akCreatedAt = cat
  }
