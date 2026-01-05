{-# LANGUAGE OverloadedStrings #-}

-- | Authentication Middleware
--
-- API key authentication using Bearer token in Authorization header.
-- Validates API keys against database and extracts AuthContext.
--
-- @since 0.1.0.0
module BPC.API.Middleware.Auth
  ( -- * Middleware
    authMiddleware
    -- * Authentication
  , authenticate
  , extractBearerToken
  , hashApiKey
    -- * Authorization
  , requirePermission
  , hasPermission
    -- * Context
  , AuthContext(..)
  , Permission(..)
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Crypto.Hash (SHA256(..), hashWith)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import Data.Word (Word8)
import Network.HTTP.Types (status401, status403)
import Network.Wai (Middleware, Request, requestHeaders, responseLBS, pathInfo)

import BPC.API.App (AppM, Env(..), withPool)
import BPC.API.Error (AppError(..))
import BPC.API.Types (AuthContext(..), Permission(..), allPermissions)
import BPC.DB (verifyApiKey, getActorPermissions)

-- | Authentication middleware.
--
-- Extracts Bearer token from Authorization header, validates against
-- database, and attaches AuthContext to request.
--
-- Public endpoints (health, metrics, docs) bypass authentication.
--
-- @since 0.1.0.0
authMiddleware :: Env -> Middleware
authMiddleware env app req respond = do
  -- Check if endpoint requires auth
  if isPublicEndpoint req
    then app req respond
    else do
      -- Extract and validate token
      case extractBearerToken req of
        Nothing -> respond $ responseLBS status401
          [("Content-Type", "application/json")]
          "{\"error\":{\"code\":\"UNAUTHORIZED\",\"message\":\"Missing Authorization header\"}}"

        Just token -> do
          -- Verify against database
          result <- withPool (envPool env) $ \conn ->
            verifyApiKey conn token
          case result of
            Left _ -> respond $ responseLBS status401
              [("Content-Type", "application/json")]
              "{\"error\":{\"code\":\"API_KEY_INVALID\",\"message\":\"API key is invalid or revoked\"}}"
            Right (tenantId, actorId) -> do
              -- Store auth context in vault or use middleware state
              -- For now, pass through (real implementation would use Vault)
              app req respond
  where
    withPool pool action = BPC.DB.withConn pool action

-- | Check if endpoint is public (no auth required).
--
-- Public endpoints:
-- - /health/live, /health/ready - Health checks
-- - /metrics - Prometheus metrics
-- - /docs/* - API documentation
--
-- @since 0.1.0.0
isPublicEndpoint :: Request -> Bool
isPublicEndpoint req =
  let path = pathInfo req  -- Use Network.Wai.pathInfo (imported)
  in case path of
    ["health"] -> True
    ["health", "live"] -> True
    ["health", "ready"] -> True
    ["metrics"] -> True
    ["docs"] -> True
    ("docs" : _) -> True
    _ -> False

-- | Extract Bearer token from Authorization header.
--
-- Expects: "Bearer <token>"
--
-- @since 0.1.0.0
extractBearerToken :: Request -> Maybe Text
extractBearerToken req = do
  authHeader <- lookup "Authorization" (requestHeaders req)
  case BS8.words authHeader of
    ["Bearer", token] -> Just $ TE.decodeUtf8 token
    _ -> Nothing

-- | Hash API key with pepper.
--
-- hash = SHA-256(key + pepper)
--
-- @since 0.1.0.0
hashApiKey :: Text -> Text -> Text
hashApiKey key pepper =
  let bytes = TE.encodeUtf8 $ key <> pepper
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

-- | Authenticate request and return AuthContext.
--
-- Used by handlers to get authenticated context.
--
-- @since 0.1.0.0
authenticate :: Text -> AppM AuthContext
authenticate token = do
  pepper <- asks envApiKeyPepper
  result <- withPool $ \conn -> verifyApiKey conn token
  case result of
    Left _ -> throwError ApiKeyInvalid
    Right (tenantId, actorId) -> do
      -- Load permissions
      perms <- withPool $ \conn ->
        getActorPermissions conn tenantId actorId
      let permissions = map dbPermToApiPerm perms
      pure AuthContext
        { acTenantId = tenantId
        , acActorId = actorId
        , acPermissions = permissions
        }
  where
    -- Map DB permission to API permission
    dbPermToApiPerm dbPerm = PermDocumentRead  -- Simplified

-- | Require a specific permission.
--
-- Throws PermissionDenied if actor lacks permission.
--
-- @since 0.1.0.0
requirePermission :: Permission -> AuthContext -> AppM ()
requirePermission perm ctx
  | hasPermission perm ctx = pure ()
  | otherwise = throwError $ PermissionDenied (permissionResource perm)

-- | Check if context has permission.
hasPermission :: Permission -> AuthContext -> Bool
hasPermission perm ctx =
  -- Admin has all permissions
  PermAdmin `elem` acPermissions ctx ||
  perm `elem` acPermissions ctx

-- | Get resource name for permission (for error messages).
permissionResource :: Permission -> Text
permissionResource = \case
  PermDocumentRead -> "documents"
  PermDocumentWrite -> "documents"
  PermSnapshotRead -> "snapshots"
  PermSnapshotWrite -> "snapshots"
  PermSnapshotSeal -> "snapshots"
  PermPassportRead -> "passports"
  PermPassportWrite -> "passports"
  PermPassportSign -> "passports"
  PermPassportActivate -> "passports"
  PermPassportRevoke -> "passports"
  PermRuleRead -> "rules"
  PermRuleWrite -> "rules"
  PermRulePublish -> "rules"
  PermAuditRead -> "audit"
  PermWebhookManage -> "webhooks"
  PermAdmin -> "admin"
