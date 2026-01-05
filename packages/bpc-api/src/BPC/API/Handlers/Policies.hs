{-# LANGUAGE OverloadedStrings #-}

-- | Policy Handlers
--
-- CRUD handlers for policy management.
--
-- @since 0.1.0.0
module BPC.API.Handlers.Policies
  ( -- * Policy Endpoints
    createPolicy
  , getPolicy
  , listPolicies
  , updatePolicy
    -- * Version Endpoints
  , createPolicyVersion
  , getPolicyVersion
  , listPolicyVersions
  , deactivatePolicyVersion
  ) where

import Control.Monad.Except (throwError)
import Data.Aeson (ToJSON(..), FromJSON(..), Value, object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

import BPC.API.App (AppM, withPool)
import BPC.API.Error (AppError(..))
import BPC.API.Types (AuthContext(..), Permission(..), CreatedResponse(..), CursorPage(..),
                       PaginationParams(..), Cursor(..), parseCursor, encodeCursor)
import BPC.API.Middleware.Auth (requirePermission)
import qualified BPC.DB.Repos.Policies as DB

-- | Policy response.
data PolicyResponse = PolicyResponse
  { prId :: UUID
  , prName :: Text
  , prDescription :: Maybe Text
  , prPriority :: Int
  , prIsActive :: Bool
  , prCreatedAt :: UTCTime
  , prUpdatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PolicyResponse where
  toJSON PolicyResponse{..} = object
    [ "id" .= prId
    , "name" .= prName
    , "description" .= prDescription
    , "priority" .= prPriority
    , "is_active" .= prIsActive
    , "created_at" .= prCreatedAt
    , "updated_at" .= prUpdatedAt
    ]

-- | Policy version response.
data PolicyVersionResponse = PolicyVersionResponse
  { pvrId :: UUID
  , pvrPolicyId :: UUID
  , pvrVersion :: Int
  , pvrPolicyHash :: Text
  , pvrEffect :: Text
  , pvrResources :: [Text]
  , pvrActions :: [Text]
  , pvrIsActive :: Bool
  , pvrCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PolicyVersionResponse where
  toJSON PolicyVersionResponse{..} = object
    [ "id" .= pvrId
    , "policy_id" .= pvrPolicyId
    , "version" .= pvrVersion
    , "policy_hash" .= pvrPolicyHash
    , "effect" .= pvrEffect
    , "resources" .= pvrResources
    , "actions" .= pvrActions
    , "is_active" .= pvrIsActive
    , "created_at" .= pvrCreatedAt
    ]

-- | Create policy request.
data CreatePolicyRequest = CreatePolicyRequest
  { cprName :: Text
  , cprDescription :: Maybe Text
  , cprPriority :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CreatePolicyRequest where
  parseJSON = Aeson.withObject "CreatePolicyRequest" $ \o -> CreatePolicyRequest
    <$> o .: "name"
    <*> o .:? "description"
    <*> o .: "priority"

-- | Create policy version request.
data CreatePolicyVersionRequest = CreatePolicyVersionRequest
  { cpvrRules :: Value
  , cpvrEffect :: Text
  , cpvrResources :: [Text]
  , cpvrActions :: [Text]
  , cpvrConditions :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CreatePolicyVersionRequest where
  parseJSON = Aeson.withObject "CreatePolicyVersionRequest" $ \o -> CreatePolicyVersionRequest
    <$> o .: "rules"
    <*> o .: "effect"
    <*> o .: "resources"
    <*> o .: "actions"
    <*> o .:? "conditions"

-- | Create a new policy.
createPolicy :: AuthContext -> CreatePolicyRequest -> AppM CreatedResponse
createPolicy ctx req = do
  requirePermission PermAdmin ctx
  policyId <- withPool $ \conn -> DB.createPolicy conn (acTenantId ctx) DB.PolicyInput
    { DB.piName = cprName req
    , DB.piDescription = cprDescription req
    , DB.piPriority = cprPriority req
    }
  pure CreatedResponse { crId = policyId, crLocation = "/v1/policies/" }

-- | Get a policy by ID.
getPolicy :: AuthContext -> UUID -> AppM PolicyResponse
getPolicy ctx policyId = do
  requirePermission PermAdmin ctx
  mPolicy <- withPool $ \conn -> DB.getPolicy conn (acTenantId ctx) policyId
  case mPolicy of
    Nothing -> throwError $ NotFound "Policy"
    Just p -> pure $ toResponse p

-- | List policies with pagination.
listPolicies :: AuthContext -> PaginationParams -> AppM (CursorPage PolicyResponse)
listPolicies ctx params = do
  requirePermission PermAdmin ctx

  -- Parse cursor if provided
  let cursorData = case ppCursor params of
        Nothing -> Nothing
        Just c -> case parseCursor c of
          Right cursor -> Just (cursorTimestamp cursor, cursorId cursor)
          Left _ -> Nothing

  -- Fetch one extra item to check if there are more
  let limit = ppLimit params + 1

  policies <- withPool $ \conn ->
    DB.listPolicies conn (acTenantId ctx) limit cursorData

  -- Check if there are more items
  let hasMore = length policies > ppLimit params
  let items = take (ppLimit params) policies

  -- Generate next cursor if there are more items
  let nextCursor = if hasMore && not (null items)
        then Just $ encodeCursor $ Cursor
               { cursorTimestamp = DB.policyCreatedAt (last items)
               , cursorId = DB.policyId (last items)
               }
        else Nothing

  pure CursorPage
    { cpItems = map toResponse items
    , cpNextCursor = nextCursor
    , cpHasMore = hasMore
    }

-- | Update a policy.
updatePolicy :: AuthContext -> UUID -> CreatePolicyRequest -> AppM ()
updatePolicy ctx policyId req = do
  requirePermission PermAdmin ctx
  success <- withPool $ \conn -> DB.updatePolicy conn (acTenantId ctx) policyId DB.PolicyInput
    { DB.piName = cprName req
    , DB.piDescription = cprDescription req
    , DB.piPriority = cprPriority req
    }
  if success then pure () else throwError $ NotFound "Policy"

-- | Create a policy version.
createPolicyVersion :: AuthContext -> UUID -> CreatePolicyVersionRequest -> AppM CreatedResponse
createPolicyVersion ctx policyId req = do
  requirePermission PermAdmin ctx
  let effect = case cpvrEffect req of
        "ALLOW" -> DB.Allow
        _ -> DB.Deny
  versionId <- withPool $ \conn -> DB.createPolicyVersion conn (acTenantId ctx) policyId DB.PolicyVersionInput
    { DB.pviRules = cpvrRules req
    , DB.pviEffect = effect
    , DB.pviResources = cpvrResources req
    , DB.pviActions = cpvrActions req
    , DB.pviConditions = cpvrConditions req
    }
  pure CreatedResponse { crId = versionId, crLocation = "/v1/policy-versions/" }

-- | Get a policy version.
getPolicyVersion :: AuthContext -> UUID -> AppM PolicyVersionResponse
getPolicyVersion ctx versionId = do
  requirePermission PermAdmin ctx
  mVersion <- withPool $ \conn -> DB.getPolicyVersion conn (acTenantId ctx) versionId
  case mVersion of
    Nothing -> throwError $ NotFound "Policy version"
    Just v -> pure $ toVersionResponse v

-- | List policy versions.
listPolicyVersions :: AuthContext -> UUID -> AppM [PolicyVersionResponse]
listPolicyVersions ctx policyId = do
  requirePermission PermAdmin ctx
  versions <- withPool $ \conn -> DB.getActivePolicyVersions conn (acTenantId ctx)
  pure $ map toVersionResponse $ filter (\v -> DB.pvPolicyId v == policyId) versions

-- | Deactivate a policy version.
deactivatePolicyVersion :: AuthContext -> UUID -> AppM ()
deactivatePolicyVersion ctx versionId = do
  requirePermission PermAdmin ctx
  success <- withPool $ \conn -> DB.deactivatePolicyVersion conn (acTenantId ctx) versionId
  if success then pure () else throwError $ NotFound "Policy version"

-- | Convert DB policy to response.
toResponse :: DB.Policy -> PolicyResponse
toResponse p = PolicyResponse
  { prId = DB.policyId p
  , prName = DB.policyName p
  , prDescription = DB.policyDescription p
  , prPriority = DB.policyPriority p
  , prIsActive = DB.policyIsActive p
  , prCreatedAt = DB.policyCreatedAt p
  , prUpdatedAt = DB.policyUpdatedAt p
  }

-- | Convert DB policy version to response.
toVersionResponse :: DB.PolicyVersion -> PolicyVersionResponse
toVersionResponse v = PolicyVersionResponse
  { pvrId = DB.pvId v
  , pvrPolicyId = DB.pvPolicyId v
  , pvrVersion = DB.pvVersion v
  , pvrPolicyHash = DB.pvPolicyHash v
  , pvrEffect = case DB.pvEffect v of
      DB.Allow -> "ALLOW"
      DB.Deny -> "DENY"
  , pvrResources = DB.pvResources v
  , pvrActions = DB.pvActions v
  , pvrIsActive = DB.pvIsActive v
  , pvrCreatedAt = DB.pvCreatedAt v
  }
