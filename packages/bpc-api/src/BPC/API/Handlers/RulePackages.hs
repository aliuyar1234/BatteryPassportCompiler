{-# LANGUAGE OverloadedStrings #-}

-- | Rule Package Handlers
module BPC.API.Handlers.RulePackages
  ( createRulePackage
  , listRulePackages
  , getRulePackage
  , createRuleVersion
  , publishRuleVersion
  , recordTestRun
  ) where

import Control.Monad.Except (throwError)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

import BPC.API.App (AppM, withPool)
import BPC.API.Error (AppError(..))
import BPC.API.Types (AuthContext(..), Permission(..), CursorPage(..), CreatedResponse(..),
                       PaginationParams(..), Cursor(..), parseCursor, encodeCursor)
import BPC.API.Middleware.Auth (requirePermission)
import qualified BPC.DB as DB

data RulePackageResponse = RulePackageResponse
  { rprId :: UUID, rprName :: Text, rprDescription :: Maybe Text, rprCreatedAt :: UTCTime }
  deriving stock (Show, Eq, Generic)

instance ToJSON RulePackageResponse where
  toJSON RulePackageResponse{..} = object
    [ "id" .= rprId, "name" .= rprName, "description" .= rprDescription, "created_at" .= rprCreatedAt ]

data RuleVersionResponse = RuleVersionResponse
  { rvrId :: UUID, rvrVersionNumber :: Int, rvrStatus :: Text, rvrDslHash :: Text, rvrCreatedAt :: UTCTime }
  deriving stock (Show, Eq, Generic)

instance ToJSON RuleVersionResponse where
  toJSON RuleVersionResponse{..} = object
    [ "id" .= rvrId, "version_number" .= rvrVersionNumber, "status" .= rvrStatus
    , "dsl_hash" .= rvrDslHash, "created_at" .= rvrCreatedAt
    ]

createRulePackage :: AuthContext -> Text -> Maybe Text -> AppM CreatedResponse
createRulePackage ctx name description = do
  requirePermission PermRuleWrite ctx
  result <- withPool $ \conn -> DB.createPackage conn (acTenantId ctx) $ DB.PackageInput name description
  case result of
    Left err -> throwError $ InternalError "Failed to create rule package"
    Right pkgId -> pure CreatedResponse { crId = pkgId, crLocation = "/v1/rule-packages/" }

listRulePackages :: AuthContext -> PaginationParams -> AppM (CursorPage RulePackageResponse)
listRulePackages ctx params = do
  requirePermission PermRuleRead ctx

  -- Parse cursor if provided
  let cursorData = case ppCursor params of
        Nothing -> Nothing
        Just c -> case parseCursor c of
          Right cursor -> Just (cursorTimestamp cursor, cursorId cursor)
          Left _ -> Nothing

  -- Fetch one extra item to check if there are more
  let limit = ppLimit params + 1

  packages <- withPool $ \conn ->
    DB.listPackages conn (acTenantId ctx) limit cursorData

  -- Check if there are more items
  let hasMore = length packages > ppLimit params
  let items = take (ppLimit params) packages

  -- Generate next cursor if there are more items
  let nextCursor = if hasMore && not (null items)
        then Just $ encodeCursor $ Cursor
               { cursorTimestamp = DB.rpCreatedAt (last items)
               , cursorId = DB.rpId (last items)
               }
        else Nothing

  pure CursorPage
    { cpItems = map toResponse items
    , cpNextCursor = nextCursor
    , cpHasMore = hasMore
    }
  where
    toResponse p = RulePackageResponse
      { rprId = DB.rpId p
      , rprName = DB.rpName p
      , rprDescription = DB.rpDescription p
      , rprCreatedAt = DB.rpCreatedAt p
      }

getRulePackage :: AuthContext -> UUID -> AppM RulePackageResponse
getRulePackage ctx pkgId = do
  requirePermission PermRuleRead ctx
  result <- withPool $ \conn -> DB.getPackage conn (acTenantId ctx) pkgId
  case result of
    Nothing -> throwError $ RulePackageNotFound pkgId
    Just p -> pure $ RulePackageResponse (DB.rpId p) (DB.rpName p) (DB.rpDescription p) (DB.rpCreatedAt p)

createRuleVersion :: AuthContext -> UUID -> Text -> Maybe Text -> AppM CreatedResponse
createRuleVersion ctx pkgId dslSource testsSource = do
  requirePermission PermRuleWrite ctx
  result <- withPool $ \conn -> DB.createVersion conn (acTenantId ctx) pkgId $ DB.VersionInput dslSource testsSource
  case result of
    Left err -> throwError $ InternalError "Failed to create rule version"
    Right verId -> pure CreatedResponse { crId = verId, crLocation = "/v1/rule-versions/" }

publishRuleVersion :: AuthContext -> UUID -> AppM RuleVersionResponse
publishRuleVersion ctx versionId = do
  requirePermission PermRulePublish ctx
  result <- withPool $ \conn -> DB.publishVersion conn (acTenantId ctx) versionId
  case result of
    Left err -> throwError $ InternalError $ T.pack $ show err
    Right () -> do
      verResult <- withPool $ \conn -> DB.getRuleVersion conn (acTenantId ctx) versionId
      case verResult of
        Nothing -> throwError $ NotFound "Rule version"
        Just v -> pure $ RuleVersionResponse (DB.rvId v) (DB.rvVersionNumber v) "PUBLISHED" (DB.rvDslHash v) (DB.rvCreatedAt v)

recordTestRun :: AuthContext -> UUID -> Int -> Int -> Int -> AppM UUID
recordTestRun ctx versionId total passed failed = do
  requirePermission PermRuleWrite ctx
  result <- withPool $ \conn -> DB.recordTestRun conn (acTenantId ctx) versionId $ DB.TestRunInput total passed failed
  case result of
    Left err -> throwError $ InternalError "Failed to record test run"
    Right runId -> pure runId
