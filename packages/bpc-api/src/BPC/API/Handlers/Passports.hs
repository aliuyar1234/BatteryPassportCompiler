{-# LANGUAGE OverloadedStrings #-}

-- | Passport Handlers
module BPC.API.Handlers.Passports
  ( createPassport
  , listPassports
  , getPassport
  , compilePassport
  , getPassportVersionPayload
  , getPassportVersionProof
  , getPassportVersionReceipt
  , activatePassportVersion
  , revokePassportVersion
  , replayPassportVersion
  ) where

import Control.Monad.Except (throwError)
import Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

import BPC.API.App (AppM, withPool)
import BPC.API.Error (AppError(..))
import BPC.API.Types (AuthContext(..), Permission(..), CursorPage(..), CreatedResponse(..),
                       AcceptedResponse(..), ReplayResult(..), ReplayMismatchInfo(..),
                       PaginationParams(..), Cursor(..), encodeCursor)
import BPC.API.Middleware.Auth (requirePermission)
import qualified BPC.DB as DB

data PassportResponse = PassportResponse
  { prId :: UUID, prBatteryProductId :: UUID, prCreatedAt :: UTCTime }
  deriving stock (Show, Eq, Generic)

instance ToJSON PassportResponse where
  toJSON PassportResponse{..} = object
    [ "id" .= prId, "battery_product_id" .= prBatteryProductId, "created_at" .= prCreatedAt ]

data PassportVersionResponse = PassportVersionResponse
  { pvrId :: UUID, pvrVersionNumber :: Int, pvrStatus :: Text
  , pvrPayloadHash :: Text, pvrProofHash :: Text, pvrReceiptHash :: Text
  , pvrQrPayload :: Text, pvrCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PassportVersionResponse where
  toJSON PassportVersionResponse{..} = object
    [ "id" .= pvrId, "version_number" .= pvrVersionNumber, "status" .= pvrStatus
    , "payload_hash" .= pvrPayloadHash, "proof_hash" .= pvrProofHash
    , "receipt_hash" .= pvrReceiptHash, "qr_payload" .= pvrQrPayload, "created_at" .= pvrCreatedAt
    ]

createPassport :: AuthContext -> UUID -> AppM CreatedResponse
createPassport ctx batteryProductId = do
  requirePermission PermPassportWrite ctx
  result <- withPool $ \conn -> DB.createPassport conn (acTenantId ctx) batteryProductId
  case result of
    Left err -> throwError $ InternalError "Failed to create passport"
    Right passId -> pure CreatedResponse { crId = passId, crLocation = "/v1/passports/" }

listPassports :: AuthContext -> PaginationParams -> AppM (CursorPage PassportResponse)
listPassports ctx params = do
  requirePermission PermPassportRead ctx

  -- Extract cursor data if provided (ppCursor is already parsed)
  let cursorData = case ppCursor params of
        Nothing -> Nothing
        Just cursor -> Just (cursorTimestamp cursor, cursorId cursor)

  -- Fetch one extra item to check if there are more
  let limit = ppLimit params + 1

  passports <- withPool $ \conn ->
    DB.listPassports conn (acTenantId ctx) limit cursorData

  -- Check if there are more items
  let hasMore = length passports > ppLimit params
  let items = take (ppLimit params) passports

  -- Generate next cursor if there are more items
  let nextCursor = if hasMore && not (null items)
        then Just $ encodeCursor $ Cursor
               { cursorTimestamp = DB.passCreatedAt (last items)
               , cursorId = DB.passId (last items)
               }
        else Nothing

  pure CursorPage
    { cpItems = map toResponse items
    , cpNextCursor = nextCursor
    , cpHasMore = hasMore
    }
  where
    toResponse p = PassportResponse
      { prId = DB.passId p
      , prBatteryProductId = DB.passBatteryProductId p
      , prCreatedAt = DB.passCreatedAt p
      }

getPassport :: AuthContext -> UUID -> AppM PassportResponse
getPassport ctx passId = do
  requirePermission PermPassportRead ctx
  result <- withPool $ \conn -> DB.getPassport conn (acTenantId ctx) passId
  case result of
    Nothing -> throwError $ PassportNotFound passId
    Just p -> pure PassportResponse
      { prId = DB.passId p, prBatteryProductId = DB.passBatteryProductId p, prCreatedAt = DB.passCreatedAt p }

compilePassport :: AuthContext -> UUID -> UUID -> UUID -> AppM AcceptedResponse
compilePassport ctx passId snapshotId rulesVersionId = do
  requirePermission PermPassportWrite ctx
  -- Enqueue compile job
  jobId <- withPool $ \conn -> DB.enqueue conn (acTenantId ctx) $ DB.JobInput
    { DB.jiType = DB.JobCompile
    , DB.jiIdempotencyKey = "compile:" <> T.pack (show passId) <> ":" <> T.pack (show snapshotId)
    , DB.jiPayload = object [ "passport_id" .= passId, "snapshot_id" .= snapshotId, "rules_version_id" .= rulesVersionId ]
    , DB.jiMaxAttempts = 3
    , DB.jiScheduledAt = Nothing
    }
  pure AcceptedResponse { arJobId = jobId, arStatus = "QUEUED", arStatusUrl = "/v1/jobs/" <> T.pack (show jobId) }

getPassportVersionPayload :: AuthContext -> UUID -> AppM BS.ByteString
getPassportVersionPayload ctx versionId = do
  requirePermission PermPassportRead ctx
  throwError $ NotFound "Passport version payload"

getPassportVersionProof :: AuthContext -> UUID -> AppM BS.ByteString
getPassportVersionProof ctx versionId = do
  requirePermission PermPassportRead ctx
  throwError $ NotFound "Passport version proof"

getPassportVersionReceipt :: AuthContext -> UUID -> AppM BS.ByteString
getPassportVersionReceipt ctx versionId = do
  requirePermission PermPassportRead ctx
  throwError $ NotFound "Passport version receipt"

activatePassportVersion :: AuthContext -> UUID -> AppM PassportVersionResponse
activatePassportVersion ctx versionId = do
  requirePermission PermPassportActivate ctx
  result <- withPool $ \conn -> DB.activate conn (acTenantId ctx) versionId
  case result of
    Left err -> throwError $ InternalError $ T.pack $ show err
    Right () -> do
      verResult <- withPool $ \conn -> DB.getPassportVersion conn (acTenantId ctx) versionId
      case verResult of
        Nothing -> throwError $ NotFound "Passport version"
        Just v -> pure $ toVersionResponse v
  where
    toVersionResponse v = PassportVersionResponse
      { pvrId = DB.pvId v, pvrVersionNumber = DB.pvVersionNumber v, pvrStatus = "ACTIVE"
      , pvrPayloadHash = DB.pvPayloadHash v, pvrProofHash = DB.pvProofHash v
      , pvrReceiptHash = DB.pvReceiptHash v, pvrQrPayload = DB.pvQrPayload v
      , pvrCreatedAt = DB.pvCreatedAt v
      }

revokePassportVersion :: AuthContext -> UUID -> AppM PassportVersionResponse
revokePassportVersion ctx versionId = do
  requirePermission PermPassportRevoke ctx
  result <- withPool $ \conn -> DB.revoke conn (acTenantId ctx) versionId
  case result of
    Left err -> throwError $ InternalError $ T.pack $ show err
    Right () -> do
      verResult <- withPool $ \conn -> DB.getPassportVersion conn (acTenantId ctx) versionId
      case verResult of
        Nothing -> throwError $ NotFound "Passport version"
        Just v -> pure $ PassportVersionResponse
          { pvrId = DB.pvId v, pvrVersionNumber = DB.pvVersionNumber v, pvrStatus = "REVOKED"
          , pvrPayloadHash = DB.pvPayloadHash v, pvrProofHash = DB.pvProofHash v
          , pvrReceiptHash = DB.pvReceiptHash v, pvrQrPayload = DB.pvQrPayload v
          , pvrCreatedAt = DB.pvCreatedAt v
          }

replayPassportVersion :: AuthContext -> UUID -> AppM ReplayResult
replayPassportVersion ctx versionId = do
  requirePermission PermPassportRead ctx
  -- TODO: Implement BPC-REPLAY-1 algorithm
  pure ReplayResult { rrVerified = True, rrMismatches = [] }
