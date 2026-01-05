{-# LANGUAGE OverloadedStrings #-}

-- | Snapshot Handlers
module BPC.API.Handlers.Snapshots
  ( createSnapshot
  , listSnapshots
  , getSnapshot
  , addSnapshotItem
  , removeSnapshotItem
  , sealSnapshot
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

data SnapshotResponse = SnapshotResponse
  { srId :: UUID, srStatus :: Text, srHash :: Maybe Text
  , srDescription :: Maybe Text, srCreatedAt :: UTCTime, srSealedAt :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SnapshotResponse where
  toJSON SnapshotResponse{..} = object
    [ "id" .= srId, "status" .= srStatus, "hash" .= srHash
    , "description" .= srDescription, "created_at" .= srCreatedAt, "sealed_at" .= srSealedAt
    ]

createSnapshot :: AuthContext -> Maybe Text -> AppM CreatedResponse
createSnapshot ctx description = do
  requirePermission PermSnapshotWrite ctx
  snapId <- withPool $ \conn -> DB.createSnapshot conn (acTenantId ctx) description
  pure CreatedResponse { crId = snapId, crLocation = "/v1/snapshots/" <> T.pack (show snapId) }

listSnapshots :: AuthContext -> PaginationParams -> AppM (CursorPage SnapshotResponse)
listSnapshots ctx params = do
  requirePermission PermSnapshotRead ctx

  -- Extract cursor data if provided (ppCursor is already parsed)
  let cursorData = case ppCursor params of
        Nothing -> Nothing
        Just cursor -> Just (cursorTimestamp cursor, cursorId cursor)

  -- Fetch one extra item to check if there are more
  let limit = ppLimit params + 1

  snapshots <- withPool $ \conn ->
    DB.listSnapshots conn (acTenantId ctx) limit cursorData

  -- Check if there are more items
  let hasMore = length snapshots > ppLimit params
  let items = take (ppLimit params) snapshots

  -- Generate next cursor if there are more items
  let nextCursor = if hasMore && not (null items)
        then Just $ encodeCursor $ Cursor
               { cursorTimestamp = DB.snapCreatedAt (last items)
               , cursorId = DB.snapId (last items)
               }
        else Nothing

  pure CursorPage
    { cpItems = map toResponse items
    , cpNextCursor = nextCursor
    , cpHasMore = hasMore
    }
  where
    toResponse s = SnapshotResponse
      { srId = DB.snapId s
      , srStatus = statusText $ DB.snapStatus s
      , srHash = DB.snapHash s
      , srDescription = DB.snapDescription s
      , srCreatedAt = DB.snapCreatedAt s
      , srSealedAt = DB.snapSealedAt s
      }
    statusText DB.SnapBuilding = "BUILDING"
    statusText DB.SnapReady = "READY"
    statusText DB.SnapSealed = "SEALED"

getSnapshot :: AuthContext -> UUID -> AppM SnapshotResponse
getSnapshot ctx snapId = do
  requirePermission PermSnapshotRead ctx
  result <- withPool $ \conn -> DB.getSnapshot conn (acTenantId ctx) snapId
  case result of
    Nothing -> throwError $ SnapshotNotFound snapId
    Just s -> pure $ toResponse s
  where
    toResponse s = SnapshotResponse (DB.snapId s) (statusText $ DB.snapStatus s)
                     (DB.snapHash s) (DB.snapDescription s) (DB.snapCreatedAt s) (DB.snapSealedAt s)
    statusText DB.SnapBuilding = "BUILDING"
    statusText DB.SnapReady = "READY"
    statusText DB.SnapSealed = "SEALED"

addSnapshotItem :: AuthContext -> UUID -> UUID -> AppM ()
addSnapshotItem ctx snapId factId = do
  requirePermission PermSnapshotWrite ctx
  result <- withPool $ \conn -> DB.addItem conn (acTenantId ctx) snapId factId
  case result of
    Left (DB.SNAPSHOT_SEALED _) -> throwError $ SnapshotSealed snapId
    Left _ -> throwError $ InternalError "Failed to add item"
    Right () -> pure ()

removeSnapshotItem :: AuthContext -> UUID -> UUID -> AppM ()
removeSnapshotItem ctx snapId factId = do
  requirePermission PermSnapshotWrite ctx
  result <- withPool $ \conn -> DB.removeItem conn (acTenantId ctx) snapId factId
  case result of
    Left (DB.SNAPSHOT_SEALED _) -> throwError $ SnapshotSealed snapId
    Left _ -> throwError $ InternalError "Failed to remove item"
    Right () -> pure ()

sealSnapshot :: AuthContext -> UUID -> AppM SnapshotResponse
sealSnapshot ctx snapId = do
  requirePermission PermSnapshotSeal ctx
  result <- withPool $ \conn -> DB.sealSnapshot conn (acTenantId ctx) snapId
  case result of
    Left err -> throwError $ InternalError $ T.pack $ show err
    Right hash -> do
      snapResult <- withPool $ \conn -> DB.getSnapshot conn (acTenantId ctx) snapId
      case snapResult of
        Nothing -> throwError $ SnapshotNotFound snapId
        Just s -> pure $ SnapshotResponse (DB.snapId s) "SEALED"
                           (Just hash) (DB.snapDescription s) (DB.snapCreatedAt s) (DB.snapSealedAt s)
