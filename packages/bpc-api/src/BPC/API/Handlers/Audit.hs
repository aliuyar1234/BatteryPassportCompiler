{-# LANGUAGE OverloadedStrings #-}

-- | Audit Handlers
module BPC.API.Handlers.Audit
  ( listAuditEvents
  , getAuditEvent
  , verifyEventChain
  ) where

import Control.Monad.Except (throwError)
import Data.Aeson (ToJSON(..), Value, object, (.=))
import qualified Data.ByteString as BS
import Data.List (sortOn)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

import BPC.API.App (AppM, withPool)
import BPC.API.Error (AppError(..))
import BPC.API.Types (AuthContext(..), Permission(..), CursorPage(..),
                       PaginationParams(..), Cursor(..), parseCursor, encodeCursor)
import BPC.API.Middleware.Auth (requirePermission)
import qualified BPC.DB as DB

data AuditEventResponse = AuditEventResponse
  { aerId :: UUID, aerAggregateType :: Text, aerAggregateId :: UUID
  , aerEventType :: Text, aerEventVersion :: Int
  , aerEventHash :: Text, aerCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AuditEventResponse where
  toJSON AuditEventResponse{..} = object
    [ "id" .= aerId, "aggregate_type" .= aerAggregateType, "aggregate_id" .= aerAggregateId
    , "event_type" .= aerEventType, "event_version" .= aerEventVersion
    , "event_hash" .= aerEventHash, "created_at" .= aerCreatedAt
    ]

data VerifyChainResponse = VerifyChainResponse
  { vcrValid :: Bool, vcrTotalEvents :: Int, vcrBrokenChains :: Int }
  deriving stock (Show, Eq, Generic)

instance ToJSON VerifyChainResponse where
  toJSON VerifyChainResponse{..} = object
    [ "valid" .= vcrValid, "total_events" .= vcrTotalEvents, "broken_chains" .= vcrBrokenChains ]

listAuditEvents :: AuthContext -> Text -> UUID -> PaginationParams -> AppM (CursorPage AuditEventResponse)
listAuditEvents ctx aggType aggId params = do
  requirePermission PermAuditRead ctx
  let mAggType = textToAggType aggType
  case mAggType of
    Nothing -> throwError $ InvalidRequest "Invalid aggregate type"
    Just at -> do
      -- Note: DB.getEventsByAggregate does not support cursor pagination yet
      -- For now, we fetch all events and apply pagination in-memory
      allEvents <- withPool $ \conn -> DB.getEventsByAggregate conn (acTenantId ctx) at aggId

      -- Sort by created_at DESC, id DESC
      let sortedEvents = reverse $ sortOn (\e -> (DB.evCreatedAt e, DB.evId e)) allEvents

      -- Apply cursor filter if provided
      let filteredEvents = case ppCursor params of
            Nothing -> sortedEvents
            Just c -> case parseCursor c of
              Right cursor -> filter (\e -> (DB.evCreatedAt e, DB.evId e) < (cursorTimestamp cursor, cursorId cursor)) sortedEvents
              Left _ -> sortedEvents

      -- Take limit + 1 to check for more
      let limit = ppLimit params + 1
      let paginatedEvents = take limit filteredEvents

      -- Check if there are more items
      let hasMore = length paginatedEvents > ppLimit params
      let items = take (ppLimit params) paginatedEvents

      -- Generate next cursor if there are more items
      let nextCursor = if hasMore && not (null items)
            then Just $ encodeCursor $ Cursor
                   { cursorTimestamp = DB.evCreatedAt (last items)
                   , cursorId = DB.evId (last items)
                   }
            else Nothing

      pure CursorPage
        { cpItems = map toResponse items
        , cpNextCursor = nextCursor
        , cpHasMore = hasMore
        }
  where
    toResponse e = AuditEventResponse
      { aerId = DB.evId e
      , aerAggregateType = DB.evAggregateType e
      , aerAggregateId = DB.evAggregateId e
      , aerEventType = DB.evEventType e
      , aerEventVersion = DB.evEventVersion e
      , aerEventHash = DB.evEventHash e
      , aerCreatedAt = DB.evCreatedAt e
      }
    textToAggType t = case t of
      "document" -> Just DB.AggDocument
      "snapshot" -> Just DB.AggSnapshot
      "passport" -> Just DB.AggPassport
      "job" -> Just DB.AggJob
      "rule_package" -> Just DB.AggRulePackage
      _ -> Nothing

getAuditEvent :: AuthContext -> UUID -> AppM AuditEventResponse
getAuditEvent ctx eventId = do
  requirePermission PermAuditRead ctx
  result <- withPool $ \conn -> DB.getEvent conn (acTenantId ctx) eventId
  case result of
    Nothing -> throwError $ NotFound "Audit event"
    Just e -> pure $ AuditEventResponse (DB.evId e) (DB.evAggregateType e) (DB.evAggregateId e)
                       (DB.evEventType e) (DB.evEventVersion e) (DB.evEventHash e) (DB.evCreatedAt e)

verifyEventChain :: AuthContext -> AppM VerifyChainResponse
verifyEventChain ctx = do
  requirePermission PermAuditRead ctx
  result <- withPool $ \conn -> DB.verifyAllChains conn (acTenantId ctx)
  case result of
    Left err -> throwError $ InternalError "Chain verification failed"
    Right report -> pure VerifyChainResponse
      { vcrValid = null (DB.vrBrokenChains report)
      , vcrTotalEvents = DB.vrTotalEvents report
      , vcrBrokenChains = length (DB.vrBrokenChains report)
      }
