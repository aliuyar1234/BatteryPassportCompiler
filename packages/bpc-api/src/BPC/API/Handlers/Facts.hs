{-# LANGUAGE OverloadedStrings #-}

-- | Fact Handlers
module BPC.API.Handlers.Facts
  ( createFact
  , listFacts
  , getFact
  , getFactsByPrefix
  ) where

import Control.Monad.Except (throwError)
import Data.Aeson (ToJSON(..), object, (.=), Value)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

import BPC.API.App (AppM, withPool)
import BPC.API.Error (AppError(..))
import BPC.API.Types (AuthContext(..), Permission(..), CursorPage(..), CreatedResponse(..),
                       PaginationParams(..), Cursor(..), parseCursor, encodeCursor)
import BPC.API.Middleware.Auth (requirePermission)
import qualified BPC.DB as DB

data FactResponse = FactResponse
  { frId :: UUID
  , frFactType :: Text
  , frFactKey :: Text
  , frPayload :: Value
  , frPayloadHash :: Text
  , frCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FactResponse where
  toJSON FactResponse{..} = object
    [ "id" .= frId, "fact_type" .= frFactType, "fact_key" .= frFactKey
    , "payload" .= frPayload, "payload_hash" .= frPayloadHash
    , "created_at" .= frCreatedAt
    ]

createFact :: AuthContext -> Text -> Text -> Value -> AppM CreatedResponse
createFact ctx factType factKey payload = do
  requirePermission PermDocumentWrite ctx
  let input = DB.FactInput factType factKey payload Nothing
  result <- withPool $ \conn -> DB.insertFact conn (acTenantId ctx) input
  case result of
    Left err -> throwError $ InternalError "Failed to create fact"
    Right factId -> pure CreatedResponse { crId = factId, crLocation = "/v1/facts/" }

listFacts :: AuthContext -> Text -> PaginationParams -> AppM (CursorPage FactResponse)
listFacts ctx factType params = do
  requirePermission PermDocumentRead ctx

  -- Note: DB.getFactsByType does not support cursor pagination yet
  -- For now, we fetch all facts and apply pagination in-memory
  -- TODO: Add cursor-based pagination to DB layer for better performance
  allFacts <- withPool $ \conn -> DB.getFactsByType conn (acTenantId ctx) factType

  -- Sort by created_at DESC, id DESC
  let sortedFacts = reverse $ sortOn (\f -> (DB.factCreatedAt f, DB.factId f)) allFacts

  -- Apply cursor filter if provided
  let filteredFacts = case ppCursor params of
        Nothing -> sortedFacts
        Just c -> case parseCursor c of
          Right cursor -> filter (\f -> (DB.factCreatedAt f, DB.factId f) < (cursorTimestamp cursor, cursorId cursor)) sortedFacts
          Left _ -> sortedFacts

  -- Take limit + 1 to check for more
  let limit = ppLimit params + 1
  let paginatedFacts = take limit filteredFacts

  -- Check if there are more items
  let hasMore = length paginatedFacts > ppLimit params
  let items = take (ppLimit params) paginatedFacts

  -- Generate next cursor if there are more items
  let nextCursor = if hasMore && not (null items)
        then Just $ encodeCursor $ Cursor
               { cursorTimestamp = DB.factCreatedAt (last items)
               , cursorId = DB.factId (last items)
               }
        else Nothing

  pure CursorPage
    { cpItems = map toResponse items
    , cpNextCursor = nextCursor
    , cpHasMore = hasMore
    }
  where
    toResponse f = FactResponse
      { frId = DB.factId f
      , frFactType = DB.factType f
      , frFactKey = DB.factKey f
      , frPayload = DB.factPayload f
      , frPayloadHash = DB.factPayloadHash f
      , frCreatedAt = DB.factCreatedAt f
      }

getFact :: AuthContext -> Text -> Text -> AppM FactResponse
getFact ctx factType factKey = do
  requirePermission PermDocumentRead ctx
  result <- withPool $ \conn -> DB.getFact conn (acTenantId ctx) factType factKey
  case result of
    Nothing -> throwError $ NotFound "Fact"
    Just f -> pure $ FactResponse (DB.factId f) (DB.factType f) (DB.factKey f)
                       (DB.factPayload f) (DB.factPayloadHash f) (DB.factCreatedAt f)

getFactsByPrefix :: AuthContext -> Text -> Text -> AppM [FactResponse]
getFactsByPrefix ctx factType prefix = do
  requirePermission PermDocumentRead ctx
  facts <- withPool $ \conn -> DB.getFactsByPrefix conn (acTenantId ctx) factType prefix
  pure $ map toResponse facts
  where
    toResponse f = FactResponse (DB.factId f) (DB.factType f) (DB.factKey f)
                     (DB.factPayload f) (DB.factPayloadHash f) (DB.factCreatedAt f)
