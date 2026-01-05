{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Event Store Repository
--
-- Append-only event store with BPC-EVENT-1 hash chain for tamper detection.
-- All events are tenant-scoped and linked via cryptographic hash chain.
--
-- == BPC-EVENT-1 Hash Calculation
--
-- @
-- event_hash = SHA256(
--   aggregate_type || "|" ||
--   aggregate_id || "|" ||
--   event_type || "|" ||
--   event_version || "|" ||
--   prev_event_hash || "|" ||
--   payload_canonical
-- )
-- @
--
-- @since 0.1.0.0
module BPC.DB.Repos.Events
  ( -- * Types
    EventId
  , TenantId
  , AggregateType(..)
  , AppendEventInput(..)
  , Event(..)
  , VerifyReport(..)
    -- * Operations
  , appendEvent
  , getEvent
  , getEventsByAggregate
  , verifyChain
  , verifyAllChains
    -- * Hash Calculation
  , calculateEventHash
  ) where

import Control.Exception (catch)
import Crypto.Hash (SHA256(..), hashWith)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple (Connection, Only(..), SqlError)
import qualified Database.PostgreSQL.Simple as PG
import Data.Word (Word8)
import GHC.Generics (Generic)

import BPC.DB.Error (ChainError(..), EventError(..))

-- | Event ID (UUID)
type EventId = UUID

-- | Tenant ID (UUID)
type TenantId = UUID

-- | Aggregate type for event grouping.
data AggregateType
  = AggDocument
  | AggSnapshot
  | AggPassport
  | AggJob
  | AggRulePackage
  | AggTenant
  | AggActor
  deriving stock (Show, Eq, Generic)

aggregateTypeToText :: AggregateType -> Text
aggregateTypeToText = \case
  AggDocument -> "document"
  AggSnapshot -> "snapshot"
  AggPassport -> "passport"
  AggJob -> "job"
  AggRulePackage -> "rule_package"
  AggTenant -> "tenant"
  AggActor -> "actor"

textToAggregateType :: Text -> Maybe AggregateType
textToAggregateType = \case
  "document" -> Just AggDocument
  "snapshot" -> Just AggSnapshot
  "passport" -> Just AggPassport
  "job" -> Just AggJob
  "rule_package" -> Just AggRulePackage
  "tenant" -> Just AggTenant
  "actor" -> Just AggActor
  _ -> Nothing

-- | Input for appending a new event.
data AppendEventInput = AppendEventInput
  { aeiAggregateType :: AggregateType
  -- ^ Type of aggregate (document, snapshot, etc.)
  , aeiAggregateId :: UUID
  -- ^ ID of the aggregate
  , aeiEventType :: Text
  -- ^ Event type (e.g., "created", "updated", "sealed")
  , aeiEventVersion :: Int
  -- ^ Event version (for optimistic concurrency)
  , aeiActorId :: Maybe UUID
  -- ^ Actor who triggered the event (if any)
  , aeiPayload :: Value
  -- ^ Event payload (JSON)
  }
  deriving stock (Show, Eq, Generic)

-- | Stored event record.
data Event = Event
  { evId :: EventId
  , evTenantId :: TenantId
  , evAggregateType :: Text
  , evAggregateId :: UUID
  , evEventType :: Text
  , evEventVersion :: Int
  , evPrevEventHash :: Maybe Text
  , evEventHash :: Text
  , evPayloadCanonical :: BS.ByteString
  , evActorId :: Maybe UUID
  , evCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Report from chain verification.
data VerifyReport = VerifyReport
  { vrTotalAggregates :: Int
  -- ^ Number of aggregates verified
  , vrTotalEvents :: Int
  -- ^ Number of events verified
  , vrBrokenChains :: [(UUID, Text, ChainError)]
  -- ^ List of (aggregate_id, aggregate_type, error)
  }
  deriving stock (Show, Eq, Generic)

-- | Calculate event hash per BPC-EVENT-1.
--
-- Hash = SHA256(aggregate_type | aggregate_id | event_type | version | prev_hash | payload)
calculateEventHash
  :: Text           -- ^ Aggregate type
  -> UUID           -- ^ Aggregate ID
  -> Text           -- ^ Event type
  -> Int            -- ^ Event version
  -> Maybe Text     -- ^ Previous event hash (Nothing for genesis)
  -> BS.ByteString  -- ^ Canonical payload bytes
  -> Text
calculateEventHash aggType aggId evType evVer prevHash payload =
  let parts =
        [ TE.encodeUtf8 aggType
        , TE.encodeUtf8 "|"
        , TE.encodeUtf8 $ UUID.toText aggId
        , TE.encodeUtf8 "|"
        , TE.encodeUtf8 evType
        , TE.encodeUtf8 "|"
        , TE.encodeUtf8 $ T.pack $ show evVer
        , TE.encodeUtf8 "|"
        , TE.encodeUtf8 $ maybe "" id prevHash
        , TE.encodeUtf8 "|"
        , payload
        ]
      bytes = BS.concat parts
      digest = hashWith SHA256 bytes
      hexBytes = convert digest :: BS.ByteString
  in TE.decodeUtf8 $ BS.concatMap (toHex) hexBytes
  where
    toHex :: Word8 -> BS.ByteString
    toHex w =
      let (hi, lo) = w `divMod` 16
      in BS.pack [hexChar hi, hexChar lo]

    hexChar :: Word8 -> Word8
    hexChar n
      | n < 10    = 48 + n  -- '0' + n
      | otherwise = 87 + n  -- 'a' + (n - 10)

-- | Append a new event to the event store.
--
-- Calculates hash chain, handles optimistic concurrency.
-- Returns EVENT_VERSION_CONFLICT if another writer won.
--
-- @since 0.1.0.0
appendEvent
  :: Connection
  -> TenantId
  -> AppendEventInput
  -> IO (Either EventError EventId)
appendEvent conn tenantId input = do
  -- Get previous event hash for this aggregate
  prevHash <- getPrevEventHash conn tenantId
    (aggregateTypeToText $ aeiAggregateType input)
    (aeiAggregateId input)

  -- Canonicalize payload
  let payloadCanonical = LBS.toStrict $ Aeson.encode (aeiPayload input)

  -- Calculate event hash
  let eventHash = calculateEventHash
        (aggregateTypeToText $ aeiAggregateType input)
        (aeiAggregateId input)
        (aeiEventType input)
        (aeiEventVersion input)
        prevHash
        payloadCanonical

  -- Insert event
  result <- insertEvent conn tenantId input prevHash eventHash payloadCanonical
  pure result

-- | Get previous event hash for an aggregate.
getPrevEventHash
  :: Connection
  -> TenantId
  -> Text
  -> UUID
  -> IO (Maybe Text)
getPrevEventHash conn tenantId aggType aggId = do
  rows <- PG.query conn
    "SELECT event_hash FROM audit_events \
    \WHERE tenant_id = ? AND aggregate_type = ? AND aggregate_id = ? \
    \ORDER BY event_version DESC LIMIT 1"
    (tenantId, aggType, aggId)
  pure $ case rows of
    []             -> Nothing
    (Only h : _)   -> Just h

-- | Insert event with conflict handling.
insertEvent
  :: Connection
  -> TenantId
  -> AppendEventInput
  -> Maybe Text
  -> Text
  -> BS.ByteString
  -> IO (Either EventError EventId)
insertEvent conn tenantId input prevHash eventHash payloadCanonical = do
  let aggType = aggregateTypeToText $ aeiAggregateType input
  result <- (Right <$> PG.query conn
    "INSERT INTO audit_events \
    \(tenant_id, aggregate_type, aggregate_id, event_type, event_version, \
    \ prev_event_hash, event_hash, payload_canonical, actor_id) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \RETURNING id"
    ( tenantId
    , aggType
    , aeiAggregateId input
    , aeiEventType input
    , aeiEventVersion input
    , prevHash
    , eventHash
    , payloadCanonical
    , aeiActorId input
    )) `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ EVENT_INVALID_HASH "No ID returned"
    Right (Only eid : _) -> pure $ Right eid
  where
    handleSqlError :: SqlError -> IO (Either EventError [Only UUID])
    handleSqlError _ = pure $ Left EVENT_VERSION_CONFLICT

-- | Get a single event by ID.
getEvent
  :: Connection
  -> TenantId
  -> EventId
  -> IO (Maybe Event)
getEvent conn tenantId eventId = do
  rows <- PG.query conn
    "SELECT id, tenant_id, aggregate_type, aggregate_id, event_type, \
    \       event_version, prev_event_hash, event_hash, payload_canonical, \
    \       actor_id, created_at \
    \FROM audit_events \
    \WHERE tenant_id = ? AND id = ?"
    (tenantId, eventId)
  pure $ case rows of
    [] -> Nothing
    ((eid, tid, at, aid, et, ev, peh, eh, pc, actId, cat) : _) ->
      Just Event
        { evId = eid
        , evTenantId = tid
        , evAggregateType = at
        , evAggregateId = aid
        , evEventType = et
        , evEventVersion = ev
        , evPrevEventHash = peh
        , evEventHash = eh
        , evPayloadCanonical = pc
        , evActorId = actId
        , evCreatedAt = cat
        }

-- | Get all events for an aggregate.
getEventsByAggregate
  :: Connection
  -> TenantId
  -> AggregateType
  -> UUID
  -> IO [Event]
getEventsByAggregate conn tenantId aggType aggId = do
  rows <- PG.query conn
    "SELECT id, tenant_id, aggregate_type, aggregate_id, event_type, \
    \       event_version, prev_event_hash, event_hash, payload_canonical, \
    \       actor_id, created_at \
    \FROM audit_events \
    \WHERE tenant_id = ? AND aggregate_type = ? AND aggregate_id = ? \
    \ORDER BY event_version ASC"
    (tenantId, aggregateTypeToText aggType, aggId)
  pure $ map toEvent rows
  where
    toEvent (eid, tid, at, aid, et, ev, peh, eh, pc, actId, cat) = Event
      { evId = eid
      , evTenantId = tid
      , evAggregateType = at
      , evAggregateId = aid
      , evEventType = et
      , evEventVersion = ev
      , evPrevEventHash = peh
      , evEventHash = eh
      , evPayloadCanonical = pc
      , evActorId = actId
      , evCreatedAt = cat
      }

-- | Verify hash chain for a specific aggregate.
--
-- Checks that each event's prev_event_hash matches the previous event's hash.
--
-- @since 0.1.0.0
verifyChain
  :: Connection
  -> TenantId
  -> AggregateType
  -> UUID
  -> IO (Either ChainError ())
verifyChain conn tenantId aggType aggId = do
  events <- getEventsByAggregate conn tenantId aggType aggId
  pure $ verifyEventChain events

-- | Verify a list of events forms a valid chain.
verifyEventChain :: [Event] -> Either ChainError ()
verifyEventChain [] = Left ChainEmpty
verifyEventChain (first : rest) = do
  -- Genesis event must have null prev_event_hash
  case evPrevEventHash first of
    Just _ -> Left ChainInvalidGenesis
    Nothing -> verifyRest (evEventHash first) rest

verifyRest :: Text -> [Event] -> Either ChainError ()
verifyRest _ [] = Right ()
verifyRest expectedPrev (ev : rest) = do
  case evPrevEventHash ev of
    Nothing -> Left $ ChainBroken (evId ev) expectedPrev ""
    Just actual
      | actual /= expectedPrev ->
          Left $ ChainBroken (evId ev) expectedPrev actual
      | otherwise ->
          verifyRest (evEventHash ev) rest

-- | Verify all hash chains for a tenant.
--
-- Returns a report with statistics and any broken chains found.
--
-- @since 0.1.0.0
verifyAllChains
  :: Connection
  -> TenantId
  -> IO (Either ChainError VerifyReport)
verifyAllChains conn tenantId = do
  -- Get all distinct aggregates
  aggregates <- PG.query conn
    "SELECT DISTINCT aggregate_type, aggregate_id FROM audit_events \
    \WHERE tenant_id = ?"
    (Only tenantId) :: IO [(Text, UUID)]

  -- Count total events
  [Only totalEvents] <- PG.query conn
    "SELECT COUNT(*) FROM audit_events WHERE tenant_id = ?"
    (Only tenantId) :: IO [Only Int]

  -- Verify each aggregate
  broken <- mapM (verifyAggregate conn tenantId) aggregates

  let brokenList = concat broken
  pure $ Right VerifyReport
    { vrTotalAggregates = length aggregates
    , vrTotalEvents = totalEvents
    , vrBrokenChains = brokenList
    }

-- | Verify a single aggregate's chain.
verifyAggregate
  :: Connection
  -> TenantId
  -> (Text, UUID)
  -> IO [(UUID, Text, ChainError)]
verifyAggregate conn tenantId (aggType, aggId) = do
  case textToAggregateType aggType of
    Nothing -> pure [(aggId, aggType, ChainEmpty)]
    Just at -> do
      result <- verifyChain conn tenantId at aggId
      case result of
        Right () -> pure []
        Left err -> pure [(aggId, aggType, err)]
