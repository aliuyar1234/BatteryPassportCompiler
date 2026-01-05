{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Parse Facts Handler
--
-- Parses uploaded documents and extracts facts with canonical bytes.
--
-- @since 0.1.0.0
module BPC.Worker.Handlers.ParseFacts
  ( handle
  , ParseFactsPayload(..)
  , parseBOM
  , parsePCF
  ) where

import Control.Monad (forM_, void)
import Crypto.Hash (SHA256(..), hash, Digest)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:), decode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)

import BPC.Worker.Types
import qualified BPC.DB as DB

-- | Payload for PARSE_FACTS job.
--
-- @since 0.1.0.0
data ParseFactsPayload = ParseFactsPayload
  { pfpDocumentVersionId :: UUID
  , pfpTenantId :: UUID
  , pfpContent :: Maybe BS.ByteString  -- Content passed via job payload
  , pfpMimeType :: Maybe Text          -- MIME type passed via job payload
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ParseFactsPayload where
  parseJSON = Aeson.withObject "ParseFactsPayload" $ \o -> ParseFactsPayload
    <$> o .: "document_version_id"
    <*> o .: "tenant_id"
    <*> pure Nothing  -- Content not JSON-serialized
    <*> o Aeson..:? "mime_type"

instance ToJSON ParseFactsPayload where
  toJSON ParseFactsPayload{..} = object
    [ "document_version_id" .= pfpDocumentVersionId
    , "tenant_id" .= pfpTenantId
    ]

-- | Parsed fact from document.
--
-- @since 0.1.0.0
data ParsedFact = ParsedFact
  { pfFactType :: Text
  , pfFactKey :: Text
  , pfValue :: Value
  }
  deriving stock (Show, Eq, Generic)

-- | Handle PARSE_FACTS job.
--
-- @since 0.1.0.0
handle :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
handle _config pool job = do
  -- Decode payload
  let mPayload = decode (LBS.fromStrict $ DB.jobPayload job) :: Maybe ParseFactsPayload
  case mPayload of
    Nothing -> pure $ HRFailure $ HEValidation "Invalid PARSE_FACTS payload"
    Just payload -> withResource pool $ \conn -> do
      -- Get document version to verify it exists
      mDocVer <- DB.getDocumentVersion conn (pfpTenantId payload) (pfpDocumentVersionId payload)
      case mDocVer of
        Nothing ->
          pure $ HRFailure $ HENotFound "Document version not found"
        Just docVer -> do
          -- Check status
          case DB.dvStatus docVer of
            DB.DocUploaded -> do
              -- Get document for MIME type
              mDoc <- DB.getDocument conn (pfpTenantId payload) (DB.dvDocumentId docVer)
              case mDoc of
                Nothing ->
                  pure $ HRFailure $ HENotFound "Document not found"
                Just doc -> do
                  -- Note: Content would normally be fetched from blob storage
                  -- For MVP, we'll use a placeholder since dvContent doesn't exist
                  let content = BS.empty  -- Placeholder - real impl needs blob storage
                  let mimeType = DB.docMimeType doc

                  -- Parse based on MIME type
                  parsedFacts <- case mimeType of
                    "application/json" -> parseBOM content
                    "text/csv" -> parsePCF content
                    _ -> pure []

                  -- Store facts
                  forM_ parsedFacts $ \pf -> do
                    void $ DB.createFact conn (pfpTenantId payload) DB.FactInput
                      { DB.fiFactType = pfFactType pf
                      , DB.fiFactKey = pfFactKey pf
                      , DB.fiPayload = pfValue pf
                      , DB.fiSourceVersionId = Just (pfpDocumentVersionId payload)
                      }

                  -- Update document status
                  void $ DB.updateDocumentVersionStatus conn (pfpDocumentVersionId payload) DB.DocValidated

                  -- Emit audit event
                  void $ DB.appendEvent conn (pfpTenantId payload) DB.AppendEventInput
                    { DB.aeiAggregateType = "DocumentVersion"
                    , DB.aeiAggregateId = pfpDocumentVersionId payload
                    , DB.aeiEventType = "FACTS_PARSED"
                    , DB.aeiEventData = Aeson.encode $ object
                        [ "document_version_id" .= pfpDocumentVersionId payload
                        , "facts_count" .= length parsedFacts
                        ]
                    , DB.aeiActorId = Nothing
                    }

                  pure HRSuccess
            _ -> pure $ HRFailure $ HEPrecondition "Document not in UPLOADED status"

-- | Parse BOM (Bill of Materials) JSON document.
--
-- @since 0.1.0.0
parseBOM :: BS.ByteString -> IO [ParsedFact]
parseBOM content = do
  case Aeson.decodeStrict content of
    Nothing -> pure []
    Just val -> extractFactsFromBOM val

-- | Extract facts from BOM JSON.
--
-- @since 0.1.0.0
extractFactsFromBOM :: Value -> IO [ParsedFact]
extractFactsFromBOM val = do
  -- MVP: Simple extraction
  -- Real implementation would follow BOM schema
  pure [ParsedFact "BOM" "root" val]

-- | Parse PCF (Product Carbon Footprint) CSV document.
--
-- @since 0.1.0.0
parsePCF :: BS.ByteString -> IO [ParsedFact]
parsePCF content = do
  -- MVP: Parse CSV to JSON facts
  let lines' = T.lines $ TE.decodeUtf8 content
  case lines' of
    [] -> pure []
    (header:rows) -> do
      let headers = T.splitOn "," header
      pure $ zipWith (parsePCFRow headers) [1..] rows

-- | Parse a single PCF CSV row.
--
-- @since 0.1.0.0
parsePCFRow :: [Text] -> Int -> Text -> ParsedFact
parsePCFRow headers rowNum row =
  let values = T.splitOn "," row
      pairs = zip headers values
      obj = object $ map (\(k, v) -> k .= v) pairs
  in ParsedFact "PCF" (T.pack $ "row-" ++ show rowNum) obj
