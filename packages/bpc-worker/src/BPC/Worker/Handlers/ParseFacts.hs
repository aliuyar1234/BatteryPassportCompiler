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

import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
  case Aeson.fromJSON (DB.jobPayload job) of
    Aeson.Error _err -> pure $ HRFailure $ HEValidation "Invalid PARSE_FACTS payload"
    Aeson.Success (_payload :: ParseFactsPayload) -> withResource pool $ \_conn -> do
      -- TODO: Implement fact parsing
      -- 1. Get document version
      -- 2. Check status is UPLOADED
      -- 3. Parse facts based on MIME type (JSON BOM, CSV PCF)
      -- 4. Store facts in database
      -- 5. Update document status to VALIDATED
      -- 6. Emit audit event
      pure HRSuccess

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
