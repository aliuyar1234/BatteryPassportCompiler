{-# LANGUAGE OverloadedStrings #-}

-- | Document Repository
--
-- Store and retrieve documents with SHA-256 content hash verification.
-- Documents are immutable once uploaded; new versions create new records.
--
-- @since 0.1.0.0
module BPC.DB.Repos.Documents
  ( -- * Types
    DocumentId
  , DocumentVersionId
  , DocumentInput(..)
  , Document(..)
  , DocumentVersion(..)
  , DocumentStatus(..)
    -- * Operations
  , createDocument
  , getDocument
  , listDocuments
  , uploadVersion
  , getDocumentVersion
  , getContent
  , getVersionsByDocument
  , updateDocumentStatus
  ) where

import Control.Exception (catch)
import Crypto.Hash (SHA256(..), hashWith)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection, Only(..), SqlError)
import qualified Database.PostgreSQL.Simple as PG
import Data.Word (Word8)
import GHC.Generics (Generic)

import BPC.DB.Error (DocumentError(..), UploadError(..))
import BPC.DB.Repos.Events (TenantId)

-- | Document ID (UUID)
type DocumentId = UUID

-- | Document Version ID (UUID)
type DocumentVersionId = UUID

-- | Document status enum.
data DocumentStatus
  = DocUploaded
  | DocParsed
  | DocValidated
  | DocRejected
  deriving stock (Show, Eq, Generic)

statusToText :: DocumentStatus -> Text
statusToText = \case
  DocUploaded  -> "UPLOADED"
  DocParsed    -> "PARSED"
  DocValidated -> "VALIDATED"
  DocRejected  -> "REJECTED"

textToStatus :: Text -> Maybe DocumentStatus
textToStatus = \case
  "UPLOADED"  -> Just DocUploaded
  "PARSED"    -> Just DocParsed
  "VALIDATED" -> Just DocValidated
  "REJECTED"  -> Just DocRejected
  _           -> Nothing

-- | Input for creating a new document.
data DocumentInput = DocumentInput
  { diName :: Text
  -- ^ Document name
  , diMimeType :: Text
  -- ^ MIME type (e.g., "application/json")
  , diMetadata :: Maybe Text
  -- ^ Optional metadata JSON
  }
  deriving stock (Show, Eq, Generic)

-- | Document record.
data Document = Document
  { docId :: DocumentId
  , docTenantId :: TenantId
  , docName :: Text
  , docMimeType :: Text
  , docMetadata :: Maybe Text
  , docCreatedAt :: UTCTime
  , docUpdatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Document version record.
data DocumentVersion = DocumentVersion
  { dvId :: DocumentVersionId
  , dvDocumentId :: DocumentId
  , dvTenantId :: TenantId
  , dvVersionNumber :: Int
  , dvContentHash :: Text
  , dvContentSize :: Int
  , dvStatus :: DocumentStatus
  , dvUploadedBy :: Maybe UUID
  , dvCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Calculate SHA-256 hash of content.
sha256Hex :: BS.ByteString -> Text
sha256Hex content =
  let digest = hashWith SHA256 content
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

-- | Create a new document (without content).
--
-- Returns the document ID. Content is uploaded separately via 'uploadVersion'.
--
-- @since 0.1.0.0
createDocument
  :: Connection
  -> TenantId
  -> DocumentInput
  -> IO (Either DocumentError DocumentId)
createDocument conn tenantId input = do
  result <- (Right <$> PG.query conn
    "INSERT INTO documents (tenant_id, name, mime_type, metadata) \
    \VALUES (?, ?, ?, ?) RETURNING id"
    (tenantId, diName input, diMimeType input, diMetadata input))
    `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ DOCUMENT_NOT_FOUND (error "No ID returned")
    Right (Only docId : _) -> pure $ Right docId
  where
    handleSqlError :: SqlError -> IO (Either DocumentError [Only UUID])
    handleSqlError _ = pure $ Left $ DOCUMENT_DUPLICATE_HASH "Constraint violation"

-- | Get a document by ID.
getDocument
  :: Connection
  -> TenantId
  -> DocumentId
  -> IO (Maybe Document)
getDocument conn tenantId docId = do
  rows <- PG.query conn
    "SELECT id, tenant_id, name, mime_type, metadata, created_at, updated_at \
    \FROM documents WHERE tenant_id = ? AND id = ?"
    (tenantId, docId)
  pure $ case rows of
    [] -> Nothing
    ((did, tid, name, mime, meta, cat, uat) : _) ->
      Just Document
        { docId = did
        , docTenantId = tid
        , docName = name
        , docMimeType = mime
        , docMetadata = meta
        , docCreatedAt = cat
        , docUpdatedAt = uat
        }

-- | List documents with cursor pagination.
--
-- Fetches documents ordered by (created_at DESC, id DESC).
-- If cursor is provided (timestamp, id), returns documents created before that point.
--
-- @since 0.1.0.0
listDocuments
  :: Connection
  -> TenantId
  -> Int
  -- ^ Limit (number of items to fetch)
  -> Maybe (UTCTime, UUID)
  -- ^ Optional cursor (timestamp, id)
  -> IO [Document]
listDocuments conn tenantId limit cursor = do
  rows <- case cursor of
    Nothing ->
      PG.query conn
        "SELECT id, tenant_id, name, mime_type, metadata, created_at, updated_at \
        \FROM documents \
        \WHERE tenant_id = ? \
        \ORDER BY created_at DESC, id DESC \
        \LIMIT ?"
        (tenantId, limit)
    Just (timestamp, cursorId) ->
      PG.query conn
        "SELECT id, tenant_id, name, mime_type, metadata, created_at, updated_at \
        \FROM documents \
        \WHERE tenant_id = ? AND (created_at, id) < (?, ?) \
        \ORDER BY created_at DESC, id DESC \
        \LIMIT ?"
        (tenantId, timestamp, cursorId, limit)

  pure $ map toDocument rows
  where
    toDocument (did, tid, name, mime, meta, cat, uat) =
      Document
        { docId = did
        , docTenantId = tid
        , docName = name
        , docMimeType = mime
        , docMetadata = meta
        , docCreatedAt = cat
        , docUpdatedAt = uat
        }

-- | Upload a new version of a document.
--
-- Calculates SHA-256 hash of content. Returns UploadDuplicateHash if
-- the same content already exists.
--
-- @since 0.1.0.0
uploadVersion
  :: Connection
  -> TenantId
  -> DocumentId
  -> BS.ByteString
  -> Maybe UUID
  -> IO (Either UploadError DocumentVersionId)
uploadVersion conn tenantId docId content uploadedBy = do
  -- Calculate content hash
  let contentHash = sha256Hex content
  let contentSize = BS.length content

  -- Get next version number
  versionNum <- getNextVersionNumber conn tenantId docId

  -- Insert version and content
  result <- insertVersion conn tenantId docId versionNum contentHash contentSize content uploadedBy

  pure result

-- | Get next version number for a document.
getNextVersionNumber :: Connection -> TenantId -> DocumentId -> IO Int
getNextVersionNumber conn tenantId docId = do
  [Only maxVer] <- PG.query conn
    "SELECT COALESCE(MAX(version_number), 0) FROM document_versions \
    \WHERE tenant_id = ? AND document_id = ?"
    (tenantId, docId)
  pure $ maxVer + 1

-- | Insert a document version.
insertVersion
  :: Connection
  -> TenantId
  -> DocumentId
  -> Int
  -> Text
  -> Int
  -> BS.ByteString
  -> Maybe UUID
  -> IO (Either UploadError DocumentVersionId)
insertVersion conn tenantId docId verNum contentHash contentSize content uploadedBy = do
  result <- (Right <$> PG.query conn
    "INSERT INTO document_versions \
    \(tenant_id, document_id, version_number, content_hash, content_size, \
    \ content, status, uploaded_by) \
    \VALUES (?, ?, ?, ?, ?, ?, 'UPLOADED', ?) \
    \RETURNING id"
    (tenantId, docId, verNum, contentHash, contentSize, PG.Binary content, uploadedBy))
    `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ UploadDocumentNotFound docId
    Right (Only vid : _) -> pure $ Right vid
  where
    handleSqlError :: SqlError -> IO (Either UploadError [Only UUID])
    handleSqlError _ = pure $ Left $ UploadDuplicateHash contentHash

-- | Get a document version by ID.
getDocumentVersion
  :: Connection
  -> TenantId
  -> DocumentVersionId
  -> IO (Maybe DocumentVersion)
getDocumentVersion conn tenantId versionId = do
  rows <- PG.query conn
    "SELECT id, document_id, tenant_id, version_number, content_hash, \
    \       content_size, status, uploaded_by, created_at \
    \FROM document_versions WHERE tenant_id = ? AND id = ?"
    (tenantId, versionId)
  pure $ case rows of
    [] -> Nothing
    ((vid, did, tid, vnum, chash, csize, status, upBy, cat) : _) ->
      Just DocumentVersion
        { dvId = vid
        , dvDocumentId = did
        , dvTenantId = tid
        , dvVersionNumber = vnum
        , dvContentHash = chash
        , dvContentSize = csize
        , dvStatus = maybe DocUploaded id $ textToStatus status
        , dvUploadedBy = upBy
        , dvCreatedAt = cat
        }

-- | Get document content by version ID.
--
-- Returns the raw bytes of the document content.
--
-- @since 0.1.0.0
getContent
  :: Connection
  -> TenantId
  -> DocumentVersionId
  -> IO (Maybe BS.ByteString)
getContent conn tenantId versionId = do
  rows <- PG.query conn
    "SELECT content FROM document_versions WHERE tenant_id = ? AND id = ?"
    (tenantId, versionId)
  pure $ case rows of
    [] -> Nothing
    (Only (PG.Binary content) : _) -> Just content

-- | Get all versions for a document.
getVersionsByDocument
  :: Connection
  -> TenantId
  -> DocumentId
  -> IO [DocumentVersion]
getVersionsByDocument conn tenantId docId = do
  rows <- PG.query conn
    "SELECT id, document_id, tenant_id, version_number, content_hash, \
    \       content_size, status, uploaded_by, created_at \
    \FROM document_versions \
    \WHERE tenant_id = ? AND document_id = ? \
    \ORDER BY version_number DESC"
    (tenantId, docId)
  pure $ map toVersion rows
  where
    toVersion (vid, did, tid, vnum, chash, csize, status, upBy, cat) =
      DocumentVersion
        { dvId = vid
        , dvDocumentId = did
        , dvTenantId = tid
        , dvVersionNumber = vnum
        , dvContentHash = chash
        , dvContentSize = csize
        , dvStatus = maybe DocUploaded id $ textToStatus status
        , dvUploadedBy = upBy
        , dvCreatedAt = cat
        }

-- | Update document version status.
updateDocumentStatus
  :: Connection
  -> TenantId
  -> DocumentVersionId
  -> DocumentStatus
  -> IO Bool
updateDocumentStatus conn tenantId versionId newStatus = do
  n <- PG.execute conn
    "UPDATE document_versions SET status = ? WHERE tenant_id = ? AND id = ?"
    (statusToText newStatus, tenantId, versionId)
  pure $ n > 0
