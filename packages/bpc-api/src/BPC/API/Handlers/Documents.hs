{-# LANGUAGE OverloadedStrings #-}

-- | Document Handlers
--
-- HTTP handlers for document CRUD operations.
--
-- @since 0.1.0.0
module BPC.API.Handlers.Documents
  ( -- * Handlers
    createDocument
  , listDocuments
  , getDocument
  , uploadDocumentVersion
  , getDocumentVersionContent
  , listDocumentVersions
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

import BPC.API.App (AppM, withPool)
import BPC.API.Error (AppError(..))
import BPC.API.Types (AuthContext(..), Permission(..), CursorPage(..),
                       CreatedResponse(..), PaginationParams(..), Cursor(..),
                       encodeCursor)
import BPC.API.Middleware.Auth (requirePermission)
import qualified BPC.DB as DB

-- | Request to create a document.
data CreateDocumentRequest = CreateDocumentRequest
  { cdrName :: Text
  , cdrMimeType :: Text
  , cdrMetadata :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CreateDocumentRequest where
  parseJSON = Aeson.withObject "CreateDocumentRequest" $ \o -> CreateDocumentRequest
    <$> o .: "name"
    <*> o .: "mime_type"
    <*> o .:? "metadata"

-- | Document response.
data DocumentResponse = DocumentResponse
  { drId :: UUID
  , drName :: Text
  , drMimeType :: Text
  , drMetadata :: Maybe Text
  , drCreatedAt :: UTCTime
  , drUpdatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON DocumentResponse where
  toJSON DocumentResponse{..} = object
    [ "id" .= drId
    , "name" .= drName
    , "mime_type" .= drMimeType
    , "metadata" .= drMetadata
    , "created_at" .= drCreatedAt
    , "updated_at" .= drUpdatedAt
    ]

-- | Document version response.
data DocumentVersionResponse = DocumentVersionResponse
  { dvrId :: UUID
  , dvrDocumentId :: UUID
  , dvrVersionNumber :: Int
  , dvrContentHash :: Text
  , dvrContentSize :: Int
  , dvrStatus :: Text
  , dvrCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON DocumentVersionResponse where
  toJSON DocumentVersionResponse{..} = object
    [ "id" .= dvrId
    , "document_id" .= dvrDocumentId
    , "version_number" .= dvrVersionNumber
    , "content_hash" .= dvrContentHash
    , "content_size" .= dvrContentSize
    , "status" .= dvrStatus
    , "created_at" .= dvrCreatedAt
    ]

-- | Create a new document.
--
-- POST /v1/documents
-- Requires: document:write
--
-- @since 0.1.0.0
createDocument
  :: AuthContext
  -> CreateDocumentRequest
  -> AppM CreatedResponse
createDocument ctx req = do
  requirePermission PermDocumentWrite ctx

  let input = DB.DocumentInput
        { DB.diName = cdrName req
        , DB.diMimeType = cdrMimeType req
        , DB.diMetadata = cdrMetadata req
        }

  result <- withPool $ \conn ->
    DB.createDocument conn (acTenantId ctx) input

  case result of
    Left err -> throwError $ InternalError $ T.pack $ show err
    Right docId -> pure CreatedResponse
      { crId = docId
      , crLocation = "/v1/documents/" <> T.pack (show docId)
      }

-- | List documents with pagination.
--
-- GET /v1/documents
-- Requires: document:read
--
-- @since 0.1.0.0
listDocuments
  :: AuthContext
  -> PaginationParams
  -> AppM (CursorPage DocumentResponse)
listDocuments ctx params = do
  requirePermission PermDocumentRead ctx

  -- Extract cursor data if provided (ppCursor is already parsed)
  let cursorData = case ppCursor params of
        Nothing -> Nothing
        Just cursor -> Just (cursorTimestamp cursor, cursorId cursor)

  -- Fetch one extra item to check if there are more
  let limit = ppLimit params + 1

  docs <- withPool $ \conn ->
    DB.listDocuments conn (acTenantId ctx) limit cursorData

  -- Check if there are more items
  let hasMore = length docs > ppLimit params
  let items = take (ppLimit params) docs

  -- Generate next cursor if there are more items
  let nextCursor = if hasMore && not (null items)
        then Just $ encodeCursor $ Cursor
               { cursorTimestamp = DB.docCreatedAt (last items)
               , cursorId = DB.docId (last items)
               }
        else Nothing

  pure CursorPage
    { cpItems = map toResponse items
    , cpNextCursor = nextCursor
    , cpHasMore = hasMore
    }
  where
    toResponse doc = DocumentResponse
      { drId = DB.docId doc
      , drName = DB.docName doc
      , drMimeType = DB.docMimeType doc
      , drMetadata = DB.docMetadata doc
      , drCreatedAt = DB.docCreatedAt doc
      , drUpdatedAt = DB.docUpdatedAt doc
      }

-- | Get a single document.
--
-- GET /v1/documents/:id
-- Requires: document:read
--
-- @since 0.1.0.0
getDocument
  :: AuthContext
  -> UUID
  -> AppM DocumentResponse
getDocument ctx docId = do
  requirePermission PermDocumentRead ctx

  result <- withPool $ \conn ->
    DB.getDocument conn (acTenantId ctx) docId

  case result of
    Nothing -> throwError $ DocumentNotFound docId
    Just doc -> pure DocumentResponse
      { drId = DB.docId doc
      , drName = DB.docName doc
      , drMimeType = DB.docMimeType doc
      , drMetadata = DB.docMetadata doc
      , drCreatedAt = DB.docCreatedAt doc
      , drUpdatedAt = DB.docUpdatedAt doc
      }

-- | Upload a new document version.
--
-- POST /v1/documents/:id/versions
-- Requires: document:write
--
-- @since 0.1.0.0
uploadDocumentVersion
  :: AuthContext
  -> UUID
  -> BS.ByteString
  -> AppM CreatedResponse
uploadDocumentVersion ctx docId content = do
  requirePermission PermDocumentWrite ctx

  result <- withPool $ \conn ->
    DB.uploadVersion conn (acTenantId ctx) docId content (Just $ acActorId ctx)

  case result of
    Left err -> throwError $ InternalError $ T.pack $ show err
    Right versionId -> pure CreatedResponse
      { crId = versionId
      , crLocation = "/v1/document-versions/" <> T.pack (show versionId)
      }

-- | Get document version content.
--
-- GET /v1/document-versions/:id/content
-- Requires: document:read
--
-- @since 0.1.0.0
getDocumentVersionContent
  :: AuthContext
  -> UUID
  -> AppM BS.ByteString
getDocumentVersionContent ctx versionId = do
  requirePermission PermDocumentRead ctx

  result <- withPool $ \conn ->
    DB.getContent conn (acTenantId ctx) versionId

  case result of
    Nothing -> throwError $ NotFound "Document version content"
    Just content -> pure content

-- | List versions for a document.
--
-- GET /v1/documents/:id/versions
-- Requires: document:read
--
-- @since 0.1.0.0
listDocumentVersions
  :: AuthContext
  -> UUID
  -> AppM [DocumentVersionResponse]
listDocumentVersions ctx docId = do
  requirePermission PermDocumentRead ctx

  versions <- withPool $ \conn ->
    DB.getVersionsByDocument conn (acTenantId ctx) docId

  pure $ map toVersionResponse versions
  where
    toVersionResponse v = DocumentVersionResponse
      { dvrId = DB.dvId v
      , dvrDocumentId = DB.dvDocumentId v
      , dvrVersionNumber = DB.dvVersionNumber v
      , dvrContentHash = DB.dvContentHash v
      , dvrContentSize = DB.dvContentSize v
      , dvrStatus = statusToText $ DB.dvStatus v
      , dvrCreatedAt = DB.dvCreatedAt v
      }

    statusToText DB.DocUploaded = "UPLOADED"
    statusToText DB.DocParsed = "PARSED"
    statusToText DB.DocValidated = "VALIDATED"
    statusToText DB.DocRejected = "REJECTED"
