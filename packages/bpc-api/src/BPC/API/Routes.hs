{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | API Routes
--
-- Route definitions and WAI application composition.
--
-- @since 0.1.0.0
module BPC.API.Routes
  ( -- * Application
    app
  , routes
    -- * Route Helpers
  , Route(..)
  , Method(..)
  ) where

import Control.Exception (catch, SomeException)
import Data.Aeson (ToJSON, encode, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Network.HTTP.Types
  ( Method, methodGet, methodPost, methodPut, methodPatch, methodDelete
  , status200, status201, status400, status401, status403, status404, status500, status501
  , hContentType, Status
  )
import Network.Wai
  ( Application, Request, Response
  , responseLBS, requestMethod, pathInfo, requestBody, strictRequestBody
  )

import BPC.API.App (Env(..), AppM, runAppM)
import BPC.API.Error (AppError(..))
import BPC.API.Types (AuthContext(..), PaginationParams(..), defaultPagination)
import qualified BPC.API.Handlers.Health as Health
import qualified BPC.API.Handlers.Documents as Documents
import qualified BPC.API.Handlers.Passports as Passports
import qualified BPC.API.Handlers.Snapshots as Snapshots
import qualified BPC.API.Handlers.Facts as Facts
import qualified BPC.API.Handlers.RulePackages as Rules
import qualified BPC.API.Handlers.Audit as Audit
import qualified BPC.API.Handlers.Webhooks as Webhooks
import qualified BPC.API.Handlers.Policies as Policies
-- import qualified BPC.API.Handlers.Metrics as Metrics  -- TODO: implement

-- | Route definition.
data Route = Route
  { routeMethod :: Method
  , routePath :: Text
  , routeDescription :: Text
  , routeRequiresAuth :: Bool
  }
  deriving stock (Show, Eq)

-- | All API routes.
routes :: [Route]
routes =
  -- Health & Metrics (no auth)
  [ Route methodGet "/health/live" "Liveness probe" False
  , Route methodGet "/health/ready" "Readiness probe" False
  , Route methodGet "/metrics" "Prometheus metrics" False

  -- Documents
  , Route methodPost "/v1/documents" "Create document" True
  , Route methodGet "/v1/documents" "List documents" True
  , Route methodGet "/v1/documents/:id" "Get document" True
  , Route methodPost "/v1/documents/:id/versions" "Upload version" True
  , Route methodGet "/v1/documents/:id/versions" "List versions" True
  , Route methodGet "/v1/document-versions/:id" "Get version" True
  , Route methodGet "/v1/document-versions/:id/content" "Get content" True

  -- Facts
  , Route methodPost "/v1/facts" "Create fact" True
  , Route methodGet "/v1/facts" "List facts" True
  , Route methodGet "/v1/facts/:type/:key" "Get fact" True

  -- Snapshots
  , Route methodPost "/v1/snapshots" "Create snapshot" True
  , Route methodGet "/v1/snapshots" "List snapshots" True
  , Route methodGet "/v1/snapshots/:id" "Get snapshot" True
  , Route methodPost "/v1/snapshots/:id/items" "Add item" True
  , Route methodDelete "/v1/snapshots/:id/items/:fact_id" "Remove item" True
  , Route methodPost "/v1/snapshots/:id/seal" "Seal snapshot" True

  -- Passports
  , Route methodPost "/v1/passports" "Create passport" True
  , Route methodGet "/v1/passports" "List passports" True
  , Route methodGet "/v1/passports/:id" "Get passport" True
  , Route methodPost "/v1/passports/:id/compile" "Compile passport" True
  , Route methodGet "/v1/passports/:id/versions" "List versions" True
  , Route methodGet "/v1/passport-versions/:id" "Get version" True
  , Route methodGet "/v1/passport-versions/:id/payload" "Get payload" True
  , Route methodGet "/v1/passport-versions/:id/proof" "Get proof" True
  , Route methodGet "/v1/passport-versions/:id/receipt" "Get receipt" True
  , Route methodPost "/v1/passport-versions/:id/activate" "Activate" True
  , Route methodPost "/v1/passport-versions/:id/revoke" "Revoke" True
  , Route methodPost "/v1/passport-versions/:id/replay" "Replay verify" True

  -- Rules
  , Route methodPost "/v1/rule-packages" "Create package" True
  , Route methodGet "/v1/rule-packages" "List packages" True
  , Route methodGet "/v1/rule-packages/:id" "Get package" True
  , Route methodPost "/v1/rule-packages/:id/versions" "Create version" True
  , Route methodGet "/v1/rule-packages/:id/versions" "List versions" True
  , Route methodGet "/v1/rule-versions/:id" "Get version" True
  , Route methodPost "/v1/rule-versions/:id/test-runs" "Record test run" True
  , Route methodPost "/v1/rule-versions/:id/publish" "Publish version" True

  -- Audit
  , Route methodGet "/v1/audit/events" "List events" True
  , Route methodGet "/v1/audit/events/:id" "Get event" True
  , Route methodPost "/v1/audit/verify" "Verify chains" True

  -- Webhooks
  , Route methodPost "/v1/webhook-endpoints" "Create endpoint" True
  , Route methodGet "/v1/webhook-endpoints" "List endpoints" True
  , Route methodGet "/v1/webhook-endpoints/:id" "Get endpoint" True
  , Route methodDelete "/v1/webhook-endpoints/:id" "Deactivate" True
  , Route methodPost "/v1/webhook-endpoints/:id/subscriptions" "Subscribe" True
  , Route methodGet "/v1/webhook-endpoints/:id/subscriptions" "List subs" True
  , Route methodDelete "/v1/webhook-endpoints/:id/subscriptions/:event" "Unsubscribe" True

  -- Policies
  , Route methodPost "/v1/policies" "Create policy" True
  , Route methodGet "/v1/policies" "List policies" True
  , Route methodGet "/v1/policies/:id" "Get policy" True

  -- GraphQL
  , Route methodPost "/v1/graphql" "GraphQL endpoint" True

  -- OpenAPI
  , Route methodGet "/docs/openapi.yaml" "OpenAPI spec" False
  ]

-- | WAI Application.
--
-- Routes requests to appropriate handlers based on path and method.
--
-- @since 0.1.0.0
app :: Env -> Application
app env request respond = do
  let method = requestMethod request
  let path = pathInfo request

  -- Route the request
  response <- routeRequest env method path request
  respond response

-- | Route a request to the appropriate handler.
routeRequest :: Env -> Method -> [Text] -> Request -> IO Response
routeRequest env method path request = do
  -- For now, use a mock auth context for authenticated routes
  -- In production, this comes from the auth middleware
  let mockAuthCtx = AuthContext
        { acTenantId = UUID.nil
        , acActorId = UUID.nil
        , acPermissions = []
        }

  case (method, path) of
    -- Health endpoints (no auth)
    ("GET", ["health"]) -> runHandler env Health.healthLive
    ("GET", ["health", "live"]) -> runHandler env Health.healthLive
    ("GET", ["health", "ready"]) -> runHandler env Health.healthReady

    -- Metrics (no auth) - TODO: implement Metrics.getMetrics
    ("GET", ["metrics"]) -> pure $ jsonError status501 "NOT_IMPLEMENTED" "Metrics endpoint not implemented"

    -- Documents
    ("POST", ["v1", "documents"]) -> do
      body <- strictRequestBody request
      case Aeson.decode body of
        Nothing -> pure $ jsonError status400 "INVALID_JSON" "Invalid request body"
        Just req -> runHandler env $ Documents.createDocument mockAuthCtx req

    ("GET", ["v1", "documents"]) ->
      runHandler env $ Documents.listDocuments mockAuthCtx defaultPagination

    ("GET", ["v1", "documents", idText]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid document ID"
        Just docId -> runHandler env $ Documents.getDocument mockAuthCtx docId

    ("GET", ["v1", "documents", idText, "versions"]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid document ID"
        Just docId -> runHandler env $ Documents.listDocumentVersions mockAuthCtx docId

    ("POST", ["v1", "documents", idText, "versions"]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid document ID"
        Just docId -> do
          body <- strictRequestBody request
          runHandler env $ Documents.uploadDocumentVersion mockAuthCtx docId (LBS.toStrict body)

    ("GET", ["v1", "document-versions", idText, "content"]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid version ID"
        Just verId -> runHandlerBS env $ Documents.getDocumentVersionContent mockAuthCtx verId

    -- Passports
    ("POST", ["v1", "passports"]) -> do
      body <- strictRequestBody request
      case Aeson.decode body of
        Nothing -> pure $ jsonError status400 "INVALID_JSON" "Invalid request body"
        Just req -> runHandler env $ Passports.createPassport mockAuthCtx req

    ("GET", ["v1", "passports"]) ->
      runHandler env $ Passports.listPassports mockAuthCtx defaultPagination

    ("GET", ["v1", "passports", idText]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid passport ID"
        Just pId -> runHandler env $ Passports.getPassport mockAuthCtx pId

    ("POST", ["v1", "passports", idText, "compile"]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid passport ID"
        Just pId -> runHandler env $ Passports.compilePassport mockAuthCtx pId

    -- TODO: implement Passports.listPassportVersions
    ("GET", ["v1", "passports", _idText, "versions"]) ->
      pure $ jsonError status501 "NOT_IMPLEMENTED" "List passport versions not implemented"

    -- Snapshots
    ("POST", ["v1", "snapshots"]) -> do
      body <- strictRequestBody request
      case Aeson.decode body of
        Nothing -> pure $ jsonError status400 "INVALID_JSON" "Invalid request body"
        Just req -> runHandler env $ Snapshots.createSnapshot mockAuthCtx req

    ("GET", ["v1", "snapshots"]) ->
      runHandler env $ Snapshots.listSnapshots mockAuthCtx defaultPagination

    ("GET", ["v1", "snapshots", idText]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid snapshot ID"
        Just sId -> runHandler env $ Snapshots.getSnapshot mockAuthCtx sId

    ("POST", ["v1", "snapshots", idText, "seal"]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid snapshot ID"
        Just sId -> runHandler env $ Snapshots.sealSnapshot mockAuthCtx sId

    -- Facts
    ("POST", ["v1", "facts"]) -> do
      body <- strictRequestBody request
      case Aeson.decode body of
        Nothing -> pure $ jsonError status400 "INVALID_JSON" "Invalid request body"
        Just req -> runHandler env $ Facts.createFact mockAuthCtx req

    ("GET", ["v1", "facts"]) ->
      runHandler env $ Facts.listFacts mockAuthCtx defaultPagination

    ("GET", ["v1", "facts", factType, factKey]) ->
      runHandler env $ Facts.getFact mockAuthCtx factType factKey

    -- Rules
    ("POST", ["v1", "rule-packages"]) -> do
      body <- strictRequestBody request
      case Aeson.decode body of
        Nothing -> pure $ jsonError status400 "INVALID_JSON" "Invalid request body"
        Just req -> runHandler env $ Rules.createRulePackage mockAuthCtx req

    ("GET", ["v1", "rule-packages"]) ->
      runHandler env $ Rules.listRulePackages mockAuthCtx defaultPagination

    ("GET", ["v1", "rule-packages", idText]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid rule package ID"
        Just rId -> runHandler env $ Rules.getRulePackage mockAuthCtx rId

    ("POST", ["v1", "rule-versions", idText, "publish"]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid rule version ID"
        Just rId -> runHandler env $ Rules.publishRuleVersion mockAuthCtx rId

    -- Audit - TODO: listAuditEvents requires aggregateType and aggregateId
    ("GET", ["v1", "audit", "events"]) ->
      pure $ jsonError status501 "NOT_IMPLEMENTED" "List events requires aggregate_type and aggregate_id query params"

    ("GET", ["v1", "audit", "events", idText]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid event ID"
        Just eId -> runHandler env $ Audit.getAuditEvent mockAuthCtx eId

    -- Webhooks - TODO: createWebhookEndpoint takes url, secret, name, not a request body
    ("POST", ["v1", "webhook-endpoints"]) ->
      pure $ jsonError status501 "NOT_IMPLEMENTED" "Create webhook endpoint requires url, secret, name"

    ("GET", ["v1", "webhook-endpoints"]) ->
      runHandler env $ Webhooks.listWebhookEndpoints mockAuthCtx

    ("GET", ["v1", "webhook-endpoints", idText]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid endpoint ID"
        Just eId -> runHandler env $ Webhooks.getWebhookEndpoint mockAuthCtx eId

    ("DELETE", ["v1", "webhook-endpoints", idText]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid endpoint ID"
        Just eId -> runHandler env $ Webhooks.deactivateWebhookEndpoint mockAuthCtx eId

    -- Policies
    ("POST", ["v1", "policies"]) -> do
      body <- strictRequestBody request
      case Aeson.decode body of
        Nothing -> pure $ jsonError status400 "INVALID_JSON" "Invalid request body"
        Just req -> runHandler env $ Policies.createPolicy mockAuthCtx req

    ("GET", ["v1", "policies"]) ->
      runHandler env $ Policies.listPolicies mockAuthCtx defaultPagination

    ("GET", ["v1", "policies", idText]) ->
      case UUID.fromText idText of
        Nothing -> pure $ jsonError status400 "INVALID_UUID" "Invalid policy ID"
        Just pId -> runHandler env $ Policies.getPolicy mockAuthCtx pId

    -- 404 for unmatched routes
    _ -> pure $ jsonError status404 "NOT_FOUND" "Route not found"

-- | Run a handler that returns JSON.
runHandler :: ToJSON a => Env -> AppM a -> IO Response
runHandler env action = do
  result <- runAppM env action
  case result of
    Left err -> pure $ errorToResponse err
    Right val -> pure $ responseLBS status200
      [(hContentType, "application/json")]
      (encode val)

-- | Run a handler that returns raw bytes.
runHandlerBS :: Env -> AppM BS.ByteString -> IO Response
runHandlerBS env action = do
  result <- runAppM env action
  case result of
    Left err -> pure $ errorToResponse err
    Right bs -> pure $ responseLBS status200
      [(hContentType, "application/octet-stream")]
      (LBS.fromStrict bs)

-- | Convert AppError to HTTP response.
errorToResponse :: AppError -> Response
errorToResponse = \case
  NotFound msg -> jsonError status404 "NOT_FOUND" msg
  DocumentNotFound _ -> jsonError status404 "DOCUMENT_NOT_FOUND" "Document not found"
  SnapshotNotFound _ -> jsonError status404 "SNAPSHOT_NOT_FOUND" "Snapshot not found"
  PassportNotFound _ -> jsonError status404 "PASSPORT_NOT_FOUND" "Passport not found"
  RulePackageNotFound _ -> jsonError status404 "RULE_PACKAGE_NOT_FOUND" "Rule package not found"
  ApiKeyInvalid -> jsonError status401 "API_KEY_INVALID" "Invalid API key"
  ApiKeyExpired -> jsonError status401 "API_KEY_EXPIRED" "API key has expired"
  PermissionDenied resource -> jsonError status403 "PERMISSION_DENIED" $
    "Permission denied for resource: " <> resource
  ValidationError fields -> jsonError status400 "VALIDATION_ERROR" $
    T.intercalate ", " [f <> ": " <> m | (f, m) <- fields]
  Conflict msg -> jsonError status400 "CONFLICT" msg
  InternalError msg -> jsonError status500 "INTERNAL_ERROR" msg

-- | Create a JSON error response.
jsonError :: Status -> Text -> Text -> Response
jsonError status code message = responseLBS status
  [(hContentType, "application/json")]
  (encode $ object
    [ "error" .= object
        [ "code" .= code
        , "message" .= message
        ]
    ])
