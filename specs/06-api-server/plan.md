# Implementation Plan: API Server

**Branch**: `06-api-server` | **Date**: 2025-12-28 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `.specify/features/06-api-server/spec.md`
**Phase**: P2-P3 | **Package**: bpc-api | **Status**: Planning

## Summary

Implement the HTTP API server with authentication, RBAC authorization, all REST endpoints (30+), cursor pagination, idempotency, correlation IDs, rate limiting, and OpenAPI/GraphQL support.

## Technical Context

**Language/Version**: Haskell GHC 9.6.4
**Package**: bpc-api
**Primary Dependencies**: warp, wai, servant, aeson
**Storage**: Uses bpc-db repositories
**Testing**: hspec-wai, tasty-hunit
**Performance Goals**: P95 < 100ms for GET requests
**Constraints**: All endpoints tenant-scoped via API key
**Scale/Scope**: ~3000 LOC, 30+ endpoints

## Constitution Check

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Determinism | N/A | HTTP is inherently side-effectful |
| II. Canonical Storage | ENFORCED | Return via canonicalDecode, not reconstruction |
| III. Immutability | ENFORCED | SEALED/PUBLISHED can't be modified |
| IV. Audit Trail | ENFORCED | All writes generate events |
| V. Layered Architecture | ENFORCED | bpc-api imports core+db, not worker |
| VI. Type-Safe Rules | N/A | Rules handled by core |

## Project Structure

```text
packages/bpc-api/
├── bpc-api.cabal
├── src/
│   └── BPC/
│       └── API/
│           ├── Main.hs                  # Warp server start
│           ├── App.hs                   # AppM monad, Env
│           ├── Routes.hs                # Route table
│           ├── Types.hs                 # Request/Response types
│           ├── Error.hs                 # ErrorEnvelope
│           ├── Middleware/
│           │   ├── Auth.hs              # API Key auth + RBAC
│           │   ├── RateLimit.hs         # Token bucket
│           │   ├── CorrelationId.hs     # X-Correlation-Id
│           │   └── Idempotency.hs       # Idempotency-Key
│           ├── Handlers/
│           │   ├── Health.hs            # /health/live, /health/ready
│           │   ├── Metrics.hs           # /metrics
│           │   ├── Documents.hs
│           │   ├── Facts.hs
│           │   ├── Snapshots.hs
│           │   ├── RulePackages.hs
│           │   ├── Passports.hs
│           │   ├── Audit.hs
│           │   ├── Webhooks.hs
│           │   └── GraphQL.hs           # MVP GraphQL
│           ├── OpenAPI.hs               # /docs/openapi.yaml
│           └── GraphQL.hs               # GraphQL schema
└── test/
    ├── Main.hs
    └── BPC/
        └── API/
            ├── AuthSpec.hs
            ├── HandlersSpec.hs
            └── IntegrationSpec.hs
```

## Implementation Phases

### Phase 1: App Monad & Server Setup

```haskell
module BPC.API.App where

-- Environment
data Env = Env
  { envPool        :: Pool Connection
  , envConfig      :: AppConfig
  , envLogger      :: Logger
  , envCorrelation :: IORef (Maybe CorrelationId)
  }

-- App monad
newtype AppM a = AppM { unAppM :: ReaderT Env (ExceptT AppError IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO,
                    MonadReader Env, MonadError AppError)

-- Run handler
runAppM :: Env -> AppM a -> IO (Either AppError a)
runAppM env = runExceptT . flip runReaderT env . unAppM
```

### Phase 2: Authentication Middleware

```haskell
module BPC.API.Middleware.Auth where

-- Auth context extracted from API key
data AuthContext = AuthContext
  { acTenantId    :: TenantId
  , acActorId     :: ActorId
  , acPermissions :: Set Permission
  }

-- Authenticate request
authenticate :: Request -> AppM AuthContext
authenticate req = do
  -- 1. Extract Authorization header
  apiKey <- extractBearerToken req

  -- 2. Compute hash: SHA-256(key + pepper)
  keyHash <- hashApiKey apiKey

  -- 3. Lookup in DB
  mActor <- withConn $ \conn -> getActorByApiKey conn keyHash
  actor <- maybe (throwError UNAUTHORIZED) pure mActor

  -- 4. Check not revoked
  when (isRevoked actor) $ throwError API_KEY_REVOKED

  -- 5. Load permissions
  permissions <- withConn $ \conn -> getPermissions conn (actorTenantId actor) (actorId actor)

  pure AuthContext{..}

-- Check permission
requirePermission :: Permission -> AuthContext -> AppM ()
requirePermission perm ctx
  | perm `Set.member` acPermissions ctx = pure ()
  | otherwise = throwError $ FORBIDDEN perm
```

### Phase 3: Error Envelope

```haskell
module BPC.API.Error where

-- Error envelope (SSOT 10.5)
data ErrorEnvelope = ErrorEnvelope
  { eeCode          :: Text
  , eeMessage       :: Text
  , eeCorrelationId :: UUID
  , eeDetails       :: Maybe Value
  }

instance ToJSON ErrorEnvelope where
  toJSON ee = object
    [ "error" .= object
        [ "code" .= eeCode ee
        , "message" .= eeMessage ee
        , "correlation_id" .= eeCorrelationId ee
        , "details" .= eeDetails ee
        ]
    ]

-- Convert AppError to HTTP response
toErrorResponse :: AppError -> CorrelationId -> Response
toErrorResponse err cid = responseLBS status [(hContentType, "application/json")] body
  where
    status = appErrorStatus err
    body = encode $ ErrorEnvelope (appErrorCode err) (appErrorMessage err) cid Nothing
```

### Phase 4: Pagination

```haskell
module BPC.API.Types where

-- Pagination query params
data PaginationParams = PaginationParams
  { ppLimit  :: Int           -- 1-200, default 50
  , ppCursor :: Maybe Text    -- Opaque base64 cursor
  }

-- Parse cursor
parseCursor :: Text -> Either Text Cursor
parseCursor = fmap decode . Base64.decode

-- Pagination response
data CursorPage a = CursorPage
  { cpItems      :: [a]
  , cpNextCursor :: Maybe Text
  }
```

### Phase 5: Idempotency Middleware

```haskell
module BPC.API.Middleware.Idempotency where

-- Check/store idempotency key
withIdempotency :: Text -> AppM Response -> AppM Response
withIdempotency key action = do
  tenantId <- asks (acTenantId . envAuth)
  requestHash <- computeRequestHash

  -- Check for existing
  existing <- withConn $ \conn -> lookupIdempotencyKey conn tenantId key

  case existing of
    Just stored
      | storedRequestHash stored == requestHash ->
          -- Replay stored response
          pure $ replayResponse stored
      | otherwise ->
          -- Different request with same key
          throwError IDEMPOTENCY_CONFLICT

    Nothing -> do
      -- Execute and store
      response <- action
      withConn $ \conn ->
        storeIdempotencyKey conn tenantId key requestHash response
      pure response
```

### Phase 6: Handlers

```haskell
module BPC.API.Handlers.Documents where

-- POST /v1/documents
createDocument :: AuthContext -> CreateDocumentRequest -> AppM CreateDocumentResponse
createDocument ctx req = do
  requirePermission "docs:upload" ctx
  docId <- withConn $ \conn ->
    Repos.createDocument conn (acTenantId ctx) (fromRequest req)
  pure $ CreateDocumentResponse docId

-- GET /v1/documents
listDocuments :: AuthContext -> PaginationParams -> AppM (CursorPage DocumentSummary)
listDocuments ctx params = do
  requirePermission "docs:read" ctx
  page <- withConn $ \conn ->
    Repos.listDocuments conn (acTenantId ctx) (toListOptions params)
  pure $ toCursorPage page

-- POST /v1/documents/{id}/versions
uploadDocumentVersion :: AuthContext -> DocumentId -> UploadRequest -> AppM UploadResponse
-- ...
```

### Phase 7: Correlation ID

```haskell
module BPC.API.Middleware.CorrelationId where

correlationMiddleware :: Middleware
correlationMiddleware app req respond = do
  cid <- case lookup "X-Correlation-Id" (requestHeaders req) of
    Just h  -> pure $ CorrelationId (decodeUtf8 h)
    Nothing -> CorrelationId <$> UUID.nextRandom

  let req' = req { requestHeaders = ("X-Correlation-Id", encodeUtf8 $ unCorrelationId cid) : requestHeaders req }

  app req' $ \response ->
    respond $ mapResponseHeaders (("X-Correlation-Id", encodeUtf8 $ unCorrelationId cid) :) response
```

### Phase 8: OpenAPI

```haskell
module BPC.API.OpenAPI where

-- Serve OpenAPI spec
openApiHandler :: AppM Response
openApiHandler = do
  spec <- liftIO $ BSL.readFile "docs/openapi.yaml"
  pure $ responseLBS status200 [(hContentType, "application/yaml")] spec
```

### Phase 9: GraphQL (MVP)

```haskell
module BPC.API.GraphQL where

-- Minimal GraphQL for passport queries
graphqlHandler :: AuthContext -> GraphQLRequest -> AppM GraphQLResponse
graphqlHandler ctx req = do
  requirePermission "passport:read" ctx
  result <- execute schema (acTenantId ctx) (grQuery req)
  pure $ GraphQLResponse result
```

## Key Endpoints (SSOT 10.4)

| Method | Path | Permission | Handler |
|--------|------|------------|---------|
| GET | /v1/health/live | - | healthLive |
| GET | /v1/health/ready | - | healthReady |
| GET | /v1/metrics | - | metrics |
| POST | /v1/documents | docs:upload | createDocument |
| GET | /v1/documents | docs:read | listDocuments |
| POST | /v1/snapshots | snapshot:build | createSnapshot |
| POST | /v1/snapshots/{id}/seal | snapshot:seal | sealSnapshot |
| POST | /v1/passports/{id}/compile | passport:compile | compilePassport |
| POST | /v1/passport-versions/{id}/activate | passport:compile | activateVersion |
| POST | /v1/passport-versions/{id}/replay | passport:replay | replayVersion |
| GET | /v1/audit/events | audit:read | listEvents |

## Metrics (SSOT 11.2)

```haskell
-- Prometheus metrics
data Metrics = Metrics
  { mHttpRequestsTotal        :: Counter
  , mHttpRequestDuration      :: Histogram
  , mDbPoolInUse              :: Gauge
  , mRateLimitedTotal         :: Counter
  }

metricsMiddleware :: Metrics -> Middleware
```

## Verification Checklist

- [ ] All 30+ endpoints implemented
- [ ] Auth validates API key and permissions
- [ ] RBAC covers all 16 permissions
- [ ] Idempotency works for all mutating endpoints
- [ ] Correlation ID in all responses and logs
- [ ] Error responses use ErrorEnvelope
- [ ] Pagination uses cursor with 1-200 limit
- [ ] OpenAPI spec at /docs/openapi.yaml
- [ ] Health endpoints work without auth
- [ ] Metrics in Prometheus format
- [ ] Integration tests pass
- [ ] Code coverage >= 70%
