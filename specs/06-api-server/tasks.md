# Tasks: API Server

**Input**: Design documents from `specs/06-api-server/`
**Prerequisites**: plan.md (required), spec.md (required), contracts/
**Phase**: P2-P3 - HTTP API, depends on 05-data-layer
**Package**: bpc-api (WAI/Warp, Servant)

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US12)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `packages/bpc-api/src/BPC/API/`
- **Tests**: `packages/bpc-api/test/BPC/API/`
- **OpenAPI**: `docs/openapi.yaml`

---

## Phase 1: Setup (Package Configuration)

**Purpose**: Configure bpc-api package with required dependencies

- [x] T001 Create `packages/bpc-api/bpc-api.cabal` with warp, wai, servant, aeson dependencies
- [x] T002 Add bpc-core, bpc-db internal dependencies to bpc-api.cabal
- [x] T003 Create directory structure: `packages/bpc-api/src/BPC/API/Middleware/`
- [x] T004 Create directory structure: `packages/bpc-api/src/BPC/API/Handlers/`
- [x] T005 [P] Create directory structure: `packages/bpc-api/test/BPC/API/`
- [x] T006 Add hspec-wai, tasty-hunit test dependencies to bpc-api.cabal
- [x] T007 Create `packages/bpc-api/src/BPC/API.hs` re-export module

**Checkpoint**: `cabal build bpc-api` succeeds with dependencies

---

## Phase 2: Foundational (App Monad & Error Types)

**Purpose**: Define app monad, environment, and error types - BLOCKS all user stories

- [x] T008 Create `packages/bpc-api/src/BPC/API/App.hs` with Env data type
- [x] T009 Define AppM monad (ReaderT Env (ExceptT AppError IO)) in App.hs
- [x] T010 Implement runAppM :: Env -> AppM a -> IO (Either AppError a) in App.hs
- [x] T011 Create `packages/bpc-api/src/BPC/API/Error.hs` with AppError sum type
- [x] T012 Add all HTTP error variants (UNAUTHORIZED, FORBIDDEN, NOT_FOUND, etc.) to Error.hs
- [x] T013 Create `packages/bpc-api/src/BPC/API/Types.hs` with request/response types
- [x] T014 Create `packages/bpc-api/src/BPC/API/Main.hs` with server startup skeleton

**Checkpoint**: App monad compiles; server starts on port

---

## Phase 3: User Story 1 - API Key Authentication (Priority: P1) MVP

**Goal**: Authenticate requests using API key in Authorization header

**Independent Test**: Request with valid/invalid API key returns appropriate response

### Tests for User Story 1

- [x] T015 [P] [US1] Create `packages/bpc-api/test/BPC/API/AuthSpec.hs` with test scaffold
- [x] T016 [US1] Add test: Missing Authorization header → 401 UNAUTHORIZED in AuthSpec.hs
- [x] T017 [US1] Add test: Invalid API key → 401 UNAUTHORIZED in AuthSpec.hs
- [x] T018 [US1] Add test: Revoked API key → 401 API_KEY_REVOKED in AuthSpec.hs
- [x] T019 [US1] Add test: Valid API key → AuthContext created in AuthSpec.hs

### Implementation for User Story 1

- [x] T020 [US1] Create `packages/bpc-api/src/BPC/API/Middleware/Auth.hs` module
- [x] T021 [US1] Define AuthContext data type (TenantId, ActorId, Permissions) in Auth.hs
- [x] T022 [US1] Implement extractBearerToken :: Request -> Maybe Text in Auth.hs
- [x] T023 [US1] Implement hashApiKey :: Text -> Text (SHA-256 + pepper) in Auth.hs
- [x] T024 [US1] Implement authenticate :: Request -> AppM AuthContext in Auth.hs
- [x] T025 [US1] Implement authMiddleware :: Middleware in Auth.hs
- [x] T026 [US1] Export authMiddleware from BPC.API module

**Checkpoint**: Auth middleware validates API keys; 401 on invalid

---

## Phase 4: User Story 2 - RBAC Authorization (Priority: P1)

**Goal**: Check permissions before executing handlers

**Independent Test**: Actor without permission → 403 FORBIDDEN

### Tests for User Story 2

- [x] T027 [P] [US2] Add RBAC tests to AuthSpec.hs
- [x] T028 [US2] Add test: Missing permission → 403 FORBIDDEN in AuthSpec.hs
- [x] T029 [US2] Add test: Has permission → handler executes in AuthSpec.hs
- [x] T030 [US2] Add test: Admin role has all permissions in AuthSpec.hs
- [x] T031 [US2] Add test: Auditor role limited to read-only in AuthSpec.hs

### Implementation for User Story 2

- [x] T032 [US2] Define Permission type with all 16 permissions in Auth.hs
- [x] T033 [US2] Implement requirePermission :: Permission -> AuthContext -> AppM () in Auth.hs
- [x] T034 [US2] Implement loadActorPermissions in authenticate in Auth.hs
- [x] T035 [US2] Export requirePermission from BPC.API module

**Checkpoint**: RBAC enforced; all 16 permissions covered

---

## Phase 5: User Story 3 - Correlation ID Tracking (Priority: P1)

**Goal**: Track requests across logs with X-Correlation-Id header

**Independent Test**: Response contains X-Correlation-Id; logs include it

### Tests for User Story 3

- [x] T036 [P] [US3] Create `packages/bpc-api/test/BPC/API/MiddlewareSpec.hs` with scaffold
- [x] T037 [US3] Add test: Missing header → UUID generated in MiddlewareSpec.hs
- [x] T038 [US3] Add test: Header present → same ID in response in MiddlewareSpec.hs
- [x] T039 [US3] Add test: Error response includes correlation_id in MiddlewareSpec.hs

### Implementation for User Story 3

- [x] T040 [US3] Create `packages/bpc-api/src/BPC/API/Middleware/CorrelationId.hs` module
- [x] T041 [US3] Define CorrelationId newtype in CorrelationId.hs
- [x] T042 [US3] Implement correlationMiddleware :: Middleware in CorrelationId.hs
- [x] T043 [US3] Add correlation_id to ErrorEnvelope in Error.hs
- [x] T044 [US3] Export correlationMiddleware from BPC.API module

**Checkpoint**: All responses have X-Correlation-Id header

---

## Phase 6: User Story 4 - Cursor Pagination (Priority: P1)

**Goal**: Paginate list responses using cursor-based pagination

**Independent Test**: Paginate 100 items with limit=10 → 10 pages

### Tests for User Story 4

- [x] T045 [P] [US4] Add pagination tests to MiddlewareSpec.hs
- [x] T046 [US4] Add test: GET with limit=10 → 10 items + next_cursor in MiddlewareSpec.hs
- [x] T047 [US4] Add test: GET with cursor → next page in MiddlewareSpec.hs
- [x] T048 [US4] Add test: Last page → next_cursor is null in MiddlewareSpec.hs
- [x] T049 [US4] Add test: limit > 200 → clamped to 200 in MiddlewareSpec.hs

### Implementation for User Story 4

- [x] T050 [US4] Define PaginationParams data type (limit 1-200, cursor) in Types.hs
- [x] T051 [US4] Define Cursor data type (timestamp + id, base64 encoded) in Types.hs
- [x] T052 [US4] Implement parseCursor :: Text -> Either Text Cursor in Types.hs
- [x] T053 [US4] Implement encodeCursor :: Cursor -> Text in Types.hs
- [x] T054 [US4] Define CursorPage a response type in Types.hs
- [x] T055 [US4] Export pagination types from BPC.API.Types module

**Checkpoint**: Cursor pagination works with 1-200 limit

---

## Phase 7: User Story 5 - Idempotency (Priority: P1)

**Goal**: Deduplicate mutating requests using Idempotency-Key header

**Independent Test**: Same request twice → same response, no duplicate

### Tests for User Story 5

- [x] T056 [P] [US5] Add idempotency tests to MiddlewareSpec.hs
- [x] T057 [US5] Add test: First request → executed and stored in MiddlewareSpec.hs
- [x] T058 [US5] Add test: Duplicate request → replay stored response in MiddlewareSpec.hs
- [x] T059 [US5] Add test: Different request same key → 409 IDEMPOTENCY_CONFLICT in MiddlewareSpec.hs
- [x] T060 [US5] Add test: Missing Idempotency-Key → 422 VALIDATION_ERROR in MiddlewareSpec.hs

### Implementation for User Story 5

- [x] T061 [US5] Create `packages/bpc-api/src/BPC/API/Middleware/Idempotency.hs` module
- [x] T062 [US5] Implement computeRequestHash :: Request -> AppM Text in Idempotency.hs
- [x] T063 [US5] Implement withIdempotency :: Text -> AppM Response -> AppM Response in Idempotency.hs
- [x] T064 [US5] Implement lookupIdempotencyKey and storeIdempotencyKey using bpc-db in Idempotency.hs
- [x] T065 [US5] Add idempotencyMiddleware :: Middleware in Idempotency.hs
- [x] T066 [US5] Export idempotencyMiddleware from BPC.API module

**Checkpoint**: Idempotency works for POST/PUT/PATCH/DELETE

---

## Phase 8: User Story 6 - Error Envelope Format (Priority: P1)

**Goal**: Return structured error responses per SSOT 10.5

**Independent Test**: All errors return ErrorEnvelope format

### Tests for User Story 6

- [x] T067 [P] [US6] Add error response tests to MiddlewareSpec.hs
- [x] T068 [US6] Add test: Error has {error: {code, message, correlation_id}} in MiddlewareSpec.hs
- [x] T069 [US6] Add test: Validation error has details object in MiddlewareSpec.hs
- [x] T070 [US6] Add test: 5xx error hides internal details in MiddlewareSpec.hs

### Implementation for User Story 6

- [x] T071 [US6] Define ErrorEnvelope data type (code, message, correlation_id, details) in Error.hs
- [x] T072 [US6] Implement ToJSON instance for ErrorEnvelope in Error.hs
- [x] T073 [US6] Implement toErrorResponse :: AppError -> CorrelationId -> Response in Error.hs
- [x] T074 [US6] Implement appErrorStatus :: AppError -> Status in Error.hs
- [x] T075 [US6] Implement errorMiddleware :: Middleware to catch exceptions in Error.hs

**Checkpoint**: All errors return consistent ErrorEnvelope

---

## Phase 9: User Story 7 - Document Endpoints (Priority: P1)

**Goal**: Implement document upload and retrieval endpoints

**Independent Test**: Create document, upload version, get content

### Tests for User Story 7

- [x] T076 [P] [US7] Create `packages/bpc-api/test/BPC/API/HandlersSpec.hs` with scaffold
- [x] T077 [US7] Add test: POST /documents → 201 with document_id in HandlersSpec.hs
- [x] T078 [US7] Add test: POST /documents/{id}/versions → 201 with sha256 in HandlersSpec.hs
- [x] T079 [US7] Add test: GET /document-versions/{id}/content → original bytes in HandlersSpec.hs
- [x] T080 [US7] Add test: GET non-existent → 404 NOT_FOUND in HandlersSpec.hs

### Implementation for User Story 7

- [x] T081 [US7] Create `packages/bpc-api/src/BPC/API/Handlers/Documents.hs` module
- [x] T082 [US7] Implement createDocument handler in Documents.hs
- [x] T083 [US7] Implement listDocuments handler with pagination in Documents.hs
- [x] T084 [US7] Implement getDocument handler in Documents.hs
- [x] T085 [US7] Implement uploadDocumentVersion handler in Documents.hs
- [x] T086 [US7] Implement getDocumentVersionContent handler in Documents.hs
- [x] T087 [US7] Register document routes in Routes.hs

**Checkpoint**: Document CRUD works with proper auth

---

## Phase 10: User Story 8 - Snapshot Endpoints (Priority: P1)

**Goal**: Implement snapshot creation, items, and sealing

**Independent Test**: Create snapshot, add items, seal

### Tests for User Story 8

- [x] T088 [P] [US8] Add snapshot tests to HandlersSpec.hs
- [x] T089 [US8] Add test: POST /snapshots → 201 with status BUILDING in HandlersSpec.hs
- [x] T090 [US8] Add test: POST /snapshots/{id}/items → item added in HandlersSpec.hs
- [x] T091 [US8] Add test: POST /snapshots/{id}/seal → SEALED with hash in HandlersSpec.hs
- [x] T092 [US8] Add test: Add to SEALED → 409 SNAPSHOT_SEALED in HandlersSpec.hs

### Implementation for User Story 8

- [x] T093 [US8] Create `packages/bpc-api/src/BPC/API/Handlers/Snapshots.hs` module
- [x] T094 [US8] Implement createSnapshot handler in Snapshots.hs
- [x] T095 [US8] Implement listSnapshots handler with pagination in Snapshots.hs
- [x] T096 [US8] Implement getSnapshot handler in Snapshots.hs
- [x] T097 [US8] Implement addSnapshotItem handler in Snapshots.hs
- [x] T098 [US8] Implement sealSnapshot handler in Snapshots.hs
- [x] T099 [US8] Register snapshot routes in Routes.hs

**Checkpoint**: Snapshot lifecycle works

---

## Phase 11: User Story 9 - Passport Endpoints (Priority: P1)

**Goal**: Implement passport creation, compilation, and activation

**Independent Test**: Create passport, compile, activate

### Tests for User Story 9

- [x] T100 [P] [US9] Add passport tests to HandlersSpec.hs
- [x] T101 [US9] Add test: POST /passports → 201 with passport_id in HandlersSpec.hs
- [x] T102 [US9] Add test: POST /passports/{id}/compile → 202 Accepted in HandlersSpec.hs
- [x] T103 [US9] Add test: GET /passport-versions/{id}/payload → canonical JSON in HandlersSpec.hs
- [x] T104 [US9] Add test: POST /passport-versions/{id}/activate → status ACTIVE in HandlersSpec.hs

### Implementation for User Story 9

- [x] T105 [US9] Create `packages/bpc-api/src/BPC/API/Handlers/Passports.hs` module
- [x] T106 [US9] Implement createPassport handler in Passports.hs
- [x] T107 [US9] Implement listPassports handler with pagination in Passports.hs
- [x] T108 [US9] Implement getPassport handler in Passports.hs
- [x] T109 [US9] Implement compilePassport handler (enqueues job) in Passports.hs
- [x] T110 [US9] Implement getPassportVersionPayload handler in Passports.hs
- [x] T111 [US9] Implement getPassportVersionProof handler in Passports.hs
- [x] T112 [US9] Implement getPassportVersionReceipt handler in Passports.hs
- [x] T113 [US9] Implement activatePassportVersion handler in Passports.hs
- [x] T114 [US9] Register passport routes in Routes.hs

**Checkpoint**: Passport endpoints work with job queue

---

## Phase 12: User Story 10 - Replay Endpoint (Priority: P2)

**Goal**: Verify passport version via replay algorithm

**Independent Test**: Replay valid version → verification passed

### Tests for User Story 10

- [x] T115 [P] [US10] Add replay tests to HandlersSpec.hs
- [x] T116 [US10] Add test: POST /passport-versions/{id}/replay → verification=passed in HandlersSpec.hs
- [x] T117 [US10] Add test: Replay with tampered hashes → 409 REPLAY_MISMATCH in HandlersSpec.hs
- [x] T118 [US10] Add test: Replay COMPILING version → error in HandlersSpec.hs

### Implementation for User Story 10

- [x] T119 [US10] Implement replayPassportVersion handler in Passports.hs
- [x] T120 [US10] Implement replay algorithm per BPC-REPLAY-1 in Passports.hs
- [x] T121 [US10] Add ReplayResult data type (passed, mismatches) in Types.hs
- [x] T122 [US10] Register replay route in Routes.hs

**Checkpoint**: Replay verifies passport integrity

---

## Phase 13: User Story 11 - Health & Metrics (Priority: P2)

**Goal**: Provide liveness, readiness, and Prometheus metrics

**Independent Test**: Health endpoints return correct status

### Tests for User Story 11

- [x] T123 [P] [US11] Add health tests to HandlersSpec.hs
- [x] T124 [US11] Add test: GET /health/live → 200 {status: "OK"} in HandlersSpec.hs
- [x] T125 [US11] Add test: GET /health/ready with DB up → 200 in HandlersSpec.hs
- [x] T126 [US11] Add test: GET /health/ready with DB down → 503 in HandlersSpec.hs
- [x] T127 [US11] Add test: GET /metrics → Prometheus format in HandlersSpec.hs

### Implementation for User Story 11

- [x] T128 [US11] Create `packages/bpc-api/src/BPC/API/Handlers/Health.hs` module
- [x] T129 [US11] Implement healthLive handler in Health.hs
- [x] T130 [US11] Implement healthReady handler (DB check) in Health.hs
- [x] T131 [US11] Create `packages/bpc-api/src/BPC/API/Handlers/Metrics.hs` module
- [x] T132 [US11] Define Metrics data type (counters, histograms, gauges) in Metrics.hs
- [x] T133 [US11] Implement metricsHandler (Prometheus format) in Metrics.hs
- [x] T134 [US11] Implement metricsMiddleware to record requests in Metrics.hs
- [x] T135 [US11] Register health and metrics routes (no auth) in Routes.hs

**Checkpoint**: Health endpoints work; metrics in Prometheus format

---

## Phase 14: User Story 12 - GraphQL (MVP) (Priority: P3)

**Goal**: Provide GraphQL endpoint for passport queries

**Independent Test**: GraphQL query for passports returns data

### Tests for User Story 12

- [x] T136 [P] [US12] Add GraphQL tests to HandlersSpec.hs
- [x] T137 [US12] Add test: POST /graphql with query → data returned in HandlersSpec.hs
- [x] T138 [US12] Add test: Query without permission → partial data in HandlersSpec.hs

### Implementation for User Story 12

- [x] T139 [US12] Create `packages/bpc-api/src/BPC/API/GraphQL.hs` module
- [x] T140 [US12] Define GraphQL schema for passports in GraphQL.hs
- [x] T141 [US12] Implement graphqlHandler in GraphQL.hs
- [x] T142 [US12] Register /v1/graphql route in Routes.hs

**Checkpoint**: GraphQL MVP queries work

---

## Phase 15: Additional Handlers

**Purpose**: Implement remaining handlers (facts, rules, audit, webhooks)

- [x] T143 [P] Create `packages/bpc-api/src/BPC/API/Handlers/Facts.hs` with CRUD handlers
- [x] T144 [P] Create `packages/bpc-api/src/BPC/API/Handlers/RulePackages.hs` with lifecycle handlers
- [x] T145 [P] Create `packages/bpc-api/src/BPC/API/Handlers/Audit.hs` with event listing
- [x] T146 [P] Create `packages/bpc-api/src/BPC/API/Handlers/Webhooks.hs` with subscription handlers
- [x] T147 Register all remaining routes in Routes.hs

---

## Phase 16: OpenAPI Spec

**Purpose**: Generate and serve OpenAPI specification

- [x] T148 Create `docs/openapi.yaml` with all endpoints per SSOT 10.4
- [x] T149 Create `packages/bpc-api/src/BPC/API/OpenAPI.hs` module
- [x] T150 Implement openApiHandler to serve spec in OpenAPI.hs
- [x] T151 Register /docs/openapi.yaml route in Routes.hs
- [x] T152 Validate OpenAPI spec with openapi-validator

---

## Phase 17: Integration Tests

**Purpose**: End-to-end API tests with real database

- [x] T153 [P] Create `packages/bpc-api/test/BPC/API/IntegrationSpec.hs` with scaffold
- [x] T154 Add integration test: Full document → snapshot → compile flow
- [x] T155 Add integration test: Auth + RBAC end-to-end
- [x] T156 Add integration test: Idempotency end-to-end
- [x] T157 Add integration test: Pagination end-to-end

---

## Phase 18: Polish & Cross-Cutting Concerns

**Purpose**: Integration, documentation, and final verification

- [x] T158 [P] Add Haddock comments to all public functions
- [x] T159 Create `packages/bpc-api/src/BPC/API/Routes.hs` with complete route table
- [x] T160 Verify no imports of BPC.Worker.* (layered architecture)
- [x] T161 Run full test suite: `cabal test bpc-api`
- [x] T162 Verify coverage >= 70%: generate HPC report
- [x] T163 Run fourmolu check on all bpc-api source
- [x] T164 Run hlint check on all bpc-api source
- [x] T165 Update quickstart.md with API usage examples
- [x] T166 Performance test: P95 < 100ms for GET requests

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: Depends on 05-data-layer completion
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **US1 (Auth)**: Foundational
- **US2 (RBAC)**: Depends on US1
- **US3-US6**: Can proceed in parallel after Auth
- **US7-US9**: Depend on middleware (Auth, Idempotency, etc.)
- **US10-US12**: After core endpoints
- **OpenAPI, Integration**: After all handlers

### Parallel Opportunities

**Phase 9-11**: Document, Snapshot, Passport handlers can proceed in parallel
**Phase 15**: Additional handlers (Facts, Rules, Audit, Webhooks) can all proceed in parallel

---

## Implementation Strategy

### MVP First (Auth + Core Endpoints)

1. Complete Phase 1-2: Setup + Foundational
2. Complete US1-US6: Middleware (Auth, RBAC, Correlation, Pagination, Idempotency, Errors)
3. Complete US7-US9: Core endpoints (Documents, Snapshots, Passports)
4. **STOP and VALIDATE**: API serves core pipeline

### Incremental Delivery

1. Setup + Foundational → Server starts
2. US1-US2 (Auth + RBAC) → Protected endpoints
3. US3-US6 (Middleware) → Request handling
4. US7-US9 (Core Handlers) → Full pipeline
5. US10-US11 (Replay + Health) → Audit + Ops
6. US12 + OpenAPI → GraphQL + Docs
7. Polish → Documentation, tests

---

## Summary

- **Total Tasks**: 166
- **Phase 1 (Setup)**: 7 tasks
- **Phase 2 (Foundational)**: 7 tasks
- **Phase 3 (US1 - Auth)**: 12 tasks
- **Phase 4 (US2 - RBAC)**: 9 tasks
- **Phase 5 (US3 - Correlation)**: 9 tasks
- **Phase 6 (US4 - Pagination)**: 11 tasks
- **Phase 7 (US5 - Idempotency)**: 11 tasks
- **Phase 8 (US6 - Errors)**: 9 tasks
- **Phase 9 (US7 - Documents)**: 12 tasks
- **Phase 10 (US8 - Snapshots)**: 12 tasks
- **Phase 11 (US9 - Passports)**: 15 tasks
- **Phase 12 (US10 - Replay)**: 8 tasks
- **Phase 13 (US11 - Health)**: 13 tasks
- **Phase 14 (US12 - GraphQL)**: 7 tasks
- **Phase 15 (Additional)**: 5 tasks
- **Phase 16 (OpenAPI)**: 5 tasks
- **Phase 17 (Integration)**: 5 tasks
- **Phase 18 (Polish)**: 9 tasks

**MVP Scope**: Phases 1-11 (104 tasks) for core API
**Parallel Opportunities**: 20+ tasks marked [P]
**Constitution Compliance**: Canonical storage, audit trail, layered architecture
