# Tasks: Advanced Features

**Input**: Design documents from `specs/08-advanced-features/`
**Prerequisites**: plan.md (required), spec.md (required), contracts/
**Phase**: P4 - Advanced features, depends on all earlier phases
**Packages**: bpc-api, bpc-db, bpc-worker (cross-cutting)

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US8)
- Include exact file paths in descriptions

## Path Conventions

- **API**: `packages/bpc-api/src/BPC/API/`
- **DB**: `packages/bpc-db/src/BPC/DB/Repos/`
- **Worker**: `packages/bpc-worker/src/BPC/Worker/Handlers/`
- **Migrations**: `migrations/`

---

## Phase 1: Setup (Migrations)

**Purpose**: Create database tables for advanced features

- [x] T001 Create `migrations/003_add_policies_webhooks.sql` with policies table
- [x] T002 Add policy_versions table to migration 003
- [x] T003 Add webhook_endpoints table to migration 003
- [x] T004 Add webhook_subscriptions table to migration 003
- [x] T005 Add webhook_deliveries table to migration 003
- [x] T006 [P] Create `migrations/004_add_rate_limits.sql` with rate_limit_buckets table
- [x] T007 Add indexes for policy evaluation (tenant_id, is_active, priority) to migration 003
- [x] T008 Add indexes for webhook lookup (tenant_id, event_type) to migration 003
- [x] T009 Add index for rate limit key_hash to migration 004

**Checkpoint**: `dbmate up` runs both migrations successfully

---

## Phase 2: Foundational (Repository Interfaces)

**Purpose**: Define repository interfaces - BLOCKS all user stories

- [x] T010 Create `packages/bpc-db/src/BPC/DB/Repos/Policies.hs` module skeleton
- [x] T011 [P] Create `packages/bpc-db/src/BPC/DB/Repos/RateLimits.hs` module skeleton
- [x] T012 [P] Define PolicyVersion data type in Policies.hs
- [x] T013 [P] Define RateLimitBucket data type in RateLimits.hs
- [x] T014 Export all repo modules from BPC.DB

**Checkpoint**: All repository skeletons compile

---

## Phase 3: User Story 1 - Policy Engine (Priority: P1) MVP

**Goal**: Fine-grained access control beyond RBAC

**Independent Test**: DENY policy blocks despite having RBAC permission

### Tests for User Story 1

- [x] T015 [P] [US1] Create `packages/bpc-api/test/BPC/API/PolicySpec.hs` with scaffold
- [x] T016 [US1] Add test: DENY policy blocks despite permission in PolicySpec.hs
- [x] T017 [US1] Add test: First-match by priority in PolicySpec.hs
- [x] T018 [US1] Add test: No matching policy → fall back to RBAC in PolicySpec.hs
- [x] T019 [US1] Add test: Policy hash computed from canonical JSON in PolicySpec.hs

### Implementation for User Story 1

- [x] T020 [US1] Define PolicyDecision data type (Allow | Deny) in Policy.hs
- [x] T021 [US1] Implement getActivePolicies :: Connection -> TenantId -> Text -> IO [PolicyVersion] in Policies.hs
- [x] T022 [US1] Implement matchesRequest :: Request -> PolicyVersion -> Bool in Policy.hs
- [x] T023 [US1] Implement evaluatePolicy :: AuthContext -> Request -> AppM PolicyDecision in Policy.hs
- [x] T024 [US1] Sort policies by priority (lowest number = highest priority)
- [x] T025 [US1] Integrate policy evaluation into auth middleware
- [x] T026 [US1] Create `packages/bpc-api/src/BPC/API/Middleware/Policy.hs` module
- [x] T027 [US1] Create `packages/bpc-api/src/BPC/API/Handlers/Policies.hs` CRUD handlers

**Checkpoint**: Policy Engine blocks/allows based on first-match priority

---

## Phase 4: User Story 2 - Token Bucket Rate Limiting (Priority: P1)

**Goal**: Protect API from abuse with per-key rate limits (BPC-RL-1)

**Independent Test**: Burst exceeds limit → 429 with Retry-After

### Tests for User Story 2

- [x] T028 [P] [US2] Create `packages/bpc-api/test/BPC/API/RateLimitSpec.hs` with scaffold
- [x] T029 [US2] Add test: Requests under limit → allowed in RateLimitSpec.hs
- [x] T030 [US2] Add test: Burst exceeds limit → 429 in RateLimitSpec.hs
- [x] T031 [US2] Add test: Token refill over time in RateLimitSpec.hs
- [x] T032 [US2] Add test: Limits independent per API key in RateLimitSpec.hs
- [x] T033 [US2] Add test: 429 response has Retry-After header in RateLimitSpec.hs

### Implementation for User Story 2

- [x] T034 [US2] Create `packages/bpc-api/src/BPC/API/Middleware/RateLimit.hs` module
- [x] T035 [US2] Define RateLimitConfig data type (from ENV) in RateLimit.hs
- [x] T036 [US2] Implement consumeToken :: Connection -> TenantId -> ApiKeyHash -> IO (Either RateLimitExceeded ()) in RateLimits.hs
- [x] T037 [US2] Use FOR UPDATE in atomic SQL for token bucket
- [x] T038 [US2] Implement token refill calculation: `min(capacity, tokens + elapsed * refill_per_second)`
- [x] T039 [US2] Implement computeRetryAfter :: Connection -> TenantId -> ApiKeyHash -> IO Int in RateLimits.hs
- [x] T040 [US2] Implement rateLimitMiddleware :: Middleware in RateLimit.hs
- [x] T041 [US2] Return 429 with Retry-After header on rate limit exceeded
- [x] T042 [US2] Implement fail-open on DB timeout (for availability)
- [x] T043 [US2] Add bpc_rate_limited_total metric counter

**Checkpoint**: Token bucket rate limiting per BPC-RL-1; 429 with Retry-After

---

## Phase 5: User Story 3 - Webhook Subscriptions (Priority: P2)

**Goal**: Configure webhook endpoints for event notifications

**Independent Test**: Create endpoint, subscribe to event type

### Tests for User Story 3

- [x] T044 [P] [US3] Create `packages/bpc-api/test/BPC/API/WebhooksSpec.hs` with scaffold
- [x] T045 [US3] Add test: POST /webhook-endpoints creates endpoint in WebhooksSpec.hs
- [x] T046 [US3] Add test: POST /webhook-subscriptions creates subscription in WebhooksSpec.hs
- [x] T047 [US3] Add test: Event triggers DELIVER_WEBHOOK job in WebhooksSpec.hs
- [x] T048 [US3] Add test: Successful delivery → status DELIVERED in WebhooksSpec.hs

### Implementation for User Story 3

- [x] T049 [US3] Implement createWebhookEndpoint in Webhooks.hs repo
- [x] T050 [US3] Implement getWebhookEndpoint in Webhooks.hs repo
- [x] T051 [US3] Implement deactivateEndpoint in Webhooks.hs repo
- [x] T052 [US3] Implement createSubscription in Webhooks.hs repo
- [x] T053 [US3] Implement getSubscriptionsByEventType in Webhooks.hs repo
- [x] T054 [US3] Define WebhookEventType enum (passport.version.signed, etc.) in Webhooks.hs
- [x] T055 [US3] Implement triggerWebhook :: Connection -> TenantId -> EventId -> Text -> IO () in Webhooks.hs
- [x] T056 [US3] Integrate triggerWebhook into event store append
- [x] T057 [US3] Create `packages/bpc-api/src/BPC/API/Handlers/Webhooks.hs` CRUD handlers
- [x] T058 [US3] Add webhook endpoints to Routes.hs

**Checkpoint**: Webhook subscriptions trigger DELIVER_WEBHOOK jobs

---

## Phase 6: User Story 4 - Webhook HMAC Signature (Priority: P2)

**Goal**: Sign webhook payloads with HMAC-SHA256 (BPC-WH-1)

**Independent Test**: Webhook recipient can verify X-BPC-Signature

### Tests for User Story 4

- [x] T059 [P] [US4] Add HMAC tests to WebhooksSpec.hs
- [x] T060 [US4] Add test: Webhook has X-BPC-Signature header in WebhooksSpec.hs
- [x] T061 [US4] Add test: Signature format is sha256=<hex> in WebhooksSpec.hs
- [x] T062 [US4] Add test: Signature verifiable with endpoint secret in WebhooksSpec.hs
- [x] T063 [US4] Add test: Tampered body fails verification in WebhooksSpec.hs

### Implementation for User Story 4

- [x] T064 [US4] Implement computeWebhookSignature :: ByteString -> ByteString -> ByteString in DeliverWebhook.hs
- [x] T065 [US4] Use HMAC-SHA256 from cryptonite
- [x] T066 [US4] Format signature as `sha256=<hex>`
- [x] T067 [US4] Add X-BPC-Signature header to webhook request
- [x] T068 [US4] Add X-BPC-Event-Type header to webhook request
- [x] T069 [US4] Add X-BPC-Delivery-Id header to webhook request

**Checkpoint**: HMAC signature verifiable per BPC-WH-1

---

## Phase 7: User Story 5 - HTTP Idempotency Store (Priority: P1)

**Goal**: Store and replay idempotent request/response pairs (BPC-IDEMP-1)

**Independent Test**: Same request with same key → same response

### Tests for User Story 5

- [x] T070 [P] [US5] Create `packages/bpc-db/test/BPC/DB/IdempotencySpec.hs` with scaffold
- [x] T071 [US5] Add test: First request stores entry in IdempotencySpec.hs
- [x] T072 [US5] Add test: Same request → replay stored response in IdempotencySpec.hs
- [x] T073 [US5] Add test: Different request same key → 409 CONFLICT in IdempotencySpec.hs
- [x] T074 [US5] Add test: Old entries cleaned up (> 24h) in IdempotencySpec.hs

### Implementation for User Story 5

- [x] T075 [US5] Create `packages/bpc-db/src/BPC/DB/Repos/Idempotency.hs` module
- [x] T076 [US5] Define IdempotencyEntry data type in Idempotency.hs
- [x] T077 [US5] Implement lookupIdempotencyKey :: Connection -> TenantId -> Text -> IO (Maybe IdempotencyEntry) in Idempotency.hs
- [x] T078 [US5] Implement storeIdempotencyKey :: Connection -> TenantId -> Text -> ByteString -> Response -> IO () in Idempotency.hs
- [x] T079 [US5] Implement cleanupIdempotencyKeys :: Connection -> IO Int in Idempotency.hs
- [x] T080 [US5] Update API middleware to use idempotency store

**Checkpoint**: Idempotency per BPC-IDEMP-1; 409 on request mismatch

---

## Phase 8: User Story 6 - Data Retention (Priority: P2)

**Goal**: Delete expired data per configured retention periods

**Independent Test**: Retention job deletes only data older than retention period

### Tests for User Story 6

- [x] T081 [P] [US6] Create `packages/bpc-worker/test/BPC/Worker/Handlers/RetentionSpec.hs`
- [x] T082 [US6] Add test: Events older than 15 years deleted in RetentionSpec.hs
- [x] T083 [US6] Add test: passport_versions older than 15 years deleted in RetentionSpec.hs
- [x] T084 [US6] Add test: document_versions older than 10 years deleted in RetentionSpec.hs
- [x] T085 [US6] Add test: RetentionApplied event emitted in RetentionSpec.hs
- [x] T086 [US6] Add test: Recent data NOT deleted in RetentionSpec.hs

### Implementation for User Story 6

- [x] T087 [US6] Create `packages/bpc-worker/src/BPC/Worker/Handlers/Retention.hs` module
- [x] T088 [US6] Define RetentionConfig data type from ENV in Retention.hs
- [x] T089 [US6] Implement deleteEventsOlderThan :: Connection -> Int -> IO Int in Retention.hs
- [x] T090 [US6] Implement deletePassportVersionsOlderThan :: Connection -> Int -> IO Int in Retention.hs
- [x] T091 [US6] Implement deleteDocumentVersionsOlderThan :: Connection -> Int -> IO Int in Retention.hs
- [x] T092 [US6] Implement runRetention :: Pool Connection -> IO () orchestrator in Retention.hs
- [x] T093 [US6] Emit RetentionApplied event with counts and cutoff date
- [x] T094 [US6] Add CLI command `bpc-cli run-retention` for manual trigger

**Checkpoint**: Retention deletes expired data; emits audit event

---

## Phase 9: User Story 7 - Audit Events for Denied Access (Priority: P1)

**Goal**: Log all denied access attempts to audit trail (SSOT 0 invariant)

**Independent Test**: Permission denied → AccessDenied event in Event Store

### Tests for User Story 7

- [x] T095 [P] [US7] Add denied access tests to AuthSpec.hs
- [x] T096 [US7] Add test: Missing permission → AccessDenied event in AuthSpec.hs
- [x] T097 [US7] Add test: Policy DENY → AccessDenied event with policy details in AuthSpec.hs
- [x] T098 [US7] Add test: Event contains actor_id, resource, action, reason in AuthSpec.hs

### Implementation for User Story 7

- [x] T099 [US7] Implement logDeniedAccess :: AuthContext -> Permission -> Text -> AppM () in Auth.hs
- [x] T100 [US7] Emit AccessDenied event on RBAC deny
- [x] T101 [US7] Emit AccessDenied event on Policy DENY
- [x] T102 [US7] Include correlation_id in event payload
- [x] T103 [US7] Call logDeniedAccess in requirePermission before throwError

**Checkpoint**: All denied access generates audit event

---

## Phase 10: User Story 8 - Policy Version Management (Priority: P2)

**Goal**: Version policies without breaking changes

**Independent Test**: New version supersedes old; deactivated version ignored

### Tests for User Story 8

- [x] T104 [P] [US8] Add version tests to PolicySpec.hs
- [x] T105 [US8] Add test: New version gets incremented version number in PolicySpec.hs
- [x] T106 [US8] Add test: Deactivated version ignored in evaluation in PolicySpec.hs
- [x] T107 [US8] Add test: Multiple active versions → highest priority wins in PolicySpec.hs

### Implementation for User Story 8

- [x] T108 [US8] Implement createPolicyVersion in Policies.hs repo
- [x] T109 [US8] Implement deactivatePolicyVersion in Policies.hs repo
- [x] T110 [US8] Implement getActivePolicyVersions in Policies.hs repo
- [x] T111 [US8] Compute policy_hash from canonical JSON on version creation
- [x] T112 [US8] Policy versions are immutable (no update, only deactivate)

**Checkpoint**: Policy versioning works; evaluation uses only active versions

---

## Phase 11: API Contracts

**Purpose**: OpenAPI specs for advanced feature endpoints

- [x] T113 [P] Create `specs/08-advanced-features/contracts/policy-endpoints.yaml` OpenAPI spec
- [x] T114 [P] Create `specs/08-advanced-features/contracts/webhook-endpoints.yaml` OpenAPI spec
- [x] T115 [P] Create `specs/08-advanced-features/contracts/rate-limit-headers.yaml` header spec
- [x] T116 Add policy endpoints to main openapi.yaml
- [x] T117 Add webhook endpoints to main openapi.yaml
- [x] T118 Validate all OpenAPI specs with openapi-validator

---

## Phase 12: Integration Tests

**Purpose**: End-to-end tests for all advanced features

- [x] T119 [P] Create `packages/bpc-api/test/integration/AdvancedFeaturesSpec.hs`
- [x] T120 Add E2E test: Policy engine with RBAC in AdvancedFeaturesSpec.hs
- [x] T121 Add E2E test: Rate limiting under load in AdvancedFeaturesSpec.hs
- [x] T122 Add E2E test: Webhook delivery with signature verification in AdvancedFeaturesSpec.hs
- [x] T123 Add E2E test: Idempotency across API calls in AdvancedFeaturesSpec.hs
- [x] T124 Add E2E test: Retention job execution in AdvancedFeaturesSpec.hs

---

## Phase 13: Polish & Cross-Cutting Concerns

**Purpose**: Integration, documentation, and final verification

- [x] T125 [P] Add Haddock comments to all public functions
- [x] T126 Update main README with advanced features documentation
- [x] T127 Run full test suite across all packages
- [x] T128 Verify no cross-package architecture violations
- [x] T129 Run fourmolu check on all modified files
- [x] T130 Run hlint check on all modified files
- [x] T131 Update quickstart.md with advanced features examples
- [x] T132 Performance test: Policy evaluation < 10ms
- [x] T133 Performance test: Rate limit check < 5ms

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: Requires migrations 001-002 from 01-foundation
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **US1 (Policy Engine)**: After Foundational
- **US2 (Rate Limiting)**: After Foundational (can parallel with US1)
- **US3-US4 (Webhooks)**: After Foundational (can parallel with US1-US2)
- **US5 (Idempotency)**: After Foundational
- **US6 (Retention)**: After Foundational
- **US7 (Audit)**: After US1 (needs policy integration)
- **US8 (Policy Versions)**: After US1
- **Contracts, Integration, Polish**: After all user stories

### Parallel Opportunities

**Phase 3-6**: Policy, Rate Limiting, Webhooks, Idempotency can proceed in parallel
**Phase 9-10**: Audit and Policy Versions can proceed in parallel

---

## Implementation Strategy

### MVP First (Policy + Rate Limiting)

1. Complete Phase 1: Migrations
2. Complete Phase 2: Repository skeletons
3. Complete US1: Policy Engine
4. Complete US2: Rate Limiting
5. Complete US7: Audit for denied access
6. **STOP and VALIDATE**: Core security features work

### Incremental Delivery

1. Migrations → Tables ready
2. US1 (Policy Engine) → Fine-grained access
3. US2 (Rate Limiting) → Abuse protection
4. US3-US4 (Webhooks) → Event integration
5. US5 (Idempotency) → Reliable requests
6. US6 (Retention) → Compliance
7. US7-US8 (Audit + Versions) → Full audit trail
8. Polish → Documentation, performance

---

## Summary

- **Total Tasks**: 133
- **Phase 1 (Setup)**: 9 tasks
- **Phase 2 (Foundational)**: 5 tasks
- **Phase 3 (US1 - Policy Engine)**: 13 tasks
- **Phase 4 (US2 - Rate Limiting)**: 16 tasks
- **Phase 5 (US3 - Webhooks)**: 15 tasks
- **Phase 6 (US4 - HMAC)**: 11 tasks
- **Phase 7 (US5 - Idempotency)**: 11 tasks
- **Phase 8 (US6 - Retention)**: 14 tasks
- **Phase 9 (US7 - Audit)**: 9 tasks
- **Phase 10 (US8 - Policy Versions)**: 9 tasks
- **Phase 11 (Contracts)**: 6 tasks
- **Phase 12 (Integration)**: 6 tasks
- **Phase 13 (Polish)**: 9 tasks

**MVP Scope**: Phases 1-4, 9 (52 tasks) for Policy + Rate Limiting + Audit
**Parallel Opportunities**: 15+ tasks marked [P]
**Constitution Compliance**: Canonical storage for policies, audit trail for denied access
