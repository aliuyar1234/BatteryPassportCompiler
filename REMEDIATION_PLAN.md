# BPC Remediation Plan

**Current Score:** 4/10
**Target Score:** 10/10
**Total Tasks:** 89

---

## Priority 1: CRITICAL Security Fixes (Must fix before any deployment)

### SEC-001: Hardcoded API Key Pepper
**File:** `bpc-api/app/Main.hs:79`
**Risk:** Complete API key compromise if source leaked

- [ ] 1.1 Add `BPC_API_KEY_PEPPER` to required environment variables
- [ ] 1.2 Update `buildEnv` to read pepper from env with validation
- [ ] 1.3 Add startup check that pepper is at least 32 bytes
- [ ] 1.4 Document pepper rotation procedure in OPERATIONS.md
- [ ] 1.5 Add test for pepper length validation

### SEC-002: Auth Middleware Bypass (CRITICAL)
**File:** `bpc-api/src/BPC/API/Middleware/Auth.hs:91-92`
**Risk:** Authentication completely bypassed - all requests treated as unauthenticated

- [ ] 2.1 Fix `pathInfo` extraction from WAI Request (use `Network.Wai.pathInfo`)
- [ ] 2.2 Add unit test verifying pathInfo extraction for `/api/v1/passports`
- [ ] 2.3 Add unit test verifying pathInfo extraction for `/health`
- [ ] 2.4 Add integration test: unauthenticated request to protected endpoint returns 401
- [ ] 2.5 Add integration test: valid API key returns 200
- [ ] 2.6 Add integration test: invalid API key returns 401

### SEC-003: API Key Expiry Not Checked
**File:** `bpc-db/src/BPC/DB/Repos/Auth.hs:345`
**Risk:** Expired API keys remain valid indefinitely

- [ ] 3.1 Implement `checkExpiry` function comparing `expires_at` with current time
- [ ] 3.2 Update `validateApiKey` to return `Left ApiKeyExpired` when expired
- [ ] 3.3 Add test: expired key returns 401 with "API key expired" message
- [ ] 3.4 Add test: key with NULL expiry never expires
- [ ] 3.5 Add test: key expiring in future is valid

### SEC-004: Webhook Secrets in Plain Text
**File:** `migrations/003_add_policies_webhooks.sql`
**Risk:** Database breach exposes all webhook secrets

- [ ] 4.1 Add `encrypt_secret`/`decrypt_secret` functions using pgcrypto
- [ ] 4.2 Create migration to add encrypted_secret column
- [ ] 4.3 Update `WebhookEndpoints` repo to encrypt on write
- [ ] 4.4 Update `WebhookEndpoints` repo to decrypt on read
- [ ] 4.5 Add `BPC_WEBHOOK_ENCRYPTION_KEY` environment variable
- [ ] 4.6 Create data migration script to encrypt existing secrets
- [ ] 4.7 Add test verifying secrets are not stored in plain text

### SEC-005: Signing Key Management
**File:** `bpc-worker/src/BPC/Worker/Handlers/SignPassport.hs`
**Risk:** Private key in memory/env, no HSM integration

- [ ] 5.1 Create `SigningKeyProvider` typeclass with `signPayload` method
- [ ] 5.2 Implement `EnvKeyProvider` for development (current behavior)
- [ ] 5.3 Add configuration flag `BPC_SIGNING_MODE=env|hsm`
- [ ] 5.4 Document HSM integration requirements in SECURITY.md
- [ ] 5.5 Add test for key provider interface

---

## Priority 2: Critical Code Quality (Broken functionality)

### CQ-001: Routes.hs Returns 404 for ALL Requests
**File:** `bpc-api/src/BPC/API/Routes.hs:116-121`
**Risk:** API completely non-functional

- [ ] 6.1 Implement route matching logic using `pathInfo` and method
- [ ] 6.2 Wire up `/api/v1/passports` to PassportHandlers
- [ ] 6.3 Wire up `/api/v1/documents` to DocumentHandlers
- [ ] 6.4 Wire up `/api/v1/snapshots` to SnapshotHandlers
- [ ] 6.5 Wire up `/api/v1/rules` to RuleHandlers
- [ ] 6.6 Wire up `/api/v1/jobs` to JobHandlers
- [ ] 6.7 Wire up `/api/v1/policies` to PolicyHandlers
- [ ] 6.8 Wire up `/api/v1/webhook-endpoints` to WebhookHandlers
- [ ] 6.9 Wire up `/health` and `/ready` endpoints
- [ ] 6.10 Add integration test for each route family

### CQ-002: CLI Package Completely Stubbed
**File:** `bpc-cli/src/BPC/CLI/*.hs`
**Risk:** No operational tooling

- [ ] 7.1 Implement `migrate` command using dbmate
- [ ] 7.2 Implement `seed` command for development data
- [ ] 7.3 Implement `verify` command for passport verification
- [ ] 7.4 Implement `export` command for passport export
- [ ] 7.5 Implement `replay` command for audit trail replay
- [ ] 7.6 Add CLI integration tests

### CQ-003: Pagination Not Implemented
**File:** `bpc-api/src/BPC/API/Handlers/*.hs`
**Risk:** Memory exhaustion on large result sets

- [ ] 8.1 Create `Pagination` type with `limit`, `offset`, `cursor` fields
- [ ] 8.2 Create `PaginatedResponse` type with `items`, `total`, `next_cursor`
- [ ] 8.3 Update `listDocuments` to accept and apply pagination
- [ ] 8.4 Update `listPassports` to accept and apply pagination
- [ ] 8.5 Update `listJobs` to accept and apply pagination
- [ ] 8.6 Add tests for pagination edge cases (empty, last page, invalid cursor)

### CQ-004: Idempotency Key Implementation Missing
**File:** `bpc-db/src/BPC/DB/Repos/Idempotency.hs`
**Risk:** Duplicate requests create duplicate resources

- [ ] 9.1 Implement `getIdempotencyRecord` with Redis/DB lookup
- [ ] 9.2 Implement `setIdempotencyRecord` with TTL
- [ ] 9.3 Create idempotency middleware for POST/PUT/PATCH
- [ ] 9.4 Store response body hash for replay
- [ ] 9.5 Add test: same key returns cached response
- [ ] 9.6 Add test: different key creates new resource
- [ ] 9.7 Add test: expired key creates new resource

### CQ-005: Audit Replay Verification Stubbed
**File:** `bpc-core/src/BPC/Core/Replay.hs`
**Risk:** Audit trail integrity unverifiable

- [ ] 10.1 Implement `replayEvents` to reconstruct state from events
- [ ] 10.2 Implement `verifyHashChain` to validate BPC-EVENT-1 chain
- [ ] 10.3 Implement `compareState` to verify replay matches current
- [ ] 10.4 Add test: valid chain passes verification
- [ ] 10.5 Add test: tampered event fails verification
- [ ] 10.6 Add test: missing event fails verification

### CQ-006: GraphQL Resolvers Stubbed
**File:** `bpc-api/src/BPC/API/GraphQL/Resolvers.hs`
**Risk:** GraphQL API non-functional

- [ ] 11.1 Implement `passportResolver` with field selection
- [ ] 11.2 Implement `documentResolver` with relationships
- [ ] 11.3 Implement `snapshotResolver` with nested facts
- [ ] 11.4 Implement `jobsResolver` with filtering
- [ ] 11.5 Add N+1 query prevention via DataLoader pattern
- [ ] 11.6 Add GraphQL integration tests

---

## Priority 3: Test Coverage (35% → 60-80%)

### TST-001: Replace Pending Tests with Real Assertions
**Files:** All `*Spec.hs` files (129 pending tests)

- [ ] 12.1 `CanonicalJsonSpec.hs`: Implement 8 pending tests for BPC-CJSON-1
- [ ] 12.2 `ProofSpec.hs`: Implement 6 pending tests for derivation trees
- [ ] 12.3 `ReceiptSpec.hs`: Implement 5 pending tests for BPC-RECEIPT-1
- [ ] 12.4 `SignatureSpec.hs`: Implement 4 pending tests for ED25519
- [ ] 12.5 `EvaluatorSpec.hs`: Implement 12 pending tests for DSL evaluation
- [ ] 12.6 `AuthSpec.hs`: Implement 8 pending tests for authentication
- [ ] 12.7 `RateLimitSpec.hs`: Implement 4 pending tests for token bucket
- [ ] 12.8 `PolicySpec.hs`: Implement 3 pending tests for policy engine
- [ ] 12.9 `WebhooksSpec.hs`: Implement 7 pending tests for HMAC signing
- [ ] 12.10 `JobsSpec.hs`: Implement 6 pending tests for job lifecycle
- [ ] 12.11 `DocumentsSpec.hs`: Implement 5 pending tests for CRUD
- [ ] 12.12 `PassportsSpec.hs`: Implement 5 pending tests for compilation

### TST-002: Add Integration Test Suite
**Files:** `test/integration/*.hs`

- [ ] 13.1 Create test database setup/teardown fixtures
- [ ] 13.2 Add end-to-end test: upload → parse → compile → sign → verify
- [ ] 13.3 Add end-to-end test: rule validation → publish → deprecate
- [ ] 13.4 Add end-to-end test: webhook trigger → delivery → retry
- [ ] 13.5 Add end-to-end test: rate limiting under concurrent load
- [ ] 13.6 Add end-to-end test: policy deny overrides RBAC allow

### TST-003: Add Property-Based Tests
**Files:** `test/properties/*.hs`

- [ ] 14.1 Property: canonical JSON encoding is deterministic
- [ ] 14.2 Property: proof verification roundtrips
- [ ] 14.3 Property: hash chain is tamper-evident
- [ ] 14.4 Property: rate limit token bucket converges

---

## Priority 4: Infrastructure

### INF-001: Docker Configuration
**Files:** `docker/`, `docker-compose.yml`

- [ ] 15.1 Create `Dockerfile` for bpc-api with multi-stage build
- [ ] 15.2 Create `Dockerfile` for bpc-worker
- [ ] 15.3 Update `docker-compose.yml` with api/worker services
- [ ] 15.4 Add health checks to all services
- [ ] 15.5 Create `docker-compose.override.yml` for local development
- [ ] 15.6 Document container deployment in DEPLOYMENT.md

### INF-002: CI/CD Pipeline
**Files:** `.github/workflows/`

- [ ] 16.1 Create `ci.yml` with build, test, lint jobs
- [ ] 16.2 Add Fourmolu formatting check
- [ ] 16.3 Add HLint check
- [ ] 16.4 Add test coverage reporting
- [ ] 16.5 Add security scanning (dependency audit)
- [ ] 16.6 Create `release.yml` for tagged releases

### INF-003: Observability
**Files:** `bpc-api/src/BPC/API/Observability.hs`

- [ ] 17.1 Add structured logging with correlation IDs
- [ ] 17.2 Add Prometheus metrics endpoint
- [ ] 17.3 Add OpenTelemetry tracing hooks
- [ ] 17.4 Document runbook in OPERATIONS.md

---

## Priority 5: Database Security

### DB-001: Connection Security
**File:** `bpc-db/src/BPC/DB/Pool.hs`

- [ ] 18.1 Add SSL mode configuration (`sslmode=verify-full`)
- [ ] 18.2 Add client certificate support for mTLS
- [ ] 18.3 Add connection string validation
- [ ] 18.4 Document secure connection setup

### DB-002: Audit Logging
**Files:** `migrations/`

- [ ] 19.1 Enable `pgaudit` extension
- [ ] 19.2 Configure audit logging for sensitive tables
- [ ] 19.3 Add audit log rotation policy

---

## Priority 6: Performance

### PERF-001: Query Optimization
**Files:** `bpc-db/src/BPC/DB/Repos/*.hs`

- [ ] 20.1 Add connection pooling configuration (min/max/timeout)
- [ ] 20.2 Add query timeout settings
- [ ] 20.3 Review and add missing indexes
- [ ] 20.4 Add EXPLAIN ANALYZE for critical queries in tests

### PERF-002: Caching
**Files:** `bpc-api/src/BPC/API/Cache.hs`

- [ ] 21.1 Add Redis cache for rule package lookups
- [ ] 21.2 Add cache invalidation on rule publish
- [ ] 21.3 Add cache-control headers for GET endpoints

---

## Priority 7: API Design

### API-001: Error Response Consistency
**Files:** `bpc-api/src/BPC/API/Error.hs`

- [ ] 22.1 Create unified `ApiError` type with code, message, details
- [ ] 22.2 Ensure all handlers return consistent error format
- [ ] 22.3 Add request-id to all error responses
- [ ] 22.4 Document error codes in OpenAPI spec

### API-002: OpenAPI Specification
**Files:** `bpc-api/openapi/`

- [ ] 23.1 Generate OpenAPI 3.1 spec from route definitions
- [ ] 23.2 Add request/response examples
- [ ] 23.3 Add authentication documentation
- [ ] 23.4 Serve spec at `/api/v1/openapi.json`

---

## Priority 8: Consistency Fixes

### CON-001: Type Alignment
**Files:** Various

- [ ] 24.1 Unify `AppError` types across packages
- [ ] 24.2 Ensure all UUIDs use `Data.UUID` consistently
- [ ] 24.3 Ensure all timestamps use `UTCTime`
- [ ] 24.4 Add newtype wrappers for domain IDs

### CON-002: Module Structure
**Files:** `bpc-*/src/BPC/*`

- [ ] 25.1 Verify import matrix compliance (no circular deps)
- [ ] 25.2 Add import linter to CI
- [ ] 25.3 Document module boundaries in ARCHITECTURE.md

---

## Summary by Priority

| Priority | Domain | Tasks | Effort |
|----------|--------|-------|--------|
| P1 | Security | 25 | High |
| P2 | Code Quality | 35 | High |
| P3 | Test Coverage | 20 | Medium |
| P4 | Infrastructure | 14 | Medium |
| P5 | Database | 7 | Low |
| P6 | Performance | 7 | Medium |
| P7 | API Design | 7 | Low |
| P8 | Consistency | 7 | Low |
| **Total** | | **89** | |

---

## Recommended Execution Order

1. **Week 1:** P1 Security (SEC-001 through SEC-005) - Cannot deploy without these
2. **Week 2:** P2-CQ-001 Routes + P2-CQ-003 Pagination - Basic API functionality
3. **Week 3:** P3 Test Coverage - Validate fixes work
4. **Week 4:** P2 remaining (CLI, Idempotency, Replay, GraphQL)
5. **Week 5:** P4 Infrastructure (Docker, CI/CD)
6. **Week 6:** P5-P8 Polish (DB security, Performance, API design, Consistency)

---

## Verification Checklist

After completing all tasks, verify:

- [ ] All 89 tasks marked complete
- [ ] `cabal build all` succeeds with no warnings
- [ ] `cabal test all` passes with 60%+ coverage
- [ ] `fourmolu -m check` passes
- [ ] `hlint` reports no errors
- [ ] Docker containers build and run
- [ ] CI pipeline passes
- [ ] Manual security review completed
- [ ] Load test passes (1000 req/s sustained)
