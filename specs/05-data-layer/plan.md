# Implementation Plan: Data Layer

**Branch**: `05-data-layer` | **Date**: 2025-12-28 | **Spec**: [spec.md](../../.specify/features/05-data-layer/spec.md)
**Input**: Feature specification from `.specify/features/05-data-layer/spec.md`

## Summary

The Data Layer (bpc-db package) provides database access infrastructure for BatteryPassportCompiler. It implements connection pooling, an append-only Event Store with hash chain verification (BPC-EVENT-1), and tenant-scoped repositories for all domain entities. The Event Store is CRITICAL for audit trail compliance (Constitution IV), ensuring every write and denied access generates a tamper-evident audit event with hash chain linking.

**Technical Approach**: Use `postgresql-simple` for type-safe SQL queries with explicit tenant filtering, `resource-pool` for connection management, and implement hash chain verification per BPC-EVENT-1 specification from SSOT 7.4. All repository functions are tenant-scoped (TenantId as first parameter) to enforce multi-tenancy isolation. Job queue uses `FOR UPDATE SKIP LOCKED` for race-free leasing with exponential backoff retry logic.

## Technical Context

**Language/Version**: Haskell (GHC 9.6.4)
**Primary Dependencies**: postgresql-simple 0.7+, resource-pool 0.4+, aeson 2.1+, bytestring, text, uuid, time, cryptonite (via bpc-core for SHA-256)
**Storage**: PostgreSQL 16 (all 25+ tables from SSOT 6.2, including events, documents, document_versions, facts, data_snapshots, snapshot_items, battery_products, passports, passport_versions, rule_packages, rule_package_versions, rule_fields, rule_tests_runs, jobs, tenants, actors, api_keys, roles, permissions, role_permissions, actor_roles, idempotency_keys, policies, policy_versions, webhook_endpoints, webhook_subscriptions, webhook_deliveries)
**Testing**: Hspec + QuickCheck for property tests; docker-compose Postgres 16 for integration tests
**Target Platform**: Linux server (Debian Bookworm) + Docker containers
**Project Type**: Single library package (bpc-db) with 9 repository modules
**Performance Goals**: Connection pool efficiency (reuse); hash chain verification for 10,000+ events; query latency <10ms p95
**Constraints**: All repo functions MUST be tenant-scoped (WHERE tenant_id=$1); Event Store append-only (no UPDATE/DELETE); SEALED snapshots immutable (status check before modification); PUBLISHED rules immutable
**Scale/Scope**: 25+ tables; 9 repository modules; ~2500 LOC; Event Store with hash chain; Job Queue with idempotent leasing

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Critical Compliance

| Principle | Requirement | Implementation | Status |
|-----------|-------------|----------------|--------|
| **IV. Audit Trail (CRITICAL)** | Every write + denied access MUST generate Event | Event Store append-only; `appendEvent` implements BPC-EVENT-1 hash chain (SSOT 7.4); hash links prev_event_hash → event_hash for tamper detection | ✓ ENFORCED |
| **III. Immutability** | SEALED Snapshots, PUBLISHED Rules, PassportVersion immutable | Repository functions check status before modification; UPDATE blocked with SNAPSHOT_SEALED / RULE_PKG_NOT_PUBLISHED errors | ✓ ENFORCED |
| **II. Canonical Storage** | Storage is canonical bytes; JSON pretty-print never source of truth | payload_canonical, proof_canonical, receipt_canonical, snapshot_canonical stored as bytea; API responses via canonicalDecode | ✓ ENFORCED |
| **V. Layered Architecture** | bpc-db MUST NOT import api/worker | Module Import Matrix (SSOT 3.4.1): BPC.DB.* can import BPC.Core.* only; CI enforces with dependency check | ✓ ENFORCED |
| **Multi-Tenancy** | All data tenant-scoped; every repo function filters by TenantId | TenantId as first parameter in all repo functions; WHERE tenant_id=$1 in all queries; tenant isolation verified in integration tests | ✓ ENFORCED |

### Quality Gates

- **Test Coverage**: ≥ 80% for bpc-db (Constitution requirement)
- **Integration Tests**: docker-compose Postgres 16 with all 25+ tables; migrations applied via dbmate
- **Property Tests**: Hash chain verification (forall events. append >> verify === Right ()); deterministic snapshot seal (same facts → same hash)
- **Formatting**: fourmolu (2-space indent, leading commas) + hlint (CI enforced, warnings as errors)
- **Compiler**: -Wall -Wcompat -Werror (warnings are errors)

### Verification

- [ ] All repository functions accept `TenantId` as first parameter (grep verification)
- [ ] Event Store `appendEvent` implements BPC-EVENT-1 hash chain (unit test with golden hash)
- [ ] `verifyChain` detects tampered events (property test: modify hash → verify fails)
- [ ] Snapshot seal is deterministic (property test: same facts ordered differently → same hash)
- [ ] Job leasing uses `FOR UPDATE SKIP LOCKED` (integration test: 2 workers → only 1 gets job)
- [ ] Integration tests pass with docker-compose Postgres (CI)
- [ ] No imports of BPC.API.* or BPC.Worker.* modules (CI dependency check)
- [ ] Code coverage ≥ 80% (CI gate)

## Project Structure

### Documentation (this feature)

```text
specs/05-data-layer/
├── plan.md              # This file (/speckit.plan output)
├── research.md          # Phase 0: postgresql-simple, pooling, hash chain, leasing
├── data-model.md        # Phase 1: Complete DDL + Haskell types + repo signatures
├── quickstart.md        # Phase 1: Usage guide with examples
└── contracts/           # N/A (internal library, no API contracts)
```

### Source Code (repository root)

```text
packages/bpc-db/
├── bpc-db.cabal                     # Package definition with dependencies
├── src/
│   └── BPC/
│       └── DB/
│           ├── Pool.hs              # Connection pool: mkPool, withConn
│           ├── Migrations.hs        # dbmate wrapper + schema version check
│           ├── Error.hs             # DB error types (DBError, domain errors)
│           └── Repos/
│               ├── Events.hs        # Event Store: appendEvent, verifyChain
│               ├── Auth.hs          # Actors, ApiKeys, Roles, Permissions, Policies
│               ├── Documents.hs     # Documents + DocumentVersions
│               ├── Facts.hs         # Facts insert/query (by type/key/prefix)
│               ├── Snapshots.hs     # Snapshots + Items + Seal (BPC-SNAPSHOT-1)
│               ├── Rules.hs         # RulePackages + Versions + Fields + TestRuns
│               ├── Passports.hs     # Passports + PassportVersions (activate/revoke)
│               ├── Jobs.hs          # Job queue: enqueue, acquireLease, complete/fail
│               └── Webhooks.hs      # Webhook endpoints/subscriptions/deliveries
└── test/
    ├── unit/
    │   ├── PoolSpec.hs              # Pool creation, checkout, timeout
    │   ├── EventsSpec.hs            # Hash chain calculation, verification
    │   ├── SnapshotsSpec.hs         # Seal determinism property test
    │   └── JobsSpec.hs              # Backoff calculation, idempotency
    └── integration/
        ├── Main.hs                  # Test suite entry point
        ├── DatabaseSpec.hs          # Connection, migrations, seed data
        ├── EventsIntegrationSpec.hs # Append, concurrent conflicts, verify 1000+ events
        ├── SnapshotsIntegrationSpec.hs # Create, add items, seal, immutability
        ├── JobsIntegrationSpec.hs   # Enqueue, lease (race test), complete/fail, backoff
        └── TenantIsolationSpec.hs   # Query with wrong tenant → Nothing

migrations/
├── 001_init.sql                     # All 25+ tables (SSOT 6.2): enums, tenants, actors, api_keys, roles, permissions, role_permissions, actor_roles, idempotency_keys, events, documents, document_versions, facts, data_snapshots, snapshot_items, battery_products, passports, rule_packages, rule_package_versions, rule_fields, rule_tests_runs, passport_versions, jobs
├── 002_seed_permissions_roles.sql   # Seed permissions + dev tenant + roles (Admin, ComplianceOfficer, Auditor)
├── 003_add_policies_webhooks.sql    # Policies, policy_versions, webhook_endpoints, webhook_subscriptions, webhook_deliveries
└── 004_add_rate_limits.sql          # (Future) Rate limiting tables

scripts/
├── migrate.sh                       # Run dbmate migrations (reads DATABASE_URL from ENV)
└── seed-dev.sh                      # Seed development data (test tenant, actors, API keys)

docker-compose.test.yml              # Postgres 16 on port 55432 for integration tests
```

**Structure Decision**: Single library package (bpc-db) with modular repository structure per bounded context. Each repository module owns queries for related tables (e.g., Snapshots.hs owns data_snapshots + snapshot_items). Event Store isolated in Repos/Events.hs to enforce append-only semantics (no UPDATE/DELETE queries exposed). Integration tests use docker-compose Postgres 16 (SSOT 4.7) with dbmate migrations. Pool.hs provides connection management; all repos accept Connection (not Pool) for flexibility in transactions.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

N/A - No constitution violations. All requirements align with Constitution principles:
- **Audit Trail**: Event Store with hash chain satisfies Constitution IV
- **Immutability**: Status checks enforce Constitution III
- **Canonical Storage**: bytea columns satisfy Constitution II
- **Layered Architecture**: Module imports satisfy Constitution V
- **Multi-Tenancy**: TenantId filtering satisfies application-layer row-level security

## Implementation Phases

### Phase 0: Research & Design

**Objective**: Research postgresql-simple patterns, connection pooling strategies (resource-pool), hash chain algorithm (BPC-EVENT-1), job leasing patterns (FOR UPDATE SKIP LOCKED), and tenant isolation approaches.

**Deliverables**:
- `specs/05-data-layer/research.md` documenting:
  - **postgresql-simple** vs alternatives (Hasql, Beam, Esqueleto, Persistent): Rationale for direct SQL with type safety
  - **resource-pool** configuration: pool size (BPC_DB_POOL_SIZE), idle timeout, striping (single stripe recommended), connection lifecycle
  - **Hash chain algorithm** (BPC-EVENT-1 from SSOT 7.4): event_hash = sha256_hex(canonicalEncode({tenant_id, aggregate_type, aggregate_id, aggregate_version, event_type, occurred_at, actor_id, payload_hash, prev_event_hash}))
  - **FOR UPDATE SKIP LOCKED** pattern: SELECT ... WHERE status='QUEUED' AND scheduled_at <= now() ORDER BY priority DESC, scheduled_at ASC FOR UPDATE SKIP LOCKED LIMIT 1; prevents double-run in concurrent workers
  - **Tenant isolation**: WHERE tenant_id=$1 in all queries; compile-time enforcement via code review + integration tests

**Research Questions**:
1. **Connection pool exhaustion**: How to handle gracefully? (Answer: timeout with DB_UNAVAILABLE; monitor pool metrics; configurable BPC_DB_POOL_SIZE)
2. **Optimal pool size**: API vs Worker? (Answer: API needs responsive pool; Worker can tolerate higher latency; default 10, tune via ENV)
3. **Hash chain verification efficiency**: 10,000+ events? (Answer: Index on (tenant_id, aggregate_type, aggregate_id, aggregate_version); batch verification; parallelize across aggregates)
4. **Double-run prevention**: Job leasing race? (Answer: FOR UPDATE SKIP LOCKED; UNIQUE constraint on lease_owner during lease period)
5. **Tenant isolation enforcement**: Prevent missing WHERE? (Answer: Code review checklist; integration tests verify isolation; grep for "SELECT.*FROM.*WHERE" patterns)

**Dependencies**: None (pure research)

**Exit Criteria**: Research document approved; approach for all 5 questions documented with rationale and trade-offs.

---

### Phase 1: Data Model & Contracts

**Objective**: Document complete DDL (all 25+ tables from SSOT 6.2), Haskell repository function signatures (pure types, no implementation), event types and payload schemas, and job queue types.

**Deliverables**:
- `specs/05-data-layer/data-model.md` with:
  - **Complete DDL** for all 25+ tables (SSOT 6.2):
    - Enums: actor_type, job_type, job_status, document_kind, document_status, snapshot_status, rule_pkg_status, passport_status, access_decision
    - Tables: tenants, actors, api_keys, roles, permissions, role_permissions, actor_roles, idempotency_keys, events, documents, document_versions, facts, data_snapshots, snapshot_items, battery_products, passports, rule_packages, rule_package_versions, rule_fields, rule_tests_runs, passport_versions, jobs, policies, policy_versions, webhook_endpoints, webhook_subscriptions, webhook_deliveries
  - **Haskell repository function signatures** (type-level contract, no implementation):
    - Pool.hs: `mkPool :: IO Pool`, `withConn :: Pool -> (Connection -> IO a) -> IO a`
    - Events.hs: `appendEvent :: Connection -> AppendEventInput -> IO (Either EventError EventId)`, `verifyChain :: Connection -> TenantId -> AggregateType -> AggregateId -> IO (Either ChainError ())`
    - Documents.hs, Facts.hs, Snapshots.hs, Rules.hs, Passports.hs, Jobs.hs, Auth.hs, Webhooks.hs: all functions with `TenantId` first parameter
  - **Event types and payload schemas**: EventType enum (e.g., PassportCreated, SnapshotSealed, RulePublished); payload as jsonb with schema version
  - **Job types and payload schemas**: JobType enum (SSOT 2.2); payload structure per job type
- `specs/05-data-layer/quickstart.md` with:
  - **How to initialize connection pool**: mkPool from ENV vars; example code
  - **How to append events**: example AppendEventInput; hash calculation; handling EVENT_VERSION_CONFLICT
  - **How to use repositories**: example queries (createDocument, insertFact, sealSnapshot, activate passport)
  - **How to verify hash chain**: verifyChain example; interpreting ChainError

**Key Decisions**:
1. **TenantId representation**: `newtype TenantId = TenantId UUID` (type safety, no runtime overhead)
2. **SQL approach**: postgresql-simple `Query` type (not Template Haskell); explicit SQL strings with `?` placeholders
3. **Repo return types**: `IO (Maybe a)` for lookups; `IO (Either DomainError a)` for operations that can fail with domain errors (e.g., SNAPSHOT_SEALED)
4. **Event payload**: `Value` (Aeson) for flexibility; schema validation deferred to event handlers
5. **Pagination**: Cursor-based (opaque cursor with timestamp + id); limit 1-200, default 50

**Dependencies**: Phase 0 research complete

**Exit Criteria**: All 25+ tables documented with CREATE TABLE DDL; all repo function signatures defined (compiles, no implementation); quickstart examples reviewed and validated; design approved.

---

### Phase 2: Connection Pool

**Objective**: Implement `BPC.DB.Pool` module with connection pool management using resource-pool; handle connection lifecycle, timeouts, and errors.

**Tasks**:
1. Create `packages/bpc-db/bpc-db.cabal` with dependencies:
   - base >= 4.17
   - bpc-core (internal dep for types)
   - postgresql-simple >= 0.7
   - resource-pool >= 0.4
   - aeson >= 2.1
   - bytestring, text, uuid, time
2. Implement `BPC.DB.Pool`:
   ```haskell
   module BPC.DB.Pool
     ( Pool
     , DBConfig(..)
     , mkPool
     , withConn
     , close
     ) where

   import Data.Pool (Pool, createPool, withResource, destroyAllResources)
   import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)

   data DBConfig = DBConfig
     { dbHost     :: Text
     , dbPort     :: Int
     , dbUser     :: Text
     , dbPassword :: Text
     , dbName     :: Text
     , dbSslMode  :: Text  -- "disable" | "require"
     , dbPoolSize :: Int
     }

   -- Reads from BPC_DB_* ENV vars
   mkPool :: DBConfig -> IO (Pool Connection)
   mkPool cfg = createPool
     (connectPostgreSQL $ mkConnString cfg)
     Database.PostgreSQL.Simple.close
     1                          -- stripes (single recommended)
     60                         -- idle connection timeout (seconds)
     (dbPoolSize cfg)           -- max connections per stripe

   withConn :: Pool Connection -> (Connection -> IO a) -> IO a
   withConn = withResource

   close :: Pool Connection -> IO ()
   close = destroyAllResources
   ```
3. Handle `DB_UNAVAILABLE` on connection failure (catch PostgreSQL exceptions; wrap in Either)
4. Write unit tests (`test/unit/PoolSpec.hs`):
   - Pool creation with valid config
   - Connection checkout and automatic return
   - Connection timeout (simulate slow query)
   - Connection failure (Postgres down → DB_UNAVAILABLE)
5. Write integration test (`test/integration/DatabaseSpec.hs`):
   - Connect to docker-compose Postgres
   - Execute simple query (SELECT 1)
   - Verify result

**Acceptance Criteria**:
- Pool creates `BPC_DB_POOL_SIZE` connections (verify with pg_stat_activity)
- `withConn` checks out connection and returns it after use (no leaks)
- Connection timeout returns `DB_UNAVAILABLE` error (catch and wrap SqlError)
- Integration test: create pool → execute query → verify result
- Unit tests pass: pool creation, checkout, timeout, failure handling
- Code coverage ≥ 80%

**Dependencies**: Phase 1 data model complete; bpc-core types available

**Exit Criteria**: Unit + integration tests pass; coverage ≥ 80%; pool lifecycle verified (create → use → close).

---

### Phase 3: Event Store (Hash Chain)

**Objective**: Implement `BPC.DB.Repos.Events` with append-only Event Store and hash chain verification per BPC-EVENT-1 (SSOT 7.4).

**Tasks**:
1. Implement `appendEvent :: Connection -> AppendEventInput -> IO (Either EventError EventId)`:
   - Calculate `payload_canonical` via `BPC.Core.CanonicalJson.canonicalEncode`
   - Calculate `payload_hash` via `BPC.Core.Hash.sha256Hex`
   - Fetch `prev_event_hash` for (tenant_id, aggregate_type, aggregate_id) with highest aggregate_version:
     ```sql
     SELECT event_hash FROM events
     WHERE tenant_id = ? AND aggregate_type = ? AND aggregate_id = ?
     ORDER BY aggregate_version DESC LIMIT 1
     ```
   - Calculate `event_hash` per BPC-EVENT-1:
     ```haskell
     event_hash = sha256Hex $ canonicalEncode $ object
       [ "tenant_id" .= tenantId
       , "aggregate_type" .= aggregateType
       , "aggregate_id" .= aggregateId
       , "aggregate_version" .= aggregateVersion
       , "event_type" .= eventType
       , "occurred_at" .= occurredAt
       , "actor_id" .= actorId
       , "payload_hash" .= payloadHash
       , "prev_event_hash" .= prevEventHash
       ]
     ```
   - INSERT with UNIQUE constraint on (tenant_id, aggregate_type, aggregate_id, aggregate_version):
     ```sql
     INSERT INTO events (event_id, tenant_id, aggregate_type, aggregate_id, aggregate_version, event_type, occurred_at, actor_id, payload, payload_canonical, payload_hash, prev_event_hash, event_hash)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
     ```
   - Return `EVENT_VERSION_CONFLICT` on constraint violation (catch SqlError with state 23505)
2. Implement `verifyChain :: Connection -> TenantId -> AggregateType -> AggregateId -> IO (Either ChainError ())`:
   - Fetch all events for aggregate ordered by aggregate_version:
     ```sql
     SELECT * FROM events
     WHERE tenant_id = ? AND aggregate_type = ? AND aggregate_id = ?
     ORDER BY aggregate_version ASC
     ```
   - Verify each `event_hash` matches calculated hash (recompute per BPC-EVENT-1)
   - Verify `prev_event_hash` links are correct (event[i].event_hash === event[i+1].prev_event_hash)
   - Return `ChainBroken` if mismatch detected (include event_id and expected/actual hashes)
3. Write property tests (`test/unit/EventsSpec.hs`):
   - `prop_appendPreservesChain`: `forall events. appendEvent e1 >> appendEvent e2 >> verifyChain === Right ()`
   - `prop_tamperingDetected`: modify event_hash → verifyChain fails with ChainBroken
4. Write integration test (`test/integration/EventsIntegrationSpec.hs`):
   - Append 100 events for single aggregate → verify chain
   - Concurrent append with same aggregate_version → EVENT_VERSION_CONFLICT
   - Append events for 10 aggregates → verify all chains

**Acceptance Criteria**:
- `appendEvent` implements BPC-EVENT-1 hash calculation (golden test with known input/output)
- Concurrent appends with same aggregate_version return `EVENT_VERSION_CONFLICT` (integration test with 2 threads)
- `verifyChain` detects tampered events (property test: modify hash → verify fails)
- Property test: hash chain is always valid after append (100 iterations)
- Integration test: append 100 events → verify chain in <1 second
- Code coverage ≥ 80%

**Dependencies**: Phase 2 pool complete; bpc-core canonical JSON + hashing available

**Exit Criteria**: Property tests pass (1000+ iterations); integration test verifies 100+ events; golden test confirms BPC-EVENT-1 compliance; coverage ≥ 80%.

---

### Phase 4: Tenant/Actor/Role Repositories

**Objective**: Implement `BPC.DB.Repos.Auth` for tenants, actors, API keys, roles, permissions, and policies.

**Tasks**:
1. Implement tenant functions:
   - `getTenant :: TenantId -> Connection -> IO (Maybe Tenant)`
   - `getTenantBySlug :: Text -> Connection -> IO (Maybe Tenant)`
   - `createTenant :: Connection -> CreateTenantInput -> IO TenantId`
2. Implement actor functions:
   - `createActor :: Connection -> TenantId -> ActorInput -> IO (Either ActorError ActorId)`
   - `getActor :: Connection -> TenantId -> ActorId -> IO (Maybe Actor)`
   - `listActors :: Connection -> TenantId -> ListOptions -> IO (Page Actor)`
3. Implement API key functions:
   - `createApiKey :: Connection -> TenantId -> ActorId -> Text -> IO (Either ApiKeyError ApiKeyId)` (hash with SHA-256(key+pepper); store prefix for identification)
   - `verifyApiKey :: Connection -> Text -> IO (Maybe (TenantId, ActorId))` (lookup by prefix, verify hash)
   - `revokeApiKey :: Connection -> TenantId -> ApiKeyId -> IO (Either ApiKeyError ())`
4. Implement role/permission functions:
   - `createRole :: Connection -> TenantId -> Text -> IO (Either RoleError RoleId)`
   - `assignRole :: Connection -> TenantId -> ActorId -> RoleId -> IO (Either RoleError ())`
   - `removeRole :: Connection -> TenantId -> ActorId -> RoleId -> IO (Either RoleError ())`
   - `getActorPermissions :: Connection -> TenantId -> ActorId -> IO [Permission]` (join actor_roles → role_permissions)
5. Write integration tests (`test/integration/AuthIntegrationSpec.hs`):
   - Create tenant → actor → API key → verify key → get permissions
   - Tenant isolation: create actor in tenant A; query with tenant B → Nothing
   - Revoke API key → verify fails
6. Write unit tests (`test/unit/AuthSpec.hs`):
   - API key hashing (SHA-256(key+pepper))
   - Permission aggregation (multiple roles)

**Acceptance Criteria**:
- All functions accept `TenantId` as first parameter (grep verification)
- Queries with wrong TenantId return Nothing (integration test)
- API key verification uses SHA-256(key+pepper) (unit test with known pepper)
- Integration test: full auth flow (tenant → actor → key → permissions)
- Tenant isolation verified: query tenant A data with tenant B → Nothing
- Code coverage ≥ 80%

**Dependencies**: Phase 3 Event Store complete (events table available)

**Exit Criteria**: Integration tests pass; tenant isolation verified; API key hashing correct; coverage ≥ 80%.

---

### Phase 5: Document/Snapshot/Fact Repositories

**Objective**: Implement `BPC.DB.Repos.Documents`, `BPC.DB.Repos.Facts`, `BPC.DB.Repos.Snapshots` with canonical storage and deterministic sealing.

**Tasks**:
1. Implement `BPC.DB.Repos.Documents`:
   - `createDocument :: Connection -> TenantId -> DocumentInput -> IO (Either DocumentError DocumentId)`
   - `uploadVersion :: Connection -> TenantId -> DocumentId -> ByteString -> Text -> IO (Either UploadError DocumentVersionId)` (calculate SHA-256 hash; UNIQUE constraint on (tenant_id, sha256) prevents duplicates)
   - `getContent :: Connection -> TenantId -> DocumentVersionId -> IO (Maybe ByteString)` (return raw bytes from content column)
   - `listVersions :: Connection -> TenantId -> DocumentId -> IO [DocumentVersion]`
2. Implement `BPC.DB.Repos.Facts`:
   - `insertFact :: Connection -> TenantId -> FactInput -> IO (Either FactError FactId)` (calculate payload_canonical via canonicalEncode; calculate payload_hash via sha256Hex; UNIQUE constraint on (tenant_id, fact_type, fact_key, schema_version, payload_hash))
   - `getFact :: Connection -> TenantId -> Text -> Text -> IO (Maybe Fact)` (lookup by fact_type and fact_key)
   - `getFactsByPrefix :: Connection -> TenantId -> Text -> Text -> IO [Fact]` (WHERE fact_type=? AND fact_key LIKE ?)
3. Implement `BPC.DB.Repos.Snapshots`:
   - `createSnapshot :: Connection -> TenantId -> Maybe Text -> IO SnapshotId` (status=BUILDING)
   - `addItem :: Connection -> TenantId -> SnapshotId -> FactId -> IO (Either SnapshotError ())` (check status != SEALED; INSERT into snapshot_items)
   - `sealSnapshot :: Connection -> TenantId -> SnapshotId -> IO (Either SealError SnapshotHash)`:
     - Load snapshot, verify status = READY (else SNAPSHOT_NOT_READY)
     - Load all items with JOIN to facts
     - Sort by (fact_type, fact_key, payload_hash_hex)
     - Build canonical per BPC-SNAPSHOT-1:
       ```haskell
       snapshotCanonical = canonicalEncode $ object
         [ "snapshot_version" .= ("BPC-SNAPSHOT-1" :: Text)
         , "snapshot_id" .= snapshotId
         , "facts" .= map (\(ft, fk, ph) -> object ["fact_type" .= ft, "fact_key" .= fk, "payload_hash" .= ph]) sortedItems
         ]
       ```
     - Calculate snapshotHash = sha256Hex snapshotCanonical
     - UPDATE: status=SEALED, sealed_at=now(), snapshot_canonical, snapshot_hash
4. Write property test (`test/unit/SnapshotsSpec.hs`):
   - `prop_sealDeterministic`: same facts (different order) → same seal hash
5. Write integration test (`test/integration/SnapshotsIntegrationSpec.hs`):
   - Create snapshot → add 10 facts → transition BUILDING → READY → seal → verify hash
   - Seal SEALED snapshot → SNAPSHOT_SEALED error
   - Add item to SEALED snapshot → SNAPSHOT_SEALED error

**Acceptance Criteria**:
- Document upload calculates SHA-256 hash (verify with known input)
- Fact insert calculates payload_canonical and payload_hash (verify with known JSON)
- Snapshot seal is deterministic (property test: 1000 iterations with shuffled facts)
- SEALED snapshot blocks `addItem` (returns `SNAPSHOT_SEALED`)
- Integration test: full flow (document → upload → facts → snapshot → seal)
- Code coverage ≥ 80%

**Dependencies**: Phase 4 Auth repos complete; bpc-core canonical JSON available

**Exit Criteria**: Property test verifies determinism (1000+ iterations); integration tests pass; seal hash matches golden; coverage ≥ 80%.

---

### Phase 6: Rules/Passport Repositories

**Objective**: Implement `BPC.DB.Repos.Rules` and `BPC.DB.Repos.Passports` with version management and status transitions.

**Tasks**:
1. Implement `BPC.DB.Repos.Rules`:
   - `createPackage :: Connection -> TenantId -> PackageInput -> IO (Either RuleError RulePackageId)`
   - `createVersion :: Connection -> TenantId -> RulePackageId -> VersionInput -> IO (Either RuleError RulePackageVersionId)` (status=DRAFT; calculate dsl_sha256 and tests_sha256)
   - `publishVersion :: Connection -> TenantId -> RulePackageVersionId -> IO (Either PublishError ())`:
     - Check latest test run: must have status=PASSED and cases >= 500 (else RULE_TESTS_FAILED)
     - UPDATE: status=PUBLISHED, published_at=now()
   - `recordTestRun :: Connection -> TenantId -> RulePackageVersionId -> TestRunInput -> IO (Either RuleError RunId)`
   - `getPublishedVersions :: Connection -> TenantId -> RulePackageId -> IO [RulePackageVersion]` (WHERE status='PUBLISHED')
2. Implement `BPC.DB.Repos.Passports`:
   - `createPassport :: Connection -> TenantId -> BatteryProductId -> IO (Either PassportError PassportId)`
   - `insertVersion :: Connection -> TenantId -> PassportId -> PassportVersionInput -> IO (Either PassportError PassportVersionId)` (status=COMPILING; store all canonical bytea columns)
   - `activate :: Connection -> TenantId -> PassportVersionId -> IO (Either ActivateError ())`:
     - Check status=SIGNED (else NOT_SIGNED)
     - Begin transaction
     - UPDATE old current_passport_version: status=SUPERSEDED, superseded_at=now()
     - UPDATE new version: status=ACTIVE, activated_at=now()
     - UPDATE passport: current_passport_version_id=new_version
     - Commit transaction
   - `revoke :: Connection -> TenantId -> PassportVersionId -> IO (Either RevokeError ())`:
     - UPDATE: status=REVOKED, revoked_at=now()
3. Write integration tests (`test/integration/RulesIntegrationSpec.hs`, `test/integration/PassportsIntegrationSpec.hs`):
   - Rule package: create → version → test run (500 cases) → publish
   - Rule package: publish without tests → RULE_TESTS_FAILED
   - Passport: create → version → activate → verify supersede (old version SUPERSEDED)
   - Passport: revoke → status=REVOKED

**Acceptance Criteria**:
- Publish blocked if test run < 500 cases (returns `RULE_TESTS_FAILED`)
- Publish blocked if test run status=FAILED (returns `RULE_TESTS_FAILED`)
- Activate sets previous version to SUPERSEDED (integration test verifies status transition)
- Activate is transactional (rollback on failure)
- Revoke sets status REVOKED with timestamp
- Integration tests verify full lifecycle (package → version → test → publish; passport → version → activate → revoke)
- Code coverage ≥ 80%

**Dependencies**: Phase 5 Snapshot repos complete; snapshots table available

**Exit Criteria**: Integration tests pass; status transitions verified; transactional activate works; coverage ≥ 80%.

---

### Phase 7: Job Queue

**Objective**: Implement `BPC.DB.Repos.Jobs` with idempotent job queue and race-free leasing using `FOR UPDATE SKIP LOCKED`.

**Tasks**:
1. Implement `enqueue :: Connection -> TenantId -> JobInput -> IO JobId`:
   - INSERT with ON CONFLICT DO NOTHING on (tenant_id, type, idempotency_key):
     ```sql
     INSERT INTO jobs (job_id, tenant_id, type, status, priority, attempts, max_attempts, idempotency_key, payload, scheduled_at)
     VALUES (?, ?, ?, 'QUEUED', ?, 0, ?, ?, ?, ?)
     ON CONFLICT (tenant_id, type, idempotency_key) DO NOTHING
     RETURNING job_id
     ```
   - If no rows returned (conflict), query existing job_id by idempotency_key
2. Implement `acquireLease :: Connection -> TenantId -> Text -> IO (Maybe Job)`:
   - Use `FOR UPDATE SKIP LOCKED` to prevent double-run:
     ```sql
     UPDATE jobs SET
       status = 'RUNNING',
       lease_owner = ?,
       lease_expires_at = now() + interval '? seconds',
       started_at = now()
     WHERE job_id = (
       SELECT job_id FROM jobs
       WHERE tenant_id = ?
         AND (status = 'QUEUED' OR (status = 'RUNNING' AND lease_expires_at < now()))
         AND scheduled_at <= now()
       ORDER BY priority DESC, scheduled_at ASC
       FOR UPDATE SKIP LOCKED
       LIMIT 1
     )
     RETURNING *
     ```
3. Implement `renewLease :: Connection -> TenantId -> JobId -> Text -> IO (Either JobError ())`:
   - UPDATE lease_expires_at WHERE lease_owner matches (prevents stealing)
4. Implement `complete :: Connection -> TenantId -> JobId -> IO (Either JobError ())`:
   - UPDATE: status='SUCCEEDED', finished_at=now()
5. Implement `fail :: Connection -> TenantId -> JobId -> Value -> IO (Either JobError ())`:
   - Load job to check attempts
   - If attempts < max_attempts:
     - Calculate backoff: `2 ^ attempts` seconds (capped at 1024)
     - UPDATE: status='QUEUED', attempts=attempts+1, scheduled_at=now()+backoff, last_error=?
   - Else:
     - UPDATE: status='DEAD_LETTER', finished_at=now(), last_error=?
6. Write integration test (`test/integration/JobsIntegrationSpec.hs`):
   - Enqueue job → lease → complete
   - Idempotency: enqueue twice with same key → same job_id
   - Race condition test: 2 workers try to lease same job concurrently → only 1 succeeds (spawn 2 threads, both call acquireLease, verify only 1 gets Job)
   - Backoff: fail job 3 times → verify scheduled_at increases exponentially
   - Dead letter: fail job max_attempts times → status=DEAD_LETTER

**Acceptance Criteria**:
- Idempotency: duplicate enqueue returns existing JobId (integration test)
- Leasing is race-free (integration test with 2 concurrent workers; only 1 gets job)
- Failed jobs retry with backoff (verify scheduled_at = now() + 2^attempts)
- DEAD_LETTER jobs after max_attempts exhausted (verify status transition)
- Lease renewal works (update lease_expires_at)
- Integration test: full lifecycle (enqueue → lease → renew → complete; enqueue → lease → fail → retry → fail → ... → DEAD_LETTER)
- Code coverage ≥ 80%

**Dependencies**: Phase 6 Passport repos complete; jobs table available

**Exit Criteria**: Race condition test passes (1000 iterations, no double-run); integration tests verify leasing and backoff; coverage ≥ 80%.

---

### Phase 8: Hash Chain Verification & Tooling

**Objective**: Implement comprehensive hash chain verification and CLI tooling for auditing.

**Tasks**:
1. Implement `verifyAllChains :: Connection -> TenantId -> IO (Either ChainError VerifyReport)`:
   - Query all distinct (aggregate_type, aggregate_id) for tenant
   - For each aggregate, call `verifyChain`
   - Aggregate results into report: (total_aggregates, total_events, broken_chains)
   - If any chain broken, return Left with details
2. Add CLI command in bpc-cli package: `bpc-cli verify-chains --tenant-id=UUID`:
   - Parse tenant-id from args
   - Create pool, connect to DB
   - Call `verifyAllChains`
   - Output report to stdout (JSON or pretty-printed)
3. Write property test (`test/unit/EventsSpec.hs`):
   - `prop_tamperingAlwaysDetected`: append N events → tamper 1 random event → verify detects (100 iterations with QuickCheck)
4. Write integration test (`test/integration/EventsIntegrationSpec.hs`):
   - Create 10 aggregates with 100 events each (1000 total events)
   - Verify all chains pass
   - Tamper 1 event (modify event_hash)
   - Verify reports broken chain with correct event_id

**Acceptance Criteria**:
- Verification detects single tampered event in 1000+ chain (integration test)
- CLI command outputs verification report (test with real DB)
- Property test: tampering always detected (100 iterations, no false negatives)
- Performance: verify 1000 events in <5 seconds (benchmark)
- Report includes: tenant_id, total_aggregates, total_events, broken_chains (with event_id, expected_hash, actual_hash)
- Code coverage ≥ 80%

**Dependencies**: Phase 7 Job queue complete; all repos available

**Exit Criteria**: Property test passes (100+ iterations); CLI verification works; integration test verifies 1000+ events; performance benchmark passes; coverage ≥ 80%.

---

### Phase 9: Webhooks Repository

**Objective**: Implement `BPC.DB.Repos.Webhooks` for webhook endpoints, subscriptions, and deliveries.

**Tasks**:
1. Implement webhook endpoint functions:
   - `createEndpoint :: Connection -> TenantId -> Text -> Text -> IO (Either WebhookError WebhookEndpointId)` (url, secret_base64)
   - `getEndpoint :: Connection -> TenantId -> WebhookEndpointId -> IO (Maybe WebhookEndpoint)`
   - `deactivateEndpoint :: Connection -> TenantId -> WebhookEndpointId -> IO (Either WebhookError ())`
2. Implement subscription functions:
   - `subscribe :: Connection -> TenantId -> WebhookEndpointId -> Text -> IO (Either WebhookError WebhookSubscriptionId)` (event_type)
   - `unsubscribe :: Connection -> TenantId -> WebhookSubscriptionId -> IO (Either WebhookError ())`
   - `getSubscriptions :: Connection -> TenantId -> WebhookEndpointId -> IO [WebhookSubscription]`
3. Implement delivery functions:
   - `recordDelivery :: Connection -> TenantId -> WebhookEndpointId -> EventId -> IO WebhookDeliveryId`
   - `markDelivered :: Connection -> TenantId -> WebhookDeliveryId -> IO (Either WebhookError ())`
   - `markFailed :: Connection -> TenantId -> WebhookDeliveryId -> Text -> IO (Either WebhookError ())`
   - `getPendingDeliveries :: Connection -> TenantId -> IO [WebhookDelivery]`
4. Write integration test (`test/integration/WebhooksIntegrationSpec.hs`):
   - Create endpoint → subscribe to event type → record delivery → mark delivered
   - Deactivate endpoint → deliveries still recorded but not sent

**Acceptance Criteria**:
- All functions tenant-scoped
- Integration test: full flow (endpoint → subscribe → delivery)
- Code coverage ≥ 80%

**Dependencies**: Phase 8 verification complete

**Exit Criteria**: Integration tests pass; coverage ≥ 80%.

---

## Verification Checklist

### Code Quality

- [ ] fourmolu formatting passes: `fourmolu -m check $(git ls-files 'packages/bpc-db/**/*.hs')`
- [ ] hlint passes: `hlint -h hlint.yaml $(git ls-files 'packages/bpc-db/**/*.hs')`
- [ ] GHC warnings as errors: `-Wall -Wcompat -Werror` in cabal file
- [ ] All imports comply with Module Import Matrix (SSOT 3.4.1): BPC.DB.* can import BPC.Core.* only (no BPC.API.* or BPC.Worker.*)
- [ ] No orphan instances
- [ ] All public functions have Haddock comments

### Test Coverage

- [ ] Unit tests: ≥ 80% coverage for bpc-db (measure with hpc or cabal test --enable-coverage)
- [ ] Property tests: determinism (snapshot seal), hash chain verification, tampering detection
- [ ] Integration tests: docker-compose Postgres 16, all repositories, full lifecycles
- [ ] Race condition tests: job leasing with concurrent workers (spawn 2 threads, verify only 1 gets job)
- [ ] Golden tests: BPC-EVENT-1 hash calculation, BPC-SNAPSHOT-1 seal format

### Constitution Compliance

- [ ] **Audit Trail (IV)**: All writes append Event to Event Store (verify in integration tests)
- [ ] **Immutability (III)**: SEALED snapshots block modification (test sealSnapshot >> addItem → SNAPSHOT_SEALED)
- [ ] **Canonical Storage (II)**: payload_canonical, proof_canonical, receipt_canonical, snapshot_canonical stored as bytea (verify schema)
- [ ] **Layered Architecture (V)**: No imports of BPC.API.* or BPC.Worker.* (grep verification + CI dependency check)
- [ ] **Tenant Isolation**: All repo functions filter by TenantId (grep "WHERE tenant_id" in all queries; integration test verifies isolation)

### Functional Requirements (from spec.md)

- [ ] **FR-001**: Pool manages BPC_DB_POOL_SIZE connections (verify with pg_stat_activity)
- [ ] **FR-002**: All repo functions accept TenantId first parameter (grep verification)
- [ ] **FR-003**: Event Store implements BPC-EVENT-1 hash chain (golden test)
- [ ] **FR-004**: EVENT_VERSION_CONFLICT on concurrent append (integration test with 2 threads)
- [ ] **FR-005**: Documents store SHA-256 hash (verify in integration test)
- [ ] **FR-006**: Facts store payload_canonical and payload_hash (verify in integration test)
- [ ] **FR-007**: Snapshots seal per BPC-SNAPSHOT-1 (golden test)
- [ ] **FR-008**: SEALED snapshots immutable (test addItem → SNAPSHOT_SEALED)
- [ ] **FR-009**: Jobs support idempotency key (test duplicate enqueue → same job_id)
- [ ] **FR-010**: Jobs support lease with expiry/renewal (integration test)
- [ ] **FR-011**: PassportVersion status transitions enforced (COMPILING → SIGNED → ACTIVE → SUPERSEDED/REVOKED)

### Success Criteria (from spec.md)

- [ ] **SC-001**: All repo functions accept TenantId first parameter (grep verification)
- [ ] **SC-002**: Event chain verification works for 10,000+ events (benchmark: verify 10k events in <5 seconds)
- [ ] **SC-003**: Snapshot seal is deterministic (property test: 1000 iterations)
- [ ] **SC-004**: Job leasing is race-condition-free (integration test: 1000 iterations, no double-run)
- [ ] **SC-005**: Integration tests with docker-compose Postgres pass (CI)
- [ ] **SC-006**: Code coverage for bpc-db ≥ 80% (hpc report)

### Documentation

- [ ] All public functions have Haddock comments (verify with `cabal haddock`)
- [ ] Quickstart guide verified with real examples (run examples in ghci)
- [ ] Data model documented (DDL + repo signatures)
- [ ] Research findings documented (research.md approved)
- [ ] Migration scripts tested (run dbmate up; verify schema matches DDL)

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **Hash chain verification slow for 10k+ events** | Medium | Medium | Index on (tenant_id, aggregate_type, aggregate_id, aggregate_version); batch verification in chunks; parallelize across aggregates using Async |
| **Connection pool exhaustion under load** | Medium | High | Monitor pool metrics (withResource duration); configurable BPC_DB_POOL_SIZE; timeout handling with DB_UNAVAILABLE; auto-scaling pool size based on load |
| **Race condition in job leasing** | Low | High | FOR UPDATE SKIP LOCKED guarantees atomicity; integration tests with concurrent workers (1000 iterations); monitor double-run metrics in production |
| **Tenant isolation bypass (missing WHERE clause)** | Low | Critical | Code review checklist (every query must have WHERE tenant_id=?); integration tests verify isolation (query with wrong tenant → Nothing); grep for SELECT.*FROM.*WHERE patterns in CI |
| **Event version conflicts under high concurrency** | Medium | Low | Retryable error (EVENT_VERSION_CONFLICT); backoff strategy (exponential); normal for event sourcing; monitor conflict rate in production |
| **DB schema drift (migrations out of sync with DDL)** | Low | Medium | dbmate schema version check; CI verifies migrations produce expected schema (compare pg_dump output); integration tests catch missing tables/columns |
| **Canonical encoding non-determinism** | Low | Critical | Golden tests for canonical JSON (BPC-CJSON-1); property tests verify sorting and formatting; use bpc-core canonicalEncode (already tested) |

## Rollout Plan

### Development

1. **Phase 0-1**: Research + Data Model (3-4 days, no code, approval gate)
2. **Phase 2**: Connection Pool (2 days, standalone, no dependencies)
3. **Phase 3**: Event Store (4 days, depends on Pool, critical for Audit Trail)
4. **Phase 4**: Auth Repos (3 days, depends on Event Store)
5. **Phase 5**: Document/Snapshot/Fact Repos (5 days, depends on Auth, can be parallelized into 3 modules)
6. **Phase 6**: Rules/Passport Repos (4 days, depends on Snapshots)
7. **Phase 7**: Job Queue (4 days, depends on Passports, critical for Worker)
8. **Phase 8**: Verification Tooling (2 days, depends on Event Store, adds CLI)
9. **Phase 9**: Webhooks Repo (2 days, depends on Events, optional for MVP)

**Parallelization Opportunities**:
- Phase 5 can split: Documents.hs (1 dev, 2 days), Facts.hs (1 dev, 2 days), Snapshots.hs (1 dev, 3 days)
- Phase 6 can split: Rules.hs (1 dev, 2 days), Passports.hs (1 dev, 3 days)

**Total**: 28-30 days sequential; 20-22 days with 2 devs; 18-20 days with 3 devs

### Integration

1. Merge to `main` after each phase passes CI (fourmolu, hlint, tests, coverage ≥ 80%)
2. Integration tests run in CI with docker-compose Postgres (migrations applied via dbmate)
3. Coverage gate: ≥ 80% (blocks merge if failed)
4. Manual review: verify Constitution compliance (checklist in PR template)

### Deployment

1. **Migrations**: Run `./scripts/migrate.sh` (before deploying api/worker; idempotent with dbmate)
2. **Seed Data**: Run `./scripts/seed-dev.sh` for dev/staging (creates dev tenant, roles, permissions)
3. **Deploy API + Worker**: Update dependencies to use new bpc-db package
4. **Verify Health**: `GET /v1/health` (checks DB connectivity with simple query)
5. **Monitor**: Watch pool metrics (active connections, wait time), query latency, event append rate

## Dependencies

### External Dependencies

- **postgresql-simple** 0.7+ (BSD-3-Clause license) - Database driver; type-safe queries
- **resource-pool** 0.4+ (BSD-3-Clause license) - Connection pooling; resource lifecycle management
- **aeson** 2.1+ (BSD-3-Clause license) - JSON encoding/decoding; used for event payloads and job payloads
- **bytestring** (BSD-3-Clause license) - Binary data (content, canonical bytes, hashes)
- **text** (BSD-2-Clause license) - Text handling (queries, field names)
- **uuid** (BSD-3-Clause license) - UUID type for IDs
- **time** (BSD-3-Clause license) - Timestamp handling (UTCTime, NominalDiffTime)
- **cryptonite** (BSD-3-Clause license, via bpc-core) - SHA-256 hashing
- **hspec** (MIT license, test only) - Test framework
- **QuickCheck** (BSD-3-Clause license, test only) - Property testing

### Internal Dependencies

- **bpc-core** - Domain types (TenantId, ActorId, etc.), canonical JSON (canonicalEncode/Decode), hashing (sha256Hex, base32NoPad)

### Infrastructure Dependencies

- **PostgreSQL 16** - Database server (docker image: postgres:16)
- **docker-compose** - Integration test environment (Postgres on port 55432)
- **dbmate** - Migration tool (reads DATABASE_URL, applies migrations/001_init.sql, etc.)

## Timeline Estimate

| Phase | Effort (days) | Dependencies | Parallelizable? |
|-------|--------------|--------------|-----------------|
| Phase 0: Research | 2 | None | No |
| Phase 1: Data Model | 2 | Phase 0 | No |
| Phase 2: Pool | 2 | Phase 1 | No |
| Phase 3: Event Store | 4 | Phase 2 | No |
| Phase 4: Auth Repos | 3 | Phase 3 | No |
| Phase 5: Doc/Snap/Fact Repos | 5 (2+2+3) | Phase 4 | Yes (3 modules) |
| Phase 6: Rules/Passport Repos | 4 (2+3) | Phase 5 | Yes (2 modules) |
| Phase 7: Job Queue | 4 | Phase 6 | No |
| Phase 8: Verification | 2 | Phase 7 | No |
| Phase 9: Webhooks | 2 | Phase 8 | No |
| **Total Sequential** | **30 days** | - | - |
| **Total with 2 Devs** | **22 days** | Phase 5-6 parallelized | - |
| **Total with 3 Devs** | **20 days** | Phase 5-6 fully parallelized | - |

**Critical Path**: Phase 0 → Phase 1 → Phase 2 → Phase 3 → Phase 4 → Phase 5 (Snapshots) → Phase 6 (Passports) → Phase 7 → Phase 8 (18 days minimum)

## Open Questions

1. **Pool Striping**: Should we use separate pools for read vs write queries? (Impacts Phase 2)
   - **Trade-off**: Read pool can tolerate higher latency; write pool needs low latency
   - **Recommendation**: Start with single pool; split if contention observed in production
   - **Defer to**: Phase 0 research

2. **Event Retention**: Should we implement time-based partitioning for events table? (Impacts Phase 3)
   - **Trade-off**: Partitioning improves query performance but adds complexity
   - **Recommendation**: Use retention policy (BPC_RETENTION_EVENTS_DAYS=5475 for 15 years); partition if events table >10M rows
   - **Defer to**: Phase 0 research

3. **Snapshot Seal Caching**: Should sealed snapshot hash be cached in-memory? (Impacts Phase 5)
   - **Trade-off**: Caching reduces DB queries but requires cache invalidation
   - **Recommendation**: No caching in bpc-db (stateless); API layer can cache if needed
   - **Defer to**: Phase 1 data model

4. **Job Priority Updates**: How to handle priority updates for already-queued jobs? (Impacts Phase 7)
   - **Trade-off**: UPDATE query vs re-enqueue (idempotency conflict)
   - **Recommendation**: Provide `updatePriority :: Connection -> TenantId -> JobId -> Int -> IO ()` function
   - **Defer to**: Phase 1 data model

5. **Chain Verification Performance**: Should we parallelize verification across aggregates? (Impacts Phase 8)
   - **Trade-off**: Parallelization improves speed but increases DB load (connection pool exhaustion)
   - **Recommendation**: Sequential for single tenant; parallel for all tenants (use Async with pool)
   - **Defer to**: Phase 0 research

**Resolution Strategy**: All questions deferred to Phase 0 research; document decision with rationale in research.md; update plan.md if design changes.
