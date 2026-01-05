# Tasks: Data Layer

**Input**: Design documents from `specs/05-data-layer/`
**Prerequisites**: plan.md (required), spec.md (required), data-model.md
**Phase**: P2 - Database access layer, depends on 01-foundation + 02-core-primitives
**Package**: bpc-db (has IO, can use postgresql-simple)

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US9)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `packages/bpc-db/src/BPC/DB/`
- **Tests**: `packages/bpc-db/test/`
- **Migrations**: `migrations/`

---

## Phase 1: Setup (Package Configuration)

**Purpose**: Configure bpc-db package with required dependencies

- [x] T001 Create `packages/bpc-db/bpc-db.cabal` with postgresql-simple, resource-pool dependencies
- [x] T002 Add bpc-core internal dependency to bpc-db.cabal
- [x] T003 Create directory structure: `packages/bpc-db/src/BPC/DB/Repos/`
- [x] T004 Create directory structure: `packages/bpc-db/test/unit/` and `test/integration/`
- [x] T005 [P] Add hspec, QuickCheck test dependencies to bpc-db.cabal
- [x] T006 Create `packages/bpc-db/src/BPC/DB.hs` re-export module

**Checkpoint**: `cabal build bpc-db` succeeds with dependencies

---

## Phase 2: Foundational (Error Types & Pool Types)

**Purpose**: Define shared types and error types - BLOCKS all user stories

- [x] T007 Create `packages/bpc-db/src/BPC/DB/Error.hs` with DBError sum type
- [x] T008 Add EventError to Error.hs (EVENT_VERSION_CONFLICT, ChainBroken)
- [x] T009 Add SnapshotError to Error.hs (SNAPSHOT_SEALED, SNAPSHOT_NOT_READY)
- [x] T010 Add JobError to Error.hs (JOB_NOT_FOUND, LEASE_EXPIRED)
- [x] T011 Add domain-specific errors (DocumentError, FactError, RuleError, PassportError) to Error.hs
- [x] T012 Create `packages/bpc-db/src/BPC/DB/Pool.hs` with DBConfig data type
- [x] T013 Export all error and pool types from BPC.DB module

**Checkpoint**: Error types compile; Pool types ready

---

## Phase 3: User Story 1 - Connection Pool Management (Priority: P1) MVP

**Goal**: Configurable DB connection pool using resource-pool

**Independent Test**: Create pool, checkout connection, execute simple query

### Tests for User Story 1

- [x] T014 [P] [US1] Create `packages/bpc-db/test/unit/PoolSpec.hs` with test scaffold
- [x] T015 [US1] Add test: Pool creation with valid config in PoolSpec.hs
- [x] T016 [US1] Add test: Connection checkout and return in PoolSpec.hs
- [x] T017 [US1] Add test: DB connection failure → DB_UNAVAILABLE in PoolSpec.hs
- [x] T018 [P] [US1] Create `packages/bpc-db/test/integration/DatabaseSpec.hs` with test scaffold
- [x] T019 [US1] Add integration test: Connect to docker-compose Postgres in DatabaseSpec.hs
- [x] T020 [US1] Add integration test: Execute SELECT 1 query in DatabaseSpec.hs

### Implementation for User Story 1

- [x] T021 [US1] Implement `mkPool :: DBConfig -> IO (Pool Connection)` in Pool.hs
- [x] T022 [US1] Implement `withConn :: Pool Connection -> (Connection -> IO a) -> IO a` in Pool.hs
- [x] T023 [US1] Implement `close :: Pool Connection -> IO ()` in Pool.hs
- [x] T024 [US1] Implement connection string builder from DBConfig in Pool.hs
- [x] T025 [US1] Handle SqlError and wrap as DB_UNAVAILABLE in Pool.hs
- [x] T026 [US1] Export mkPool, withConn, close from BPC.DB module

**Checkpoint**: Pool creates BPC_DB_POOL_SIZE connections; withConn works

---

## Phase 4: User Story 2 - Event Store (Priority: P1)

**Goal**: Append-only event store with BPC-EVENT-1 hash chain for tamper detection

**Independent Test**: Append events, verify hash chain, detect tampering

### Tests for User Story 2

- [x] T027 [P] [US2] Create `packages/bpc-db/test/unit/EventsSpec.hs` with test scaffold
- [x] T028 [US2] Add golden test: BPC-EVENT-1 hash calculation in EventsSpec.hs
- [x] T029 [US2] Add property test: `prop_appendPreservesChain` in EventsSpec.hs
- [x] T030 [US2] Add property test: `prop_tamperingDetected` in EventsSpec.hs
- [x] T031 [P] [US2] Create `packages/bpc-db/test/integration/EventsIntegrationSpec.hs` with scaffold
- [x] T032 [US2] Add integration test: Append 100 events, verify chain in EventsIntegrationSpec.hs
- [x] T033 [US2] Add integration test: Concurrent append → EVENT_VERSION_CONFLICT in EventsIntegrationSpec.hs
- [x] T034 [US2] Add integration test: Tamper event → verifyChain detects in EventsIntegrationSpec.hs

### Implementation for User Story 2

- [x] T035 [US2] Create `packages/bpc-db/src/BPC/DB/Repos/Events.hs` module
- [x] T036 [US2] Define AppendEventInput data type in Events.hs
- [x] T037 [US2] Implement `appendEvent :: Connection -> AppendEventInput -> IO (Either EventError EventId)` in Events.hs
- [x] T038 [US2] Implement hash calculation per BPC-EVENT-1 in appendEvent
- [x] T039 [US2] Implement prev_event_hash lookup query in appendEvent
- [x] T040 [US2] Handle constraint violation → EVENT_VERSION_CONFLICT in appendEvent
- [x] T041 [US2] Implement `verifyChain :: Connection -> TenantId -> AggregateType -> AggregateId -> IO (Either ChainError ())` in Events.hs
- [x] T042 [US2] Export appendEvent, verifyChain from BPC.DB module

**Checkpoint**: BPC-EVENT-1 hash chain works; tampering detected

---

## Phase 5: User Story 3 - Tenant Isolation (Priority: P1)

**Goal**: All data access is tenant-scoped with TenantId filtering

**Independent Test**: Query with wrong TenantId returns empty/Nothing

### Tests for User Story 3

- [x] T043 [P] [US3] Create `packages/bpc-db/test/integration/TenantIsolationSpec.hs` with scaffold
- [x] T044 [US3] Add test: Create data in tenant A, query with tenant B → Nothing in TenantIsolationSpec.hs
- [x] T045 [US3] Add test: All repo functions require TenantId parameter in TenantIsolationSpec.hs

### Implementation for User Story 3

- [x] T046 [US3] Ensure all repo function signatures have TenantId as first parameter
- [x] T047 [US3] Ensure all SQL queries have WHERE tenant_id=$1 clause
- [x] T048 [US3] Add grep verification for tenant isolation in test suite

**Checkpoint**: Tenant isolation enforced; cross-tenant queries return empty

---

## Phase 6: User Story 4 - Document Repository (Priority: P1)

**Goal**: Store and retrieve documents with SHA-256 content hash

**Independent Test**: Upload document, retrieve content, verify hash

### Tests for User Story 4

- [x] T049 [P] [US4] Add document tests to `packages/bpc-db/test/integration/DocumentsIntegrationSpec.hs`
- [x] T050 [US4] Add test: Create document in DocumentsIntegrationSpec.hs
- [x] T051 [US4] Add test: Upload version with SHA-256 hash calculation in DocumentsIntegrationSpec.hs
- [x] T052 [US4] Add test: Get content returns original bytes in DocumentsIntegrationSpec.hs
- [x] T053 [US4] Add test: Duplicate SHA-256 → constraint violation in DocumentsIntegrationSpec.hs

### Implementation for User Story 4

- [x] T054 [US4] Create `packages/bpc-db/src/BPC/DB/Repos/Documents.hs` module
- [x] T055 [US4] Implement `createDocument :: Connection -> TenantId -> DocumentInput -> IO (Either DocumentError DocumentId)` in Documents.hs
- [x] T056 [US4] Implement `uploadVersion :: Connection -> TenantId -> DocumentId -> ByteString -> Text -> IO (Either UploadError DocumentVersionId)` in Documents.hs
- [x] T057 [US4] Implement `getContent :: Connection -> TenantId -> DocumentVersionId -> IO (Maybe ByteString)` in Documents.hs
- [x] T058 [US4] Export document functions from BPC.DB module

**Checkpoint**: Document upload stores SHA-256 hash; content retrieval works

---

## Phase 7: User Story 5 - Facts Repository (Priority: P1)

**Goal**: Store facts with canonical bytes and payload hash

**Independent Test**: Insert fact, verify canonical and hash fields

### Tests for User Story 5

- [x] T059 [P] [US5] Add fact tests to `packages/bpc-db/test/integration/FactsIntegrationSpec.hs`
- [x] T060 [US5] Add test: Insert fact calculates payload_canonical in FactsIntegrationSpec.hs
- [x] T061 [US5] Add test: Insert fact calculates payload_hash in FactsIntegrationSpec.hs
- [x] T062 [US5] Add test: getFact by type/key in FactsIntegrationSpec.hs
- [x] T063 [US5] Add test: getFactsByPrefix returns matching facts in FactsIntegrationSpec.hs
- [x] T064 [US5] Add test: Duplicate fact (same hash) → constraint violation in FactsIntegrationSpec.hs

### Implementation for User Story 5

- [x] T065 [US5] Create `packages/bpc-db/src/BPC/DB/Repos/Facts.hs` module
- [x] T066 [US5] Implement `insertFact :: Connection -> TenantId -> FactInput -> IO (Either FactError FactId)` in Facts.hs
- [x] T067 [US5] Use canonicalEncode and sha256Hex for payload_canonical/hash in insertFact
- [x] T068 [US5] Implement `getFact :: Connection -> TenantId -> Text -> Text -> IO (Maybe Fact)` in Facts.hs
- [x] T069 [US5] Implement `getFactsByPrefix :: Connection -> TenantId -> Text -> Text -> IO [Fact]` in Facts.hs
- [x] T070 [US5] Export fact functions from BPC.DB module

**Checkpoint**: Facts stored with canonical bytes; lookup by type/key/prefix works

---

## Phase 8: User Story 6 - Snapshots Repository (Priority: P1)

**Goal**: Create snapshots, add items, seal with BPC-SNAPSHOT-1 deterministic hash

**Independent Test**: Create→add items→seal; same facts = same hash

### Tests for User Story 6

- [x] T071 [P] [US6] Create `packages/bpc-db/test/unit/SnapshotsSpec.hs` with scaffold
- [x] T072 [US6] Add property test: `prop_sealDeterministic` (same facts, different order → same hash) in SnapshotsSpec.hs
- [x] T073 [P] [US6] Create `packages/bpc-db/test/integration/SnapshotsIntegrationSpec.hs` with scaffold
- [x] T074 [US6] Add test: Create snapshot with status BUILDING in SnapshotsIntegrationSpec.hs
- [x] T075 [US6] Add test: Add items to snapshot in SnapshotsIntegrationSpec.hs
- [x] T076 [US6] Add test: Seal snapshot → status SEALED, hash calculated in SnapshotsIntegrationSpec.hs
- [x] T077 [US6] Add test: Add item to SEALED snapshot → SNAPSHOT_SEALED error in SnapshotsIntegrationSpec.hs

### Implementation for User Story 6

- [x] T078 [US6] Create `packages/bpc-db/src/BPC/DB/Repos/Snapshots.hs` module
- [x] T079 [US6] Implement `createSnapshot :: Connection -> TenantId -> Maybe Text -> IO SnapshotId` in Snapshots.hs
- [x] T080 [US6] Implement `addItem :: Connection -> TenantId -> SnapshotId -> FactId -> IO (Either SnapshotError ())` in Snapshots.hs
- [x] T081 [US6] Implement status check (not SEALED) before addItem in Snapshots.hs
- [x] T082 [US6] Implement `sealSnapshot :: Connection -> TenantId -> SnapshotId -> IO (Either SealError SnapshotHash)` in Snapshots.hs
- [x] T083 [US6] Implement BPC-SNAPSHOT-1 canonical format in sealSnapshot
- [x] T084 [US6] Sort facts by (fact_type, fact_key, payload_hash) for determinism in sealSnapshot
- [x] T085 [US6] Export snapshot functions from BPC.DB module

**Checkpoint**: Snapshot seal is deterministic; SEALED snapshots immutable

---

## Phase 9: User Story 7 - Passports Repository (Priority: P1)

**Goal**: Manage passports and versions with activate/revoke lifecycle

**Independent Test**: Create→version→activate; old version becomes SUPERSEDED

### Tests for User Story 7

- [x] T086 [P] [US7] Create `packages/bpc-db/test/integration/PassportsIntegrationSpec.hs` with scaffold
- [x] T087 [US7] Add test: Create passport for battery product in PassportsIntegrationSpec.hs
- [x] T088 [US7] Add test: Insert version with all canonical artifacts in PassportsIntegrationSpec.hs
- [x] T089 [US7] Add test: Activate version → status ACTIVE in PassportsIntegrationSpec.hs
- [x] T090 [US7] Add test: Activate supersedes previous version in PassportsIntegrationSpec.hs
- [x] T091 [US7] Add test: Revoke version → status REVOKED in PassportsIntegrationSpec.hs

### Implementation for User Story 7

- [x] T092 [US7] Create `packages/bpc-db/src/BPC/DB/Repos/Passports.hs` module
- [x] T093 [US7] Implement `createPassport :: Connection -> TenantId -> BatteryProductId -> IO (Either PassportError PassportId)` in Passports.hs
- [x] T094 [US7] Implement `insertVersion :: Connection -> TenantId -> PassportId -> PassportVersionInput -> IO (Either PassportError PassportVersionId)` in Passports.hs
- [x] T095 [US7] Implement `activate :: Connection -> TenantId -> PassportVersionId -> IO (Either ActivateError ())` in Passports.hs
- [x] T096 [US7] Implement transactional activate (supersede old, activate new) in Passports.hs
- [x] T097 [US7] Implement `revoke :: Connection -> TenantId -> PassportVersionId -> IO (Either RevokeError ())` in Passports.hs
- [x] T098 [US7] Export passport functions from BPC.DB module

**Checkpoint**: Passport lifecycle works; activate is transactional

---

## Phase 10: User Story 8 - Jobs Repository (Priority: P1)

**Goal**: Job queue with idempotent enqueue and race-free leasing

**Independent Test**: 2 workers compete for lease → only 1 gets job

### Tests for User Story 8

- [x] T099 [P] [US8] Create `packages/bpc-db/test/unit/JobsSpec.hs` with scaffold
- [x] T100 [US8] Add test: Backoff calculation (2^attempts) in JobsSpec.hs
- [x] T101 [P] [US8] Create `packages/bpc-db/test/integration/JobsIntegrationSpec.hs` with scaffold
- [x] T102 [US8] Add test: Enqueue job → status QUEUED in JobsIntegrationSpec.hs
- [x] T103 [US8] Add test: Idempotent enqueue (same key → same job_id) in JobsIntegrationSpec.hs
- [x] T104 [US8] Add test: Acquire lease → status RUNNING in JobsIntegrationSpec.hs
- [x] T105 [US8] Add test: Race condition (2 threads → only 1 gets job) in JobsIntegrationSpec.hs
- [x] T106 [US8] Add test: Complete job → status SUCCEEDED in JobsIntegrationSpec.hs
- [x] T107 [US8] Add test: Fail job → retry with backoff in JobsIntegrationSpec.hs
- [x] T108 [US8] Add test: Exceed max_attempts → DEAD_LETTER in JobsIntegrationSpec.hs

### Implementation for User Story 8

- [x] T109 [US8] Create `packages/bpc-db/src/BPC/DB/Repos/Jobs.hs` module
- [x] T110 [US8] Implement `enqueue :: Connection -> TenantId -> JobInput -> IO JobId` in Jobs.hs
- [x] T111 [US8] Use ON CONFLICT DO NOTHING for idempotency in enqueue
- [x] T112 [US8] Implement `acquireLease :: Connection -> TenantId -> Text -> IO (Maybe Job)` in Jobs.hs
- [x] T113 [US8] Use FOR UPDATE SKIP LOCKED for race-free leasing in acquireLease
- [x] T114 [US8] Implement `renewLease :: Connection -> TenantId -> JobId -> Text -> IO (Either JobError ())` in Jobs.hs
- [x] T115 [US8] Implement `complete :: Connection -> TenantId -> JobId -> IO (Either JobError ())` in Jobs.hs
- [x] T116 [US8] Implement `fail :: Connection -> TenantId -> JobId -> Value -> IO (Either JobError ())` in Jobs.hs
- [x] T117 [US8] Implement exponential backoff (2^attempts, capped at 1024) in fail
- [x] T118 [US8] Implement DEAD_LETTER transition when max_attempts exceeded in fail
- [x] T119 [US8] Export job functions from BPC.DB module

**Checkpoint**: Job leasing race-free; backoff works; DEAD_LETTER transition

---

## Phase 11: User Story 9 - Rules Repository (Priority: P1)

**Goal**: Manage rule packages and versions with publish validation

**Independent Test**: Package→version→test run (500 cases)→publish

### Tests for User Story 9

- [x] T120 [P] [US9] Create `packages/bpc-db/test/integration/RulesIntegrationSpec.hs` with scaffold
- [x] T121 [US9] Add test: Create rule package in RulesIntegrationSpec.hs
- [x] T122 [US9] Add test: Create version with DSL hash in RulesIntegrationSpec.hs
- [x] T123 [US9] Add test: Record test run in RulesIntegrationSpec.hs
- [x] T124 [US9] Add test: Publish with >= 500 test cases in RulesIntegrationSpec.hs
- [x] T125 [US9] Add test: Publish without tests → RULE_TESTS_FAILED in RulesIntegrationSpec.hs
- [x] T126 [US9] Add test: Publish with < 500 cases → RULE_TESTS_FAILED in RulesIntegrationSpec.hs

### Implementation for User Story 9

- [x] T127 [US9] Create `packages/bpc-db/src/BPC/DB/Repos/Rules.hs` module
- [x] T128 [US9] Implement `createPackage :: Connection -> TenantId -> PackageInput -> IO (Either RuleError RulePackageId)` in Rules.hs
- [x] T129 [US9] Implement `createVersion :: Connection -> TenantId -> RulePackageId -> VersionInput -> IO (Either RuleError RulePackageVersionId)` in Rules.hs
- [x] T130 [US9] Calculate dsl_sha256 and tests_sha256 in createVersion
- [x] T131 [US9] Implement `publishVersion :: Connection -> TenantId -> RulePackageVersionId -> IO (Either PublishError ())` in Rules.hs
- [x] T132 [US9] Check test run >= 500 cases before publish in publishVersion
- [x] T133 [US9] Implement `recordTestRun :: Connection -> TenantId -> RulePackageVersionId -> TestRunInput -> IO (Either RuleError RunId)` in Rules.hs
- [x] T134 [US9] Export rule functions from BPC.DB module

**Checkpoint**: Publish blocked without >= 500 test cases

---

## Phase 12: Auth Repository

**Purpose**: Tenant, actor, API key, role, and permission management

- [x] T135 [P] Create `packages/bpc-db/src/BPC/DB/Repos/Auth.hs` module
- [x] T136 Implement getTenant, getTenantBySlug, createTenant in Auth.hs
- [x] T137 Implement createActor, getActor, listActors in Auth.hs
- [x] T138 Implement createApiKey with SHA-256(key+pepper) in Auth.hs
- [x] T139 Implement verifyApiKey (lookup by prefix, verify hash) in Auth.hs
- [x] T140 Implement revokeApiKey in Auth.hs
- [x] T141 Implement createRole, assignRole, removeRole in Auth.hs
- [x] T142 Implement getActorPermissions (join actor_roles → role_permissions) in Auth.hs
- [x] T143 [P] Create `packages/bpc-db/test/integration/AuthIntegrationSpec.hs` with tests
- [x] T144 Export auth functions from BPC.DB module

---

## Phase 13: Webhooks Repository

**Purpose**: Webhook endpoints, subscriptions, and delivery tracking

- [x] T145 [P] Create `packages/bpc-db/src/BPC/DB/Repos/Webhooks.hs` module
- [x] T146 Implement createEndpoint, getEndpoint, deactivateEndpoint in Webhooks.hs
- [x] T147 Implement subscribe, unsubscribe, getSubscriptions in Webhooks.hs
- [x] T148 Implement recordDelivery, markDelivered, markFailed in Webhooks.hs
- [x] T149 Implement getPendingDeliveries in Webhooks.hs
- [x] T150 [P] Create `packages/bpc-db/test/integration/WebhooksIntegrationSpec.hs` with tests
- [x] T151 Export webhook functions from BPC.DB module

---

## Phase 14: Hash Chain Verification CLI

**Purpose**: CLI tool for auditing event hash chains

- [x] T152 Implement `verifyAllChains :: Connection -> TenantId -> IO (Either ChainError VerifyReport)` in Events.hs
- [x] T153 Add VerifyReport data type (total_aggregates, total_events, broken_chains) in Events.hs
- [x] T154 Add CLI command `bpc-cli verify-chains --tenant-id=UUID` in bpc-cli package
- [x] T155 Add integration test: Verify 1000 events in < 5 seconds

---

## Phase 15: Polish & Cross-Cutting Concerns

**Purpose**: Integration, documentation, and final verification

- [x] T156 [P] Add Haddock comments to all public functions in BPC.DB modules
- [x] T157 Create comprehensive re-export from `packages/bpc-db/src/BPC/DB.hs`
- [x] T158 Verify no imports of BPC.API.* or BPC.Worker.* (layered architecture)
- [x] T159 Run full test suite: `cabal test bpc-db` with all tests passing
- [x] T160 Verify coverage >= 80%: generate HPC report for bpc-db
- [x] T161 Run fourmolu check: `fourmolu -m check packages/bpc-db/src/**/*.hs`
- [x] T162 Run hlint check: `hlint -h hlint.yaml packages/bpc-db/src/**/*.hs`
- [x] T163 Update quickstart.md with repository usage examples
- [x] T164 Run integration tests with docker-compose.test.yml

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: Depends on 01-foundation + 02-core-primitives
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **US1 (Pool)**: Foundational
- **US2 (Events)**: Depends on US1 (Pool)
- **US3 (Tenant Isolation)**: Pattern applied to all repos
- **US4-US9**: Depend on US1 (Pool) + US2 (Events)
- **Auth, Webhooks**: After core repos
- **CLI**: After Events verification
- **Polish**: After all repos

### Parallel Opportunities

**Phase 6-11**: Document, Fact, Snapshot, Passport, Job, Rule repos can largely proceed in parallel once Pool + Events are complete

---

## Implementation Strategy

### MVP First (Pool + Events + Core Repos)

1. Complete Phase 1-2: Setup + Foundational (T001-T013)
2. Complete US1: Pool (T014-T026)
3. Complete US2: Events (T027-T042)
4. Complete US4-US6: Documents, Facts, Snapshots (T049-T085)
5. **STOP and VALIDATE**: Core data pipeline works

### Incremental Delivery

1. Setup + Foundational → Error types ready
2. US1 (Pool) → DB connectivity
3. US2 (Events) → Audit trail
4. US4-US6 → Document pipeline
5. US7 (Passports) → Passport storage
6. US8 (Jobs) → Job queue
7. US9 (Rules) → Rule management
8. Auth + Webhooks + CLI → Supporting features
9. Polish → Documentation, coverage

---

## Summary

- **Total Tasks**: 164
- **Phase 1 (Setup)**: 6 tasks
- **Phase 2 (Foundational)**: 7 tasks
- **Phase 3 (US1 - Pool)**: 13 tasks
- **Phase 4 (US2 - Events)**: 16 tasks
- **Phase 5 (US3 - Isolation)**: 6 tasks
- **Phase 6 (US4 - Documents)**: 10 tasks
- **Phase 7 (US5 - Facts)**: 12 tasks
- **Phase 8 (US6 - Snapshots)**: 15 tasks
- **Phase 9 (US7 - Passports)**: 13 tasks
- **Phase 10 (US8 - Jobs)**: 21 tasks
- **Phase 11 (US9 - Rules)**: 15 tasks
- **Phase 12 (Auth)**: 10 tasks
- **Phase 13 (Webhooks)**: 7 tasks
- **Phase 14 (CLI)**: 4 tasks
- **Phase 15 (Polish)**: 9 tasks

**MVP Scope**: Phases 1-8 (85 tasks) for core data layer
**Parallel Opportunities**: 15+ tasks marked [P]
**Constitution Compliance**: Tenant isolation, hash chain for audit, immutability enforced
