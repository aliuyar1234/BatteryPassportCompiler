# Tasks: Job Processing (Worker)

**Input**: Design documents from `specs/07-job-processing/`
**Prerequisites**: plan.md (required), spec.md (required), data-model.md
**Phase**: P2-P3 - Background worker, depends on 05-data-layer + 04-compilation-pipeline
**Package**: bpc-worker (async job processing)

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US11)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `packages/bpc-worker/src/BPC/Worker/`
- **Handlers**: `packages/bpc-worker/src/BPC/Worker/Handlers/`
- **Tests**: `packages/bpc-worker/test/`

---

## Phase 1: Setup (Package Configuration)

**Purpose**: Configure bpc-worker package with required dependencies

- [x] T001 Create `packages/bpc-worker/bpc-worker.cabal` with async, http-client, qrcode dependencies
- [x] T002 Add bpc-core, bpc-db internal dependencies to bpc-worker.cabal
- [x] T003 Create directory structure: `packages/bpc-worker/src/BPC/Worker/Handlers/`
- [x] T004 Create directory structure: `packages/bpc-worker/test/unit/` and `test/integration/`
- [x] T005 [P] Add hspec, QuickCheck test dependencies to bpc-worker.cabal
- [x] T006 Create `packages/bpc-worker/src/BPC/Worker.hs` re-export module

**Checkpoint**: `cabal build bpc-worker` succeeds with dependencies

---

## Phase 2: Foundational (Worker Types & Retry Logic)

**Purpose**: Define worker types, errors, and retry logic - BLOCKS all user stories

- [x] T007 Create `packages/bpc-worker/src/BPC/Worker/Types.hs` with WorkerConfig data type
- [x] T008 Add WorkerEnv, HandlerError data types to Types.hs
- [x] T009 Add HandlerResult data type (Success | Failure | Retry) to Types.hs
- [x] T010 Create `packages/bpc-worker/src/BPC/Worker/Retry.hs` module
- [x] T011 Implement `computeBackoff :: Int -> NominalDiffTime` (2^attempts, capped at 1024) in Retry.hs
- [x] T012 Define maxAttempts = 5 constant (MUST per SSOT) in Retry.hs
- [x] T013 Implement `isRetryable :: SomeException -> Bool` in Retry.hs
- [x] T014 Export retry functions from BPC.Worker module

**Checkpoint**: Retry logic compiles; backoff formula tested

---

## Phase 3: User Story 1 - Worker Loop (Priority: P1) MVP

**Goal**: Continuous job polling and processing loop

**Independent Test**: Start worker, enqueue job, observe processing

### Tests for User Story 1

- [x] T015 [P] [US1] Create `packages/bpc-worker/test/unit/BPC/Worker/RunnerSpec.hs` with scaffold
- [x] T016 [US1] Add test: Worker polls at configured interval in RunnerSpec.hs
- [x] T017 [US1] Add test: Worker processes job when available in RunnerSpec.hs
- [x] T018 [US1] Add test: Worker sleeps when queue empty in RunnerSpec.hs
- [x] T019 [US1] Add test: Worker graceful shutdown on signal in RunnerSpec.hs

### Implementation for User Story 1

- [x] T020 [US1] Create `packages/bpc-worker/src/BPC/Worker/Main.hs` module
- [x] T021 [US1] Implement loadConfig from ENV vars in Main.hs
- [x] T022 [US1] Create `packages/bpc-worker/src/BPC/Worker/Runner.hs` module
- [x] T023 [US1] Implement `runWorker :: WorkerConfig -> Pool Connection -> IO ()` main loop in Runner.hs
- [x] T024 [US1] Implement poll interval logic (BPC_JOBS_POLL_INTERVAL_MS) in Runner.hs
- [x] T025 [US1] Add graceful shutdown handling in Runner.hs
- [x] T026 [US1] Export runWorker from BPC.Worker module

**Checkpoint**: Worker loop runs continuously; processes jobs when available

---

## Phase 4: User Story 2 - Job Leasing & Locking (Priority: P1)

**Goal**: Distributed job leasing with FOR UPDATE SKIP LOCKED

**Independent Test**: Multiple workers compete; no job processed twice

### Tests for User Story 2

- [x] T027 [P] [US2] Add leasing tests to RunnerSpec.hs
- [x] T028 [US2] Add test: acquireLease sets status RUNNING in RunnerSpec.hs
- [x] T029 [US2] Add test: Leased job not acquired by another worker in RunnerSpec.hs
- [x] T030 [US2] Add test: Expired lease allows re-acquisition in RunnerSpec.hs
- [x] T031 [P] [US2] Create `packages/bpc-worker/test/integration/BPC/Worker/ConcurrencySpec.hs`
- [x] T032 [US2] Add test: 10 workers, no duplicate processing in ConcurrencySpec.hs

### Implementation for User Story 2

- [x] T033 [US2] Implement `acquireLeaseFromDb :: Connection -> Text -> IO (Maybe Job)` in Runner.hs
- [x] T034 [US2] Use FOR UPDATE SKIP LOCKED SQL in acquireLeaseFromDb
- [x] T035 [US2] Implement `leaseRenewalLoop :: WorkerConfig -> Pool Connection -> Job -> IO ()` in Runner.hs
- [x] T036 [US2] Use async thread for lease renewal in runWorker
- [x] T037 [US2] Implement lease renewal at BPC_JOBS_LEASE_RENEW_SECONDS interval
- [x] T038 [US2] Cancel renewal thread on job completion

**Checkpoint**: FOR UPDATE SKIP LOCKED prevents double-processing

---

## Phase 5: User Story 3 - Error Handling & Retries (Priority: P1)

**Goal**: Exponential backoff retries with max 5 attempts

**Independent Test**: Failed job retries with increasing delays; DEAD_LETTER after 5

### Tests for User Story 3

- [x] T039 [P] [US3] Create `packages/bpc-worker/test/unit/BPC/Worker/RetrySpec.hs` with scaffold
- [x] T040 [US3] Add test: computeBackoff(1) = 2s in RetrySpec.hs
- [x] T041 [US3] Add test: computeBackoff(5) = 32s in RetrySpec.hs
- [x] T042 [US3] Add test: computeBackoff(10) = 1024s (capped) in RetrySpec.hs
- [x] T043 [US3] Add test: Retryable error → re-queue with backoff in RunnerSpec.hs
- [x] T044 [US3] Add test: Non-retryable error → FAILED in RunnerSpec.hs
- [x] T045 [US3] Add test: attempts >= 5 → DEAD_LETTER in RunnerSpec.hs

### Implementation for User Story 3

- [x] T046 [US3] Implement `handleJobResult :: Connection -> Job -> Either SomeException () -> IO ()` in Runner.hs
- [x] T047 [US3] On success: call completeJob in handleJobResult
- [x] T048 [US3] On retryable failure: calculate backoff, call retryJob in handleJobResult
- [x] T049 [US3] On max attempts: call updateJobStatus DEAD_LETTER in handleJobResult
- [x] T050 [US3] On non-retryable: call updateJobStatus FAILED in handleJobResult
- [x] T051 [US3] Store last_error JSON in all failure cases

**Checkpoint**: Exponential backoff works; DEAD_LETTER after 5 attempts

---

## Phase 6: User Story 4 - PARSE_FACTS Handler (Priority: P1)

**Goal**: Parse uploaded documents and extract facts with canonical bytes

**Independent Test**: Upload BOM document → Facts created with hashes

### Tests for User Story 4

- [x] T052 [P] [US4] Create `packages/bpc-worker/test/unit/BPC/Worker/Handlers/ParseFactsSpec.hs`
- [x] T053 [US4] Add test: Valid BOM → Facts with canonical bytes in ParseFactsSpec.hs
- [x] T054 [US4] Add test: Fact hash = SHA-256(canonical) in ParseFactsSpec.hs
- [x] T055 [US4] Add test: DocumentVersion → VALIDATED in ParseFactsSpec.hs
- [x] T056 [US4] Add test: Invalid format → REJECTED in ParseFactsSpec.hs
- [x] T057 [US4] Add test: Audit event emitted in ParseFactsSpec.hs

### Implementation for User Story 4

- [x] T058 [US4] Create `packages/bpc-worker/src/BPC/Worker/Handlers/ParseFacts.hs` module
- [x] T059 [US4] Define ParseFactsPayload data type in ParseFacts.hs
- [x] T060 [US4] Implement parseFacts :: Pool Connection -> Job -> IO () in ParseFacts.hs
- [x] T061 [US4] Implement parseBOM :: ByteString -> IO [ParsedFact] in ParseFacts.hs
- [x] T062 [US4] Implement parsePCF :: ByteString -> IO [ParsedFact] in ParseFacts.hs
- [x] T063 [US4] Use canonicalEncode and sha256 for fact storage
- [x] T064 [US4] Update DocumentVersion status to VALIDATED
- [x] T065 [US4] Emit FACTS_PARSED audit event

**Checkpoint**: Facts created with canonical bytes and hash

---

## Phase 7: User Story 5 - BUILD_SNAPSHOT Handler (Priority: P1)

**Goal**: Transition snapshots from BUILDING to READY

**Independent Test**: Snapshot BUILDING → READY

### Tests for User Story 5

- [x] T066 [P] [US5] Add BUILD_SNAPSHOT tests to HandlersSpec.hs
- [x] T067 [US5] Add test: BUILDING → READY in HandlersSpec.hs
- [x] T068 [US5] Add test: Empty snapshot → still READY in HandlersSpec.hs
- [x] T069 [US5] Add test: Already READY → error in HandlersSpec.hs

### Implementation for User Story 5

- [x] T070 [US5] Create `packages/bpc-worker/src/BPC/Worker/Handlers/BuildSnapshot.hs` module
- [x] T071 [US5] Define BuildSnapshotPayload data type in BuildSnapshot.hs
- [x] T072 [US5] Implement buildSnapshot :: Pool Connection -> Job -> IO () in BuildSnapshot.hs
- [x] T073 [US5] Verify status is BUILDING before transition
- [x] T074 [US5] Emit SNAPSHOT_READY audit event

**Checkpoint**: BUILD_SNAPSHOT transitions BUILDING → READY

---

## Phase 8: User Story 6 - COMPILE_PASSPORT Handler (Priority: P1)

**Goal**: Compile passport using pure compilePassportPure function

**Independent Test**: SEALED snapshot + PUBLISHED rules → PassportVersion

### Tests for User Story 6

- [x] T075 [P] [US6] Create `packages/bpc-worker/test/unit/BPC/Worker/Handlers/CompilePassportSpec.hs`
- [x] T076 [US6] Add test: Valid inputs → PassportVersion created in CompilePassportSpec.hs
- [x] T077 [US6] Add test: Snapshot not SEALED → error in CompilePassportSpec.hs
- [x] T078 [US6] Add test: Rules not PUBLISHED → error in CompilePassportSpec.hs
- [x] T079 [US6] Add test: Compilation is deterministic (golden) in CompilePassportSpec.hs
- [x] T080 [US6] Add test: Payload too large → error in CompilePassportSpec.hs
- [x] T081 [US6] Add test: SIGN_PASSPORT job enqueued in CompilePassportSpec.hs

### Implementation for User Story 6

- [x] T082 [US6] Create `packages/bpc-worker/src/BPC/Worker/Handlers/CompilePassport.hs` module
- [x] T083 [US6] Define CompilePassportPayload data type in CompilePassport.hs
- [x] T084 [US6] Implement compilePassport :: WorkerConfig -> Pool Connection -> Job -> IO ()
- [x] T085 [US6] Load and verify SEALED snapshot
- [x] T086 [US6] Load and verify PUBLISHED rules
- [x] T087 [US6] Call compilePassportPure from bpc-core
- [x] T088 [US6] Verify size limits (payload, proof, receipt)
- [x] T089 [US6] Insert PassportVersion with all canonical artifacts
- [x] T090 [US6] Enqueue SIGN_PASSPORT job
- [x] T091 [US6] Emit PASSPORT_COMPILED audit event

**Checkpoint**: compilePassportPure used; deterministic output verified

---

## Phase 9: User Story 7 - SIGN_PASSPORT Handler (Priority: P1)

**Goal**: Sign receipt hash with ED25519

**Independent Test**: PassportVersion COMPILING → SIGNED with signature

### Tests for User Story 7

- [x] T092 [P] [US7] Create `packages/bpc-worker/test/unit/BPC/Worker/Handlers/SignPassportSpec.hs`
- [x] T093 [US7] Add test: Valid passport → signature created in SignPassportSpec.hs
- [x] T094 [US7] Add test: Signature verifiable with public key in SignPassportSpec.hs
- [x] T095 [US7] Add test: Signing key missing → error in SignPassportSpec.hs
- [x] T096 [US7] Add test: GENERATE_QR job enqueued in SignPassportSpec.hs
- [x] T097 [US7] Add test: Status transitions COMPILING → SIGNED in SignPassportSpec.hs

### Implementation for User Story 7

- [x] T098 [US7] Create `packages/bpc-worker/src/BPC/Worker/Handlers/SignPassport.hs` module
- [x] T099 [US7] Define SignPassportPayload data type in SignPassport.hs
- [x] T100 [US7] Implement signPassport :: WorkerConfig -> Pool Connection -> Job -> IO ()
- [x] T101 [US7] Load ED25519 private key from BPC_SIGNING_ED25519_PRIVATE_KEY_BASE64
- [x] T102 [US7] Sign receipt hash with ED25519
- [x] T103 [US7] Store signature and public key in PassportVersion
- [x] T104 [US7] Update status to SIGNED
- [x] T105 [US7] Enqueue GENERATE_QR job
- [x] T106 [US7] Emit PASSPORT_SIGNED audit event

**Checkpoint**: ED25519 signature verifiable; status SIGNED

---

## Phase 10: User Story 8 - GENERATE_QR Handler (Priority: P2)

**Goal**: Generate QR PNG in BPC-QR-1 format

**Independent Test**: PassportVersion SIGNED → QR PNG stored

### Tests for User Story 8

- [x] T107 [P] [US8] Create `packages/bpc-worker/test/unit/BPC/Worker/Handlers/GenerateQRSpec.hs`
- [x] T108 [US8] Add test: Valid passport → QR PNG generated in GenerateQRSpec.hs
- [x] T109 [US8] Add test: QR payload matches BPC-QR-1 format in GenerateQRSpec.hs
- [x] T110 [US8] Add test: Base32 hashes no padding in GenerateQRSpec.hs
- [x] T111 [US8] Add test: Status remains SIGNED in GenerateQRSpec.hs

### Implementation for User Story 8

- [x] T112 [US8] Create `packages/bpc-worker/src/BPC/Worker/Handlers/GenerateQR.hs` module
- [x] T113 [US8] Define GenerateQRPayload data type in GenerateQR.hs
- [x] T114 [US8] Implement generateQR :: Pool Connection -> Job -> IO ()
- [x] T115 [US8] Build QR payload string using buildQrPayload from bpc-core
- [x] T116 [US8] Generate QR code PNG using qrcode library
- [x] T117 [US8] Store qr_payload and qr_png in PassportVersion
- [x] T118 [US8] Emit QR_GENERATED audit event

**Checkpoint**: QR PNG generated; payload in BPC-QR-1 format

---

## Phase 11: User Story 9 - EXPORT_PASSPORT Handler (Priority: P3)

**Goal**: Export passports in various formats (JSON, PDF)

**Independent Test**: PassportVersion ACTIVE → export file generated

### Tests for User Story 9

- [x] T119a [P] [US9] Create `packages/bpc-worker/test/unit/BPC/Worker/Handlers/ExportPassportSpec.hs`
- [x] T119b [US9] Add test: ACTIVE passport → JSON export in ExportPassportSpec.hs
- [x] T119c [US9] Add test: Non-ACTIVE → error in ExportPassportSpec.hs
- [x] T119d [US9] Add test: Export file path stored in job result in ExportPassportSpec.hs

### Implementation for User Story 9

- [x] T119e [US9] Create `packages/bpc-worker/src/BPC/Worker/Handlers/ExportPassport.hs` module
- [x] T119f [US9] Define ExportPassportPayload data type in ExportPassport.hs
- [x] T119g [US9] Implement exportPassport :: Pool Connection -> Job -> IO ()
- [x] T119h [US9] Verify PassportVersion is ACTIVE before export
- [x] T119i [US9] Generate JSON export file
- [x] T119j [US9] Emit PASSPORT_EXPORTED audit event

**Checkpoint**: Export generates JSON file; only ACTIVE passports exportable

---

## Phase 12: User Story 10 - RUN_RULE_TESTS Handler (Priority: P2)

**Goal**: Execute rule tests requiring >= 500 passing cases for VALIDATED

**Independent Test**: RulePackageVersion → test run results stored

### Tests for User Story 10

- [x] T119 [P] [US10] Create `packages/bpc-worker/test/unit/BPC/Worker/Handlers/RunRuleTestsSpec.hs`
- [x] T120 [US10] Add test: All tests pass + >=500 → VALIDATED in RunRuleTestsSpec.hs
- [x] T121 [US10] Add test: Tests pass but <500 → DRAFT in RunRuleTestsSpec.hs
- [x] T122 [US10] Add test: Tests fail → DRAFT with details in RunRuleTestsSpec.hs
- [x] T123 [US10] Add test: Test results stored in rule_tests_runs in RunRuleTestsSpec.hs

### Implementation for User Story 10

- [x] T124 [US10] Create `packages/bpc-worker/src/BPC/Worker/Handlers/RunRuleTests.hs` module
- [x] T125 [US10] Define RunRuleTestsPayload data type in RunRuleTests.hs
- [x] T126 [US10] Implement runRuleTests :: Pool Connection -> Job -> IO ()
- [x] T127 [US10] Parse test suite from rule version
- [x] T128 [US10] Execute tests with seed from payload
- [x] T129 [US10] Check >=500 passing cases for VALIDATED
- [x] T130 [US10] Store test run results
- [x] T131 [US10] Emit TESTS_RUN audit event

**Checkpoint**: >=500 passing cases required for VALIDATED

---

## Phase 13: User Story 11 - DELIVER_WEBHOOK Handler (Priority: P3)

**Goal**: Deliver webhook events with HMAC signature

**Independent Test**: Webhook delivery with X-BPC-Signature header

### Tests for User Story 11

- [x] T132 [P] [US11] Create `packages/bpc-worker/test/unit/BPC/Worker/Handlers/DeliverWebhookSpec.hs`
- [x] T133 [US11] Add test: Successful delivery → DELIVERED in DeliverWebhookSpec.hs
- [x] T134 [US11] Add test: HMAC signature verifiable in DeliverWebhookSpec.hs
- [x] T135 [US11] Add test: HTTP error → retry in DeliverWebhookSpec.hs
- [x] T136 [US11] Add test: Timeout → retry in DeliverWebhookSpec.hs
- [x] T137 [US11] Add test: Max attempts → DEAD_LETTER in DeliverWebhookSpec.hs

### Implementation for User Story 11

- [x] T138 [US11] Create `packages/bpc-worker/src/BPC/Worker/Handlers/DeliverWebhook.hs` module
- [x] T139 [US11] Define DeliverWebhookPayload data type in DeliverWebhook.hs
- [x] T140 [US11] Implement deliverWebhook :: Pool Connection -> Job -> IO ()
- [x] T141 [US11] Compute HMAC-SHA256 signature
- [x] T142 [US11] Add X-BPC-Signature header with sha256=<hex> format
- [x] T143 [US11] Execute HTTP POST with 30s timeout
- [x] T144 [US11] Handle success/failure responses
- [x] T145 [US11] Update delivery status

**Checkpoint**: HMAC signature in X-BPC-Signature; retry on failure

---

## Phase 14: Job Dispatcher

**Purpose**: Map job types to handlers

- [x] T146 Create `packages/bpc-worker/src/BPC/Worker/Dispatch.hs` module
- [x] T147 Implement `processJob :: WorkerConfig -> Pool Connection -> Job -> IO ()` in Dispatch.hs
- [x] T148 Map INGEST_DOCUMENT → ingestDocument handler
- [x] T149 Map PARSE_FACTS → parseFacts handler
- [x] T150 Map BUILD_SNAPSHOT → buildSnapshot handler
- [x] T151 Map COMPILE_PASSPORT → compilePassport handler
- [x] T152 Map SIGN_PASSPORT → signPassport handler
- [x] T153 Map GENERATE_QR → generateQR handler
- [x] T154 Map RUN_RULE_TESTS → runRuleTests handler
- [x] T155 Map EXPORT_PASSPORT → exportPassport handler
- [x] T156 Map DELIVER_WEBHOOK → deliverWebhook handler
- [x] T157 Throw UnsupportedJobType for unknown types

**Checkpoint**: All job types dispatched to correct handlers

---

## Phase 15: Integration Tests

**Purpose**: End-to-end pipeline and concurrency tests

- [x] T158 [P] Create `packages/bpc-worker/test/integration/BPC/Worker/E2ESpec.hs`
- [x] T159 Add E2E test: Full pipeline upload → active in E2ESpec.hs
- [x] T160 Add E2E test: Auto-chained jobs (compile → sign → qr) in E2ESpec.hs
- [x] T161 Add concurrency test: 10 workers, 100 jobs, no duplicates in ConcurrencySpec.hs
- [x] T162 Add retry test: Verify exponential backoff timing in E2ESpec.hs
- [x] T163 Add dead letter test: 10 failures → DEAD_LETTER in E2ESpec.hs [SSOT 4.6: BPC_JOBS_MAX_ATTEMPTS_DEFAULT=10]

---

## Phase 16: Polish & Cross-Cutting Concerns

**Purpose**: Integration, documentation, and final verification

- [x] T164 [P] Add Haddock comments to all public functions
- [x] T165 Create comprehensive re-export from `packages/bpc-worker/src/BPC/Worker.hs`
- [x] T166 Verify no imports of BPC.API.* (layered architecture)
- [x] T167 Run full test suite: `cabal test bpc-worker`
- [x] T168 Verify coverage >= 75%: generate HPC report
- [x] T169 Run fourmolu check on all bpc-worker source
- [x] T170 Run hlint check on all bpc-worker source
- [x] T171 Update quickstart.md with worker usage examples
- [x] T172 Performance test: 100+ jobs/minute per worker

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: Depends on 05-data-layer + 04-compilation-pipeline
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **US1 (Worker Loop)**: Foundational
- **US2 (Leasing)**: Depends on US1
- **US3 (Retries)**: Depends on US1
- **US4-US11 (Handlers)**: Depend on US1-US3
- **Dispatcher (Phase 14)**: After all handlers
- **Integration (Phase 15)**: After dispatcher

### Parallel Opportunities

**Phase 6-13**: All handlers can proceed in parallel once Runner infrastructure is complete

---

## Implementation Strategy

### MVP First (Worker + Core Handlers)

1. Complete Phase 1-2: Setup + Foundational
2. Complete US1-US3: Worker loop, leasing, retries
3. Complete US4: PARSE_FACTS handler
4. Complete US6-US7: COMPILE_PASSPORT, SIGN_PASSPORT handlers
5. **STOP and VALIDATE**: Core pipeline works

### Incremental Delivery

1. Setup + Foundational → Worker types ready
2. US1-US3 (Worker loop) → Jobs processed
3. US4 (PARSE_FACTS) → Facts extracted
4. US5 (BUILD_SNAPSHOT) → Snapshots ready
5. US6-US7 (COMPILE + SIGN) → Passports compiled
6. US8 (GENERATE_QR) → QR codes
7. US9 (RUN_RULE_TESTS) → Rule validation
8. US10 (DELIVER_WEBHOOK) → Integrations
9. Polish → Documentation, tests

---

## Summary

- **Total Tasks**: 182
- **Phase 1 (Setup)**: 6 tasks
- **Phase 2 (Foundational)**: 8 tasks
- **Phase 3 (US1 - Worker Loop)**: 12 tasks
- **Phase 4 (US2 - Leasing)**: 12 tasks
- **Phase 5 (US3 - Retries)**: 13 tasks
- **Phase 6 (US4 - PARSE_FACTS)**: 14 tasks
- **Phase 7 (US5 - BUILD_SNAPSHOT)**: 9 tasks
- **Phase 8 (US6 - COMPILE_PASSPORT)**: 17 tasks
- **Phase 9 (US7 - SIGN_PASSPORT)**: 15 tasks
- **Phase 10 (US8 - GENERATE_QR)**: 12 tasks
- **Phase 11 (US9 - EXPORT_PASSPORT)**: 10 tasks
- **Phase 12 (US10 - RUN_RULE_TESTS)**: 13 tasks
- **Phase 13 (US11 - DELIVER_WEBHOOK)**: 14 tasks
- **Phase 14 (Dispatcher)**: 12 tasks
- **Phase 15 (Integration)**: 6 tasks
- **Phase 16 (Polish)**: 9 tasks

**MVP Scope**: Phases 1-9 (106 tasks) for core compilation pipeline
**Parallel Opportunities**: 15+ tasks marked [P]
**Constitution Compliance**: Deterministic compilation, audit trail, layered architecture
