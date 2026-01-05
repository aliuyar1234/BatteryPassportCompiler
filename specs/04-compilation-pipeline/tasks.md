# Tasks: Compilation Pipeline

**Input**: Design documents from `specs/04-compilation-pipeline/`
**Prerequisites**: plan.md (required), spec.md (required), data-model.md
**Phase**: P1 - Core library, depends on 02-core-primitives + 03-rule-engine
**Package**: bpc-core (IO-free, pure Haskell)

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US7)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `packages/bpc-core/src/BPC/Core/`
- **Tests**: `packages/bpc-core/test/BPC/Core/`
- **Golden Fixtures**: `packages/bpc-core/test/golden/`

---

## Phase 1: Setup (Module Structure)

**Purpose**: Create module structure for compilation pipeline

- [X] T001 Create `packages/bpc-core/src/BPC/Core/Compile.hs` module skeleton
- [X] T002 [P] Create `packages/bpc-core/src/BPC/Core/Proof.hs` module skeleton
- [X] T003 [P] Create `packages/bpc-core/src/BPC/Core/Receipt.hs` module skeleton
- [X] T004 [P] Create `packages/bpc-core/src/BPC/Core/QR.hs` module skeleton
- [X] T005 Create `packages/bpc-core/test/golden/` directory for fixtures
- [X] T006 Add ed25519 dependency to `packages/bpc-core/bpc-core.cabal`
- [X] T007 Add tasty-golden dependency to test-suite in bpc-core.cabal

**Checkpoint**: `cabal build bpc-core` succeeds with new module skeletons

---

## Phase 2: Foundational (Shared Types & Errors)

**Purpose**: Define shared types and errors - BLOCKS all user stories

**CRITICAL**: Types must be defined before any implementation

- [X] T008 Define CompileInput data type in Compile.hs
- [X] T009 Define CompileOutput data type in Compile.hs
- [X] T010 Define CompileError sum type in Compile.hs
- [X] T011 Define size limit constants (maxPayloadSize, maxProofSize, maxReceiptSize) in Compile.hs
- [X] T012 [P] Define ProofNode data type in Proof.hs
- [X] T013 [P] Define NodeType enum (CONST, FACT_GET, FIELD_REF, OP, ASSERT, COMPLIANCE_EMIT) in Proof.hs
- [X] T014 [P] Define Proof data type in Proof.hs
- [X] T015 [P] Define ProofError sum type in Proof.hs
- [X] T016 [P] Define ReceiptInput data type in Receipt.hs
- [X] T017 [P] Define ReceiptUnsigned data type in Receipt.hs
- [X] T018 [P] Define QrInput data type in QR.hs
- [X] T019 Export all types from BPC.Core module

**Checkpoint**: All data types compile without errors

---

## Phase 3: User Story 1 - Pure Passport Compilation (Priority: P1) MVP

**Goal**: Implement `compilePassportPure` - deterministic compilation from Snapshot+Rules to Passport artifacts

**Independent Test**: `compilePassportPure` with identical inputs produces byte-identical outputs

### Tests for User Story 1

- [X] T020 [P] [US1] Create `packages/bpc-core/test/BPC/Core/CompileSpec.hs` with test scaffold
- [X] T021 [US1] Add test: Happy path with minimal input in CompileSpec.hs
- [X] T022 [US1] Add property test: `prop_compile_deterministic` (1000 samples) in CompileSpec.hs
- [X] T023 [US1] Add test: Snapshot not SEALED returns SnapshotNotSealed error in CompileSpec.hs
- [X] T024 [US1] Add test: Rules not PUBLISHED returns RulesNotPublished error in CompileSpec.hs
- [X] T025 [US1] Add test: Cycle in fields returns CycleDetected error in CompileSpec.hs
- [X] T026 [US1] Add test: Evaluation error propagates correctly in CompileSpec.hs

### Implementation for User Story 1

- [X] T027 [US1] Implement input validation (isSealed, isPublished checks) in Compile.hs
- [X] T028 [US1] Implement `evalFieldsWithProof` that evaluates fields and emits proof nodes in Compile.hs
- [X] T029 [US1] Implement `buildPayload` from evaluated values in Compile.hs
- [X] T030 [US1] Implement `compilePassportPure` main orchestration function in Compile.hs
- [X] T031 [US1] Add checkSize helper function for size limit enforcement in Compile.hs
- [X] T032 [US1] Export compilePassportPure from BPC.Core module

**Checkpoint**: `prop_compile_deterministic` passes with 1000 samples

---

## Phase 4: User Story 2 - Proof Tree Generation (Priority: P1)

**Goal**: Generate BPC-PROOF-1 derivation tree capturing all computation steps

**Independent Test**: `verifyProof` accepts all generated proofs

### Tests for User Story 2

- [X] T033 [P] [US2] Create `packages/bpc-core/test/BPC/Core/ProofSpec.hs` with test scaffold
- [X] T034 [US2] Add test: Build proof from simple node list in ProofSpec.hs
- [X] T035 [US2] Add test: Proof version is "BPC-PROOF-1" in ProofSpec.hs
- [X] T036 [US2] Add test: Node hash computed correctly (BPC-PROOF-HASH-1) in ProofSpec.hs
- [X] T037 [US2] Add property test: `prop_proof_verify` (all generated proofs verify) in ProofSpec.hs
- [X] T038 [US2] Add property test: `prop_proof_hash_deterministic` in ProofSpec.hs
- [X] T039 [US2] Add test: Manipulated proof (wrong hash) fails verification in ProofSpec.hs
- [X] T040 [US2] Add test: Field index maps field paths to node IDs in ProofSpec.hs

### Implementation for User Story 2

- [X] T041 [US2] Implement `computeNodeHash` per BPC-PROOF-HASH-1 formula in Proof.hs
- [X] T042 [US2] Implement `computeRootHash` from all nodes in Proof.hs
- [X] T043 [US2] Implement `buildFieldIndex` mapping field paths to node IDs in Proof.hs
- [X] T044 [US2] Implement `buildProof` from list of ProofNodes in Proof.hs
- [X] T045 [US2] Implement `verifyProof` that checks all node hashes in Proof.hs
- [X] T046 [US2] Export buildProof, verifyProof, computeNodeHash from BPC.Core module

**Checkpoint**: `prop_proof_verify` passes; all node hashes match BPC-PROOF-HASH-1

---

## Phase 5: User Story 3 - Receipt Generation (Priority: P1)

**Goal**: Generate BPC-RECEIPT-1 machine-verifiable receipt with all hashes

**Independent Test**: Receipt hash is deterministic for same inputs

### Tests for User Story 3

- [X] T047 [P] [US3] Create `packages/bpc-core/test/BPC/Core/ReceiptSpec.hs` with test scaffold
- [X] T048 [US3] Add test: Build receipt from ReceiptInput in ReceiptSpec.hs
- [X] T049 [US3] Add test: Receipt version is "BPC-RECEIPT-1" in ReceiptSpec.hs
- [X] T050 [US3] Add test: Receipt contains all required fields per BPC-RECEIPT-1 schema in ReceiptSpec.hs
- [X] T051 [US3] Add property test: `prop_receipt_hash_deterministic` in ReceiptSpec.hs
- [X] T052 [US3] Add test: Hashes in receipt match computed values in ReceiptSpec.hs
- [X] T053 [US3] Add test: Unsigned receipt has no signature field in ReceiptSpec.hs

### Implementation for User Story 3

- [X] T054 [US3] Implement `buildReceiptUnsigned` from ReceiptInput in Receipt.hs
- [X] T055 [US3] Implement `hashReceiptUnsigned` using canonicalEncode + sha256Hex in Receipt.hs
- [X] T056 [US3] Implement ToJSON instance for ReceiptUnsigned in Receipt.hs
- [X] T057 [US3] Export buildReceiptUnsigned, hashReceiptUnsigned from BPC.Core module

**Checkpoint**: Receipt structure matches BPC-RECEIPT-1 exactly; hash is deterministic

---

## Phase 6: User Story 4 - Signature Generation & Verification (Priority: P1)

**Goal**: Sign receipt hash with ED25519 and verify signatures

**Independent Test**: Sign → verify roundtrip always succeeds

### Tests for User Story 4

- [X] T058 [P] [US4] Add signature tests to ReceiptSpec.hs
- [X] T059 [US4] Add test: Sign and verify roundtrip succeeds in ReceiptSpec.hs
- [X] T060 [US4] Add property test: `prop_signature_roundtrip` in ReceiptSpec.hs
- [X] T061 [US4] Add test: Verify with wrong key fails in ReceiptSpec.hs
- [X] T062 [US4] Add test: Verify with wrong hash fails in ReceiptSpec.hs
- [X] T063 [US4] Add test: Derive public key from private key in ReceiptSpec.hs

### Implementation for User Story 4

- [X] T064 [US4] Define Ed25519PrivateKey newtype in Receipt.hs
- [X] T065 [US4] Define Ed25519PublicKey newtype in Receipt.hs
- [X] T066 [US4] Define Signature newtype in Receipt.hs
- [X] T067 [US4] Implement `signReceiptHash` using ed25519 library in Receipt.hs
- [X] T068 [US4] Implement `verifySignature` using ed25519 library in Receipt.hs
- [X] T069 [US4] Implement `derivePublicKey` from private key in Receipt.hs
- [X] T070 [US4] Export signReceiptHash, verifySignature, derivePublicKey from BPC.Core module

**Checkpoint**: `prop_signature_roundtrip` passes; ED25519 algorithm verified

---

## Phase 7: User Story 5 - QR Payload Generation (Priority: P2)

**Goal**: Generate compact BPC-QR-1 payload string for physical battery labels

**Independent Test**: QR payload format matches `BPC1|pv=<uuid>|ph=<b32>|pr=<b32>|rh=<b32>`

### Tests for User Story 5

- [X] T071 [P] [US5] Create `packages/bpc-core/test/BPC/Core/QRSpec.hs` with test scaffold
- [X] T072 [US5] Add test: QR payload format matches BPC-QR-1 in QRSpec.hs
- [X] T073 [US5] Add test: Base32 encoding has no padding (no `=`) in QRSpec.hs
- [X] T074 [US5] Add test: Base32 contains only [A-Z2-7] characters in QRSpec.hs
- [X] T075 [US5] Add property test: `prop_qr_deterministic` in QRSpec.hs
- [X] T076 [US5] Add test: Base32 decode roundtrip matches original hashes in QRSpec.hs

### Implementation for User Story 5

- [X] T077 [US5] Implement `buildQrPayload` in BPC-QR-1 format in QR.hs
- [X] T078 [US5] Use base32NoPad from BPC.Core.Hash for encoding in QR.hs
- [X] T079 [US5] Export buildQrPayload from BPC.Core module

**Checkpoint**: QR payload ~208 characters, fits alphanumeric QR mode

---

## Phase 8: User Story 6 - Size Limit Enforcement (Priority: P1)

**Goal**: Enforce hard size limits on all artifacts

**Independent Test**: Compilation exceeding limits returns appropriate error

### Tests for User Story 6

- [X] T080 [P] [US6] Add size limit tests to CompileSpec.hs
- [X] T081 [US6] Add test: Payload > 131,072 bytes returns PAYLOAD_TOO_LARGE in CompileSpec.hs
- [X] T082 [US6] Add test: Proof > 262,144 bytes returns PROOF_TOO_LARGE in CompileSpec.hs
- [X] T083 [US6] Add test: Receipt > 16,384 bytes returns RECEIPT_TOO_LARGE in CompileSpec.hs
- [X] T084 [US6] Add test: All under limits → success in CompileSpec.hs

### Implementation for User Story 6

- [X] T085 [US6] Implement `checkSize` helper in Compile.hs (if not done in T031)
- [X] T086 [US6] Integrate size checks into compilePassportPure in Compile.hs
- [X] T087 [US6] Add error messages with actual size in CompileError instances in Compile.hs

**Checkpoint**: Size limits enforced; errors include actual size

---

## Phase 9: User Story 7 - Golden Tests (Priority: P2)

**Goal**: Byte-exact regression tests to guarantee determinism

**Independent Test**: All golden fixtures are byte-identical on re-run

### Tests for User Story 7

- [X] T088 [P] [US7] Create `packages/bpc-core/test/golden/minimal.json` fixture
- [X] T089 [P] [US7] Create `packages/bpc-core/test/golden/medium.json` fixture
- [X] T090 [P] [US7] Create `packages/bpc-core/test/golden/edge.json` fixture
- [X] T091 [US7] Create golden test runner in `packages/bpc-core/test/BPC/Core/GoldenSpec.hs`
- [X] T092 [US7] Add test: minimal.json bytes match exactly in GoldenSpec.hs
- [X] T093 [US7] Add test: medium.json bytes match exactly in GoldenSpec.hs
- [X] T094 [US7] Add test: edge.json bytes match exactly in GoldenSpec.hs

### Implementation for User Story 7

- [X] T095 [US7] Implement loadFixture helper to load JSON fixture in GoldenSpec.hs
- [X] T096 [US7] Implement assertEqual for payload_canonical, proof_canonical, receipt_canonical in GoldenSpec.hs
- [X] T097 [US7] Implement assertEqual for all hashes in GoldenSpec.hs

**Checkpoint**: All 3 golden fixtures pass byte-exact comparison

---

## Phase 10: Polish & Cross-Cutting Concerns

**Purpose**: Integration, documentation, and final verification

- [X] T098 [P] Add Haddock comments to all public functions in compilation modules
- [X] T099 Create comprehensive re-export from `packages/bpc-core/src/BPC/Core.hs`
- [X] T100 Verify no IO imports: `grep -r "import.*IO" packages/bpc-core/src/BPC/Core/`
- [X] T101 Run full test suite: `cabal test bpc-core` with all compilation tests passing
- [X] T102 Verify coverage >= 90%: generate HPC report for BPC.Core.Compile, Proof, Receipt, QR
- [X] T103 Run fourmolu check: `fourmolu -m check packages/bpc-core/src/BPC/Core/*.hs`
- [X] T104 Run hlint check: `hlint -h hlint.yaml packages/bpc-core/src/BPC/Core/*.hs`
- [X] T105 Update quickstart.md with compilation examples
- [X] T106 Performance test: Compile typical passport (20-50 fields) in < 100ms

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: Depends on 02-core-primitives + 03-rule-engine completion
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phase 3-9)**: All depend on Foundational
  - US1 (Compile) must complete first (orchestrates everything)
  - US2 (Proof) depends on US1 evaluation mechanism
  - US3 (Receipt) depends on US1 completion
  - US4 (Signature) depends on US3 (receipt to sign)
  - US5 (QR) can proceed in parallel after US1
  - US6 (Size Limits) depends on US1 (integrated into compile)
  - US7 (Golden Tests) depends on US1-US4
- **Polish (Phase 10)**: Depends on all user stories

### User Story Dependencies

```
Phase 2: Foundational (Types & Errors)
     │
     ▼
Phase 3: US1 (Pure Compilation) ────────────────────┐
     │                                              │
     ├──────────────────┬──────────────────┐        │
     ▼                  ▼                  ▼        │
Phase 4: US2      Phase 5: US3      Phase 7: US5   │
(Proof)           (Receipt)         (QR)           │
     │                  │                          │
     │                  ▼                          │
     │            Phase 6: US4                     │
     │            (Signature)                      │
     │                  │                          │
     ├──────────────────┤                          │
     │                  │                          │
     │                  ▼                          ▼
     │            Phase 8: US6 ◄───────────────────┤
     │            (Size Limits)                    │
     │                  │                          │
     └──────────────────┴──────────────────────────┘
                        │
                        ▼
                  Phase 9: US7 (Golden Tests)
                        │
                        ▼
                  Phase 10: Polish
```

### Parallel Opportunities

**Phase 1**: T002, T003, T004 (module skeletons) can run in parallel
**Phase 2**: T012-T018 (type definitions in different files) can run in parallel
**Phase 3**: T020 (test scaffold) can start early
**Phase 4-7**: Test scaffolds (T033, T047, T058, T071) can start in parallel
**Phase 9**: T088-T090 (fixture creation) can run in parallel

---

## Summary

- **Total Tasks**: 106
- **Phase 1 (Setup)**: 7 tasks
- **Phase 2 (Foundational)**: 12 tasks
- **Phase 3 (US1 - Compile)**: 13 tasks
- **Phase 4 (US2 - Proof)**: 14 tasks
- **Phase 5 (US3 - Receipt)**: 11 tasks
- **Phase 6 (US4 - Signature)**: 13 tasks
- **Phase 7 (US5 - QR)**: 9 tasks
- **Phase 8 (US6 - Size Limits)**: 8 tasks
- **Phase 9 (US7 - Golden Tests)**: 10 tasks
- **Phase 10 (Polish)**: 9 tasks

**MVP Scope**: Phases 1-6 (70 tasks) for complete compilation + signing
**Parallel Opportunities**: 15 tasks marked [P]
**Constitution Compliance**: All code IO-free, determinism verified via property tests

**STATUS**: All 106 tasks completed
