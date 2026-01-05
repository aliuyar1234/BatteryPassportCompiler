# Tasks: Core Primitives

**Input**: Design documents from `specs/02-core-primitives/`
**Prerequisites**: plan.md (required), spec.md (required), data-model.md
**Phase**: P1 - Core library, depends on 01-foundation
**Package**: bpc-core (IO-free, pure Haskell)

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US5)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `packages/bpc-core/src/BPC/Core/`
- **Tests**: `packages/bpc-core/test/BPC/Core/`
- **Cabal**: `packages/bpc-core/bpc-core.cabal`

---

## Phase 1: Setup (Package Configuration)

**Purpose**: Configure bpc-core package with required dependencies

- [X] T001 Update `packages/bpc-core/bpc-core.cabal` with dependencies: aeson, cryptonite, memory, scientific, text, bytestring
- [X] T002 Add test-suite to `packages/bpc-core/bpc-core.cabal` with QuickCheck, HUnit, hspec dependencies
- [X] T003 Create `packages/bpc-core/src/BPC/Core.hs` re-export module (empty placeholder)
- [X] T004 Create `packages/bpc-core/test/Main.hs` test driver

**Checkpoint**: `cabal build bpc-core` and `cabal test bpc-core` run (tests may be empty)

---

## Phase 2: Foundational (Error Types)

**Purpose**: Define error types used by all primitives - BLOCKS all user stories

**CRITICAL**: Error types must be defined before any implementation

- [X] T005 Create `packages/bpc-core/src/BPC/Core/Error.hs` with `CanonicalError` type
- [X] T006 Add `UnitError` to `packages/bpc-core/src/BPC/Core/Error.hs` (UNIT_MISMATCH, etc.)
- [X] T007 Export error types from `packages/bpc-core/src/BPC/Core.hs`

**Checkpoint**: Error types available for use in implementation

---

## Phase 3: User Story 1 - Canonical JSON Encoding (Priority: P1) MVP

**Goal**: JSON values deterministically encoded to canonical bytes (BPC-CJSON-1)

**Independent Test**: Property test `prop_canonicalEncode_deterministic` passes with 10,000 samples

### Tests for User Story 1

- [X] T008 [P] [US1] Create `packages/bpc-core/test/BPC/Core/CanonicalJsonSpec.hs` with test scaffold
- [X] T009 [US1] Add property test `prop_canonicalEncode_deterministic` in CanonicalJsonSpec.hs
- [X] T010 [US1] Add golden test: `{"b":2,"a":1}` → `{"a":1,"b":2}` in CanonicalJsonSpec.hs
- [X] T011 [US1] Add test: Float input → `CanonicalNumberNotAllowed` in CanonicalJsonSpec.hs
- [X] T012 [US1] Add test: Exponent input → `CanonicalNumberNotAllowed` in CanonicalJsonSpec.hs
- [X] T013 [US1] Add edge case tests: empty object, Unicode strings, deep nesting in CanonicalJsonSpec.hs

### Implementation for User Story 1

- [X] T014 [US1] Create `packages/bpc-core/src/BPC/Core/CanonicalJson.hs` module skeleton
- [X] T015 [US1] Implement `canonicalEncode :: Value -> Either CanonicalError ByteString` in CanonicalJson.hs
- [X] T016 [US1] Implement key sorting by UTF-8 byte order in canonicalEncode
- [X] T017 [US1] Implement float/exponent rejection in canonicalEncode
- [X] T018 [US1] Implement whitespace removal in canonicalEncode
- [X] T019 [US1] Implement `canonicalDecode :: ByteString -> Either CanonicalError Value` in CanonicalJson.hs
- [X] T020 [US1] Export canonicalEncode, canonicalDecode from BPC.Core module

**Checkpoint**: `cabal test bpc-core` passes all canonical JSON tests, coverage >= 95%

---

## Phase 4: User Story 2 - SHA-256 Hashing (Priority: P1)

**Goal**: Secure byte hashing for integrity verification

**Independent Test**: Hash fixtures from SSOT 7.2 match exactly

### Tests for User Story 2

- [X] T021 [P] [US2] Create `packages/bpc-core/test/BPC/Core/HashSpec.hs` with test scaffold
- [X] T022 [US2] Add fixture test: `{"a":1,"b":2}` → `43258cff...` in HashSpec.hs
- [X] T023 [US2] Add property test: same input → same hash in HashSpec.hs
- [X] T024 [US2] Add test: different inputs → different hashes in HashSpec.hs

### Implementation for User Story 2

- [X] T025 [US2] Create `packages/bpc-core/src/BPC/Core/Hash.hs` module skeleton
- [X] T026 [US2] Implement `sha256Hex :: ByteString -> Text` using cryptonite in Hash.hs
- [X] T027 [US2] Implement `sha256 :: ByteString -> ByteString` (raw bytes) in Hash.hs
- [X] T028 [US2] Export sha256, sha256Hex from BPC.Core module

**Checkpoint**: Hash fixture `43258cff783fe7036d8a43033f830adfc60ec037382473548ac742b888292777` matches exactly

---

## Phase 5: User Story 3 - Base32 Encoding (Priority: P2)

**Goal**: Base32 encoding without padding for QR payloads (BPC-QR-1)

**Independent Test**: Output contains only [A-Z2-7], no `=` padding

### Tests for User Story 3

- [X] T029 [P] [US3] Create test cases for Base32 in HashSpec.hs (same file as hashing)
- [X] T030 [US3] Add test: output contains only [A-Z2-7] characters in HashSpec.hs
- [X] T031 [US3] Add test: output has no `=` padding in HashSpec.hs
- [X] T032 [US3] Add property test: same input → same Base32 output in HashSpec.hs

### Implementation for User Story 3

- [X] T033 [US3] Implement `base32NoPad :: ByteString -> Text` using memory (RFC4648) in Hash.hs
- [X] T034 [US3] Export base32NoPad from BPC.Core module

**Checkpoint**: Base32 encoding produces valid [A-Z2-7] output without padding

---

## Phase 6: User Story 4 - Domain Type Safety (Priority: P1)

**Goal**: Newtype wrappers for all domain IDs to prevent mix-ups at compile time

**Independent Test**: Compiler rejects `TenantId` assigned to `PassportId` variable

### Tests for User Story 4

- [X] T035 [P] [US4] Create `packages/bpc-core/test/BPC/Core/DomainSpec.hs` with test scaffold
- [X] T036 [US4] Add JSON round-trip tests for all ID types in DomainSpec.hs
- [X] T037 [US4] Add Eq/Ord tests for ID types in DomainSpec.hs

### Implementation for User Story 4

- [X] T038 [US4] Create `packages/bpc-core/src/BPC/Core/Types/Domain.hs` module
- [X] T039 [P] [US4] Define TenantId, ActorId, ApiKeyId, RoleId newtypes in Domain.hs
- [X] T040 [P] [US4] Define DocumentId, DocumentVersionId, FactId newtypes in Domain.hs
- [X] T041 [P] [US4] Define SnapshotId, SnapshotItemId newtypes in Domain.hs
- [X] T042 [P] [US4] Define RulePackageId, RulePackageVersionId newtypes in Domain.hs
- [X] T043 [P] [US4] Define PassportId, PassportVersionId newtypes in Domain.hs
- [X] T044 [P] [US4] Define EventId, JobId newtypes in Domain.hs
- [X] T045 [P] [US4] Define PolicyId, PolicyVersionId newtypes in Domain.hs
- [X] T046 [P] [US4] Define WebhookEndpointId, WebhookSubscriptionId, WebhookDeliveryId newtypes in Domain.hs
- [X] T047 [US4] Derive Eq, Ord, Show, ToJSON, FromJSON for all ID types in Domain.hs
- [X] T048 [US4] Export all ID types from BPC.Core module

**Checkpoint**: All 15+ ID types defined, compiler prevents type confusion

---

## Phase 7: User Story 5 - Unit Arithmetic (Priority: P2)

**Goal**: Physical quantities with compile-time unit checking (Qty) and fixed-precision decimals (Dec)

**Independent Test**: `Qty 1000 "g"` converts to `Qty 1 "kg"`; `Qty "kg" + Qty "kWh"` fails

### Tests for User Story 5 - Decimal

- [X] T049 [P] [US5] Create `packages/bpc-core/test/BPC/Core/DecimalSpec.hs` with test scaffold
- [X] T050 [US5] Add arithmetic tests: Dec(6) + Dec(6) = Dec(6) in DecimalSpec.hs
- [X] T051 [US5] Add property test: addition/subtraction preserves scale in DecimalSpec.hs

### Implementation for User Story 5 - Decimal

- [X] T052 [US5] Create `packages/bpc-core/src/BPC/Core/Types/Decimal.hs` module
- [X] T053 [US5] Define `Dec (scale :: Nat) = Dec Integer` using DataKinds in Decimal.hs
- [X] T054 [US5] Implement (+), (-), (*), (/) for Dec in Decimal.hs
- [X] T055 [US5] Implement `toDec :: KnownNat scale => Integer -> Dec scale` in Decimal.hs
- [X] T056 [US5] Export Dec, toDec from BPC.Core module

### Tests for User Story 5 - Quantity

- [X] T057 [P] [US5] Create `packages/bpc-core/test/BPC/Core/QuantitySpec.hs` with test scaffold
- [X] T058 [US5] Add conversion test: `Qty 1000 "g"` → `Qty 1 "kg"` in QuantitySpec.hs
- [X] T059 [US5] Add test: same unit addition works in QuantitySpec.hs
- [X] T060 [US5] Add test: different unit addition → UNIT_MISMATCH in QuantitySpec.hs
- [X] T061 [US5] Add test: Qty × Dec → Qty with same unit in QuantitySpec.hs
- [X] T062 [US5] Add property test: conversion round-trip (kg → g → kg) in QuantitySpec.hs

### Implementation for User Story 5 - Quantity

- [X] T063 [US5] Create `packages/bpc-core/src/BPC/Core/Types/Units.hs` with UnitDef
- [X] T064 [US5] Define conversion table: kg↔g, kWh↔Wh, kgCO2e↔gCO2e in Units.hs
- [X] T065 [US5] Add all 9 supported units (kg, g, kWh, Wh, gCO2e, kgCO2e, gCO2e_per_kWh, pct, each) in Units.hs
- [X] T066 [US5] Create `packages/bpc-core/src/BPC/Core/Types/Quantity.hs` module
- [X] T067 [US5] Define `Qty (unit :: Symbol) = Qty (Dec 6)` using DataKinds in Quantity.hs
- [X] T068 [US5] Implement `convert :: Qty u1 -> Either UnitError (Qty u2)` in Quantity.hs
- [X] T069 [US5] Implement (+), (-) for Qty (same unit only) in Quantity.hs
- [X] T070 [US5] Implement (*), (/) for Qty × Dec in Quantity.hs
- [X] T071 [US5] Export Qty, convert, unit operations from BPC.Core module

**Checkpoint**: All 9 units supported, conversion works, unit mismatch detected

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Integration, documentation, and final verification

- [X] T072 [P] Add Haddock comments to all public functions in BPC.Core modules
- [X] T073 Verify no IO imports: `grep -r "import.*IO" packages/bpc-core/src/`
- [X] T074 Run full test suite: `cabal test bpc-core` with all tests passing
- [X] T075 Verify coverage >= 90%: generate HPC report
- [X] T076 Run fourmolu check: `fourmolu -m check packages/bpc-core/src/**/*.hs`
- [X] T077 Run hlint check: `hlint -h hlint.yaml packages/bpc-core/src/**/*.hs`
- [X] T078 Update quickstart.md with working code examples
- [X] T079 Final integration test: encode → hash → verify round-trip

---

## Summary

- **Total Tasks**: 79
- **Phase 1 (Setup)**: 4 tasks
- **Phase 2 (Foundational)**: 3 tasks
- **Phase 3 (US1 - Canonical JSON)**: 13 tasks
- **Phase 4 (US2 - SHA-256)**: 8 tasks
- **Phase 5 (US3 - Base32)**: 6 tasks
- **Phase 6 (US4 - Domain Types)**: 14 tasks
- **Phase 7 (US5 - Unit Arithmetic)**: 23 tasks
- **Phase 8 (Polish)**: 8 tasks

**MVP Scope**: Phases 1-4 (28 tasks) for canonical JSON + hashing
**Parallel Opportunities**: 18 tasks marked [P]
**Constitution Compliance**: All code IO-free, determinism property tested

**STATUS**: All 79 tasks completed
