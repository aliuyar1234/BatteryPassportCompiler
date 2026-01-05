# Implementation Plan: Core Primitives

**Branch**: `02-core-primitives` | **Date**: 2025-12-28 | **Spec**: [spec.md](D:\Projekte\BatteryPassportCompiler\.specify\features\02-core-primitives\spec.md)

## Summary

This feature implements the foundational primitives for BatteryPassportCompiler: canonical JSON encoding (BPC-CJSON-1), cryptographic hashing (SHA-256), Base32 encoding for QR codes, domain-specific type-safe ID wrappers, and physical quantity arithmetic with unit checking. These primitives are critical for achieving byte-identical determinism, immutable canonical storage, and type-safe rule evaluation as required by the Constitution.

**Technical Approach**: Pure functional implementation in bpc-core package using Haskell, aeson for JSON, cryptonite for hashing, and phantom types (DataKinds) for compile-time unit/scale enforcement. All functions are IO-free to maintain the bpc-core purity constraint.

## Technical Context

**Language/Version**: Haskell (GHC 9.6.4)
**Primary Dependencies**: aeson (JSON), cryptonite (hashing), memory (Base32), scientific (number handling), text, bytestring
**Storage**: N/A (pure library, no persistence)
**Testing**: HUnit for unit tests, QuickCheck for property tests, golden tests for canonical output fixtures
**Target Platform**: Cross-platform library (Linux, macOS, Windows via Cabal)
**Project Type**: Single library package (bpc-core)
**Performance Goals**:
- Canonical encoding: 10,000+ values/sec for typical passport sizes
- Hash computation: leverages cryptonite (native C bindings)
- Zero allocations for newtype wrappers (GHC optimizes away at runtime)

**Constraints**:
- MUST be IO-free (Constitution V. Layered Architecture)
- MUST produce byte-identical output for identical inputs (Constitution I. Determinism)
- MUST reject floats/exponents in canonical JSON (SSOT 7.1)
- Size limits enforced downstream (not in primitives): payload ≤131KB, proof ≤262KB, receipt ≤16KB

**Scale/Scope**:
- 9 supported units (kg, g, kWh, Wh, gCO2e, kgCO2e, gCO2e_per_kWh, pct, each)
- 15+ domain ID types (TenantId, PassportId, SnapshotId, etc.)
- Decimal scales 0-18 supported via type-level Nat

## Constitution Check

**Gate: PASSED**

### I. Determinism
- **PASS**: All functions are pure; property test `prop_canonicalEncode_deterministic` ensures identical inputs yield identical bytes
- **PASS**: No timestamps, randomness, or environment variables in primitives
- **PASS**: Golden tests verify byte-exact canonical output

### II. Canonical Storage
- **PASS**: `canonicalEncode` enforces BPC-CJSON-1: keys sorted by UTF-8 bytes, no whitespace
- **PASS**: Float/exponent numbers rejected with `CanonicalNumberNotAllowed` error
- **PASS**: `CanonicalBytes` newtype prevents accidental mixing with non-canonical data

### III. Immutability
- **N/A**: This package provides primitives; immutability enforced in bpc-db layer

### IV. Audit Trail
- **N/A**: This package provides primitives; audit events handled in bpc-db layer

### V. Layered Architecture
- **PASS**: bpc-core has zero IO imports
- **PASS**: No dependencies on bpc-db, bpc-api, bpc-worker
- **PASS**: Only pure libraries (aeson, cryptonite, text, bytestring)

### VI. Type-Safe Rules
- **PASS**: Qty uses phantom type for unit enforcement: `Qty (unit :: Symbol)`
- **PASS**: Dec uses phantom type for scale enforcement: `Dec (scale :: Nat)`
- **PASS**: Unit arithmetic type rules: `+/-` requires same unit; `*` is Qty×Dec; `/` is Qty÷Dec
- **PASS**: Compiler prevents TenantId/PassportId confusion via newtype wrappers

## Project Structure

### Documentation (this feature)

```text
specs/02-core-primitives/
├── plan.md              # This file
├── research.md          # Algorithm choices and design rationale
├── data-model.md        # Haskell type definitions
├── quickstart.md        # Usage examples
└── contracts/           # N/A (pure library, no external contracts)
```

### Source Code (repository root)

```text
src/BPC/Core/
├── CanonicalJson.hs     # BPC-CJSON-1 encode/decode
├── Hash.hs              # SHA-256, Base32 utilities
├── Types/
│   ├── Domain.hs        # TenantId, PassportId, etc. (newtype wrappers)
│   ├── Decimal.hs       # Dec(scale) with phantom Nat
│   ├── Quantity.hs      # Qty(unit) with phantom Symbol
│   └── Units.hs         # UnitDef, conversion tables
└── Error.hs             # CanonicalError, UnitError

test/BPC/Core/
├── CanonicalJsonSpec.hs # Property tests, golden tests
├── HashSpec.hs          # Hash fixtures from SSOT 7.2
├── DecimalSpec.hs       # Scale arithmetic tests
├── QuantitySpec.hs      # Unit conversion, arithmetic tests
└── DomainSpec.hs        # Newtype wrapper tests

test/golden-tests/
├── canonical-json/      # Expected canonical output fixtures
│   ├── simple-object.json
│   ├── nested-object.json
│   └── unicode-strings.json
└── hash-fixtures/       # Expected hash outputs from SSOT 7.2
    └── sha256-fixtures.json
```

**Structure Decision**: Single library package under `src/BPC/Core/` following the existing SSOT architecture (Section 3.4). No need for multi-project structure as this is a foundational library with no frontend/backend split.

## Implementation Phases

### Phase 0: Research & Design (COMPLETED in research.md)
- Document BPC-CJSON-1 specification
- Justify SHA-256 choice
- Justify Base32 RFC4648 no-padding choice
- Design unit system with conversion table
- List all domain ID newtypes from SSOT 6.2

### Phase 1: Canonical JSON (Priority: P1)
**Goal**: Implement BPC-CJSON-1 with deterministic key ordering and float rejection

**Tasks**:
1. Create `src/BPC/Core/CanonicalJson.hs`
2. Define `CanonicalError` type: `CanonicalNumberNotAllowed | CanonicalDecodeFailed Text`
3. Implement `canonicalEncode :: Value -> Either CanonicalError ByteString`
   - Sort object keys by UTF-8 byte order
   - Reject Scientific numbers with exponent or fractional part
   - Remove all whitespace
4. Implement `canonicalDecode :: ByteString -> Either CanonicalError Value`
5. Write property test: `prop_canonicalEncode_deterministic`
6. Write golden tests with fixtures from SSOT 7.1
7. Write edge case tests: empty object, Unicode strings, deep nesting

**Acceptance**:
- Property test passes with 10,000 samples
- Golden test: `{"b":2,"a":1}` → `{"a":1,"b":2}`
- Float input → `CanonicalNumberNotAllowed`
- Exponent input → `CanonicalNumberNotAllowed`
- Coverage ≥ 95%

### Phase 2: Cryptographic Hashing (Priority: P1)
**Goal**: Implement SHA-256 and Base32 encoding

**Tasks**:
1. Create `src/BPC/Core/Hash.hs`
2. Implement `sha256Hex :: ByteString -> Text` using cryptonite
3. Implement `base32NoPad :: ByteString -> Text` using memory (RFC4648)
4. Write unit tests with SSOT 7.2 fixtures
5. Write property test: same input → same hash

**Acceptance**:
- Hash fixture from SSOT 7.2 matches exactly: `{"a":1,"b":2}` → `43258cff783fe7036d8a43033f830adfc60ec037382473548ac742b888292777`
- Base32 output contains only [A-Z2-7]
- Base32 output has no `=` padding
- Coverage ≥ 95%

### Phase 3: Domain ID Types (Priority: P1)
**Goal**: Implement type-safe newtype wrappers for all domain IDs

**Tasks**:
1. Create `src/BPC/Core/Types/Domain.hs`
2. Define newtypes from SSOT 6.2:
   - `TenantId`, `ActorId`, `ApiKeyId`, `RoleId`
   - `DocumentId`, `DocumentVersionId`, `FactId`
   - `SnapshotId`, `SnapshotItemId`
   - `RulePackageId`, `RulePackageVersionId`
   - `PassportId`, `PassportVersionId`
   - `EventId`, `JobId`, `PolicyId`, `PolicyVersionId`
   - `WebhookEndpointId`, `WebhookSubscriptionId`, `WebhookDeliveryId`
3. Derive: `Eq`, `Ord`, `Show`, `ToJSON`, `FromJSON`
4. Write compile-fail test: cannot assign `TenantId` to `PassportId` variable

**Acceptance**:
- All 15+ ID types defined
- Compiler prevents ID type confusion
- JSON serialization round-trips correctly

### Phase 4: Decimal Arithmetic (Priority: P1)
**Goal**: Implement fixed-precision decimals with type-level scale enforcement

**Tasks**:
1. Create `src/BPC/Core/Types/Decimal.hs`
2. Define `Dec (scale :: Nat) = Dec Integer` using DataKinds
3. Implement arithmetic: `(+)`, `(-)`, `(*)`, `(/)`
4. Implement `toDec :: KnownNat scale => Integer -> Dec scale`
5. Write property tests: addition/subtraction preserves scale
6. Write unit tests: Dec(2) + Dec(2) = Dec(2); Dec(2) + Dec(6) fails typecheck

**Acceptance**:
- Arithmetic operations preserve scale
- Compiler enforces scale compatibility
- Division by zero detected (Pure exception handled via Either)

### Phase 5: Quantity with Units (Priority: P2)
**Goal**: Implement physical quantities with unit checking

**Tasks**:
1. Create `src/BPC/Core/Types/Units.hs`
2. Define `UnitDef` with conversion factors (SSOT 8.1):
   - kg ↔ g (1000×)
   - kWh ↔ Wh (1000×)
   - kgCO2e ↔ gCO2e (1000×)
3. Create `src/BPC/Core/Types/Quantity.hs`
4. Define `Qty (unit :: Symbol) = Qty (Dec 6)` using DataKinds
5. Implement `convert :: KnownSymbol u1, KnownSymbol u2 => Qty u1 -> Either UnitError (Qty u2)`
6. Implement arithmetic:
   - `(+)`, `(-)`: same unit only
   - `(*)`: Qty × Dec → Qty
   - `(/)`: Qty ÷ Dec → Qty
7. Write unit tests for all 9 units
8. Write property test: conversion round-trip (kg → g → kg)

**Acceptance**:
- All 9 units supported
- Conversion works: `Qty 1000 "g"` → `Qty 1 "kg"`
- Unit mismatch detected: `Qty "kg" + Qty "kWh"` → `UnitMismatch` error
- Multiplication/division preserves unit

### Phase 6: Integration & Documentation (Priority: P2)
**Goal**: Integrate all primitives and document usage

**Tasks**:
1. Create `src/BPC/Core/Error.hs` consolidating all error types
2. Export public API from `src/BPC/Core.hs`
3. Write integration test: encode → hash → verify round-trip
4. Update `quickstart.md` with usage examples
5. Run full test suite: `cabal test bpc-core`
6. Verify coverage ≥ 90% (Constitution requirement)

**Acceptance**:
- All exports documented
- Quickstart examples run without modification
- Test coverage ≥ 90%
- No IO imports (verified via grep)

## Verification Checklist

### Code Quality
- [ ] fourmolu formatting applied: `fourmolu -m check $(git ls-files 'src/BPC/Core/*.hs')`
- [ ] hlint passes: `hlint -h hlint.yaml $(git ls-files 'src/BPC/Core/*.hs')`
- [ ] No compiler warnings with `-Wall -Wcompat -Werror`

### Testing
- [ ] Property test `prop_canonicalEncode_deterministic` passes with 10,000 samples
- [ ] Golden test fixtures from SSOT 7.1, 7.2 match exactly
- [ ] Unit tests cover all 9 units and conversions
- [ ] Coverage ≥ 90% for bpc-core package
- [ ] No IO in test helpers (maintain purity)

### Constitution Compliance
- [ ] No IO imports in `src/BPC/Core/**/*.hs` (verified: `grep -r "import.*IO" src/BPC/Core/`)
- [ ] Determinism property test passes
- [ ] Canonical JSON rejects floats/exponents
- [ ] Type-level enforcement prevents unit/scale mismatches

### Documentation
- [ ] All public functions have Haddock comments
- [ ] `quickstart.md` has copy-paste examples
- [ ] `data-model.md` documents all types
- [ ] `research.md` explains algorithm choices

### Integration
- [ ] Exports from `src/BPC/Core.hs` are minimal and well-organized
- [ ] No circular dependencies between modules
- [ ] Build succeeds: `cabal build bpc-core`
- [ ] Tests succeed: `cabal test bpc-core`

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Float rejection breaks existing JSON | HIGH | Golden tests with real-world data; clear error messages |
| Key ordering differs from standard libs | MEDIUM | Property tests with random JSON; document UTF-8 byte order |
| Unit conversion rounding errors | MEDIUM | Use Integer arithmetic scaled by 10^6; document precision limits |
| Type-level programming complexity | LOW | Limit to simple DataKinds; provide helper functions; document examples |
| Performance of canonical encoding | LOW | Benchmark with QuickBench; optimize hot paths if needed |

## Dependencies

**Cabal Dependencies** (add to `bpc-core.cabal`):
- `aeson >= 2.1 && < 2.3` (JSON)
- `cryptonite >= 0.30 && < 0.31` (SHA-256)
- `memory >= 0.18 && < 0.19` (Base32)
- `scientific >= 0.3 && < 0.4` (Number handling)
- `text >= 2.0 && < 2.2`
- `bytestring >= 0.11 && < 0.13`
- `base >= 4.17 && < 5`

**Test Dependencies**:
- `QuickCheck >= 2.14 && < 2.15`
- `HUnit >= 1.6 && < 1.7`
- `hspec >= 2.11 && < 2.12`

**No external service dependencies** (pure library)

## Follow-up Features

After completing 02-core-primitives, these features will depend on it:

1. **03-rule-engine**: Uses `Dec`, `Qty`, canonical JSON for rule evaluation
2. **04-compilation-pipeline**: Uses hashing, canonical encoding for proof/receipt generation
3. **05-data-layer**: Uses domain ID types for repository functions
4. **06-api-server**: Uses canonical JSON for response serialization
5. **07-job-processing**: Uses domain ID types for job dispatch

## Notes

- This is the foundation for the entire system; extra attention to quality and testing is warranted
- All algorithms are specified in SSOT sections 7.1-7.2; no interpretation needed
- Type-level enforcement (DataKinds) prevents entire classes of runtime errors
- IO-free design enables pure testing and easy reasoning about correctness
