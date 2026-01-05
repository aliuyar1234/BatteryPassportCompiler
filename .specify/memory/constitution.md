<!--
Sync Impact Report
==================
Version change: N/A (template) → 1.0.0 (initial)
Modified principles: N/A (initial creation)
Added sections:
  - Core Principles (I-VI)
  - Technical Constraints
  - Quality Gates
  - Governance
Removed sections: N/A
Templates requiring updates:
  - .specify/templates/plan-template.md ✅ (Constitution Check section compatible)
  - .specify/templates/spec-template.md ✅ (Requirements section compatible)
  - .specify/templates/tasks-template.md ✅ (Phase structure compatible)
Follow-up TODOs: None
-->

# BatteryPassportCompiler Constitution

## Core Principles

### I. Determinism (NON-NEGOTIABLE)

Identical inputs MUST produce byte-identical outputs. This is the foundational guarantee enabling audit replay and cryptographic verification.

- Same (Snapshot, Rules, CompilerBuildId, IssuedAt) MUST yield identical (payload, proof, receipt) bytes
- No randomness, timestamps, or environment-dependent values in compilation
- `compilePassportPure` is a pure function: `CompileInput -> Either CompileError CompileOutput`
- Property tests MUST verify: `forall input. compile(input) === compile(input)`

### II. Canonical Storage

Storage is always canonical bytes; JSON pretty-print is never the source of truth.

- All hashed artifacts (`payload_canonical`, `proof_canonical`, `receipt_canonical`, `snapshot_canonical`) stored as `bytea`
- API responses MUST be produced via `canonicalDecode` of stored bytes, never reconstruction
- Canonical JSON (BPC-CJSON-1): keys sorted by UTF-8 bytes, no floats/exponents, no whitespace
- Golden tests MUST compare byte-exact canonical output against fixtures

### III. Immutability

Once created, certain entities MUST NOT be modified. Mutation is replaced by versioning.

- **Immutable**: DocumentVersion, Facts, SEALED DataSnapshot, PUBLISHED RulePackageVersion, PassportVersion
- SEALED snapshots cannot have items added/removed; new snapshot required
- PUBLISHED rules cannot be edited; create new version
- PassportVersions can only transition forward in status: COMPILING → SIGNED → ACTIVE → SUPERSEDED/REVOKED

### IV. Audit Trail

Every write and every denied access MUST generate an Audit Event in the Event Store.

- Event Store is append-only with hash chain (`prev_event_hash` → `event_hash`)
- Events include: `tenant_id`, `aggregate_type`, `aggregate_id`, `aggregate_version`, `event_type`, `actor_id`, `payload`
- Hash chain enables tamper detection via `verifyChain`
- Retention: events and passport_versions default 15 years; document_versions default 10 years

### V. Layered Architecture

Strict dependency rules between packages ensure separation of concerns and testability.

- **bpc-core**: Pure domain logic (DSL, Eval, Proof, Receipt, QR). MUST be IO-free. Cannot import DB/HTTP/Queue.
- **bpc-db**: Database access layer. Can import core. Cannot import api/worker.
- **bpc-api**: HTTP server. Can import core + db. Cannot import worker.
- **bpc-worker**: Job processing. Can import core + db. Cannot import api.
- **bpc-cli**: Admin/ops CLI. Can import core + db. Cannot import api/worker.

Violations of import rules MUST fail CI.

### VI. Type-Safe Rules

The Rule DSL MUST be type-checked before evaluation. Runtime type errors are design failures.

- Parser produces untyped AST; Typecheck transforms to typed `Expr` (GADTs)
- Unit arithmetic enforced: `+/-` only same unit/scale; `*//` Qty×Dec, Qty÷Dec
- Unknown field refs → RULE_TYPE_ERROR; unknown units → UNIT_MISMATCH
- Property tests: `forall input. parse(typecheck(input))` or `RULE_TYPE_ERROR`
- Test coverage requirement: ≥500 test cases for RulePackageVersion publish

## Technical Constraints

### Technology Stack

- **Language**: Haskell (GHC 9.6.4)
- **Build**: Cabal 3.10.2.1
- **Database**: PostgreSQL 16
- **Queue**: RabbitMQ (optional, DB polling fallback)
- **Signature**: ED25519
- **Hashing**: SHA-256, Base32 no-padding for QR

### Effect System

- API/Worker: `ReaderT Env (ExceptT AppError IO)`
- Core: Pure functions only (no `IO` in type signatures)

### Size Limits

- Payload: ≤ 131,072 bytes (PAYLOAD_TOO_LARGE if exceeded)
- Proof: ≤ 262,144 bytes (PROOF_TOO_LARGE if exceeded)
- Receipt: ≤ 16,384 bytes (RECEIPT_TOO_LARGE if exceeded)

### Multi-Tenancy

- All data is tenant-scoped via `tenant_id` foreign key
- Every repository function MUST accept and filter by `TenantId`
- Row-level security enforced at application layer

## Quality Gates

### Code Quality

- **Formatting**: fourmolu (check mode in CI)
- **Linting**: hlint with project hlint.yaml
- **Compiler**: `-Wall -Wcompat -Werror` (warnings are errors)

### Test Coverage (MUST)

| Package | Minimum Coverage |
|---------|------------------|
| bpc-core | ≥ 90% |
| bpc-db | ≥ 80% |
| bpc-worker | ≥ 75% |
| bpc-api | ≥ 70% |

### Required Test Types

- **Property Tests**: Determinism, canonical encoding, proof hashing
- **Golden Tests**: Byte-exact comparison of canonical artifacts
- **Integration Tests**: DB + API + Worker with docker-compose Postgres
- **Rule Tests**: ≥500 test cases before RulePackageVersion can be PUBLISHED

## Governance

This constitution is the supreme authority for BatteryPassportCompiler development. All code, architecture decisions, and PRs MUST comply.

### Amendment Process

1. Propose change with rationale in PR
2. Document impact on existing code
3. Update affected artifacts (SSOT.md, templates, tests)
4. Version bump per semantic versioning:
   - MAJOR: Principle removal or incompatible redefinition
   - MINOR: New principle or material expansion
   - PATCH: Clarification or typo fix

### Compliance Verification

- All PRs MUST pass CI (formatting, linting, tests, coverage)
- Reviewers MUST verify constitution compliance
- Complexity beyond these principles MUST be justified in PR description

### Reference Documents

- **SSOT.md**: Normative specification (DDL, algorithms, API contracts, error codes)
- **CLAUDE.md**: Development guidance for AI assistants

**Version**: 1.0.0 | **Ratified**: 2025-12-28 | **Last Amended**: 2025-12-28
