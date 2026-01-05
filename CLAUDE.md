# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

BatteryPassportCompiler (BPC) is a deterministic compiler for Battery Passports with cryptographic proof objects, ED25519 signatures, QR codes, and audit replay capability. The core pipeline: Upload DocumentVersion → Parse Facts → Build+Seal Snapshot → Compile (pure) → Sign → QR → Activate → Replay.

## Critical Invariants

- **Determinism**: Same inputs must produce byte-identical artifacts (payload/proof/receipt)
- **Canonical Storage**: Storage is always canonical bytes; JSON pretty-print is never the source of truth
- **Immutability**: DocumentVersion, Facts, SEALED Snapshots, PUBLISHED Rules, PassportVersion are immutable
- **Audit Trail**: Every write and denied access generates an Audit Event (Event Store)

## Build & Development Commands

```bash
# Start dependencies
docker compose up -d postgres rabbitmq

# Database setup
./scripts/migrate.sh          # Run dbmate migrations
./scripts/seed-dev.sh         # Seed development data

# Build
cabal build all               # Build all packages

# Test
cabal test all                # Run all tests
cabal test bpc-db:integration # Run integration tests for bpc-db
cabal test bpc-api:integration
cabal test bpc-worker:integration

# Run services
cabal run bpc-api:exe:bpc-api
cabal run bpc-worker:exe:bpc-worker

# Formatting & linting
fourmolu -m check $(git ls-files '*.hs')
hlint -h hlint.yaml $(git ls-files '*.hs')
```

## Architecture

### Package Structure

| Package | Path | Purpose |
|---------|------|---------|
| bpc-core | `packages/bpc-core` | Pure domain logic: DSL, Evaluator, Canonicalization, Proof, Receipt (IO-free) |
| bpc-db | `packages/bpc-db` | DB Pool, Repositories, Event Store, migrations |
| bpc-api | `packages/bpc-api` | HTTP server, handlers, auth/rate-limit middleware, OpenAPI/GraphQL |
| bpc-worker | `packages/bpc-worker` | Worker loop, job dispatch, handlers (parse/build/compile/sign/qr/webhook) |
| bpc-cli | `packages/bpc-cli` | Admin/ops CLI (migrate/seed/verify/export) |

### Dependency Rules (MUST follow)

- `bpc-core` is IO-free; cannot import DB/HTTP/Queue
- `bpc-db` can import `bpc-core`; cannot import api/worker
- `bpc-api`/`bpc-worker`/`bpc-cli` can import `bpc-core` + `bpc-db`
- `bpc-api` and `bpc-worker` cannot import each other

### Module Import Matrix

| Importer | Can Import | Cannot Import |
|----------|------------|---------------|
| `BPC.Core.*` | `BPC.Core.*` | `BPC.DB.*`, `BPC.API.*`, `BPC.Worker.*` |
| `BPC.DB.*` | `BPC.Core.*`, `BPC.DB.*` | `BPC.API.*`, `BPC.Worker.*` |
| `BPC.API.*` | `BPC.Core.*`, `BPC.DB.*` | `BPC.Worker.*` |
| `BPC.Worker.*` | `BPC.Core.*`, `BPC.DB.*` | `BPC.API.*` |

### Effect System

- API/Worker: `ReaderT Env (ExceptT AppError IO)`
- Core: Pure functions only (`compilePassportPure :: CompileInput -> Either CompileError CompileOutput`)

## Key Algorithms & Formats

### Canonical JSON (BPC-CJSON-1)
- Object keys sorted by UTF-8 bytes
- Floats/exponent numbers forbidden
- No whitespace

### Versioned Formats
- **BPC-SNAPSHOT-1**: Snapshot seal format
- **BPC-PROOF-1**: Derivation tree with node hashes
- **BPC-RECEIPT-1**: Machine-verifiable receipt
- **BPC-QR-1**: `BPC1|pv=<uuid>|ph=<b32>|pr=<b32>|rh=<b32>`
- **BPC-SIGN-1**: ED25519 signature over receipt hash

### Size Limits
- Payload: ≤ 131,072 bytes
- Proof: ≤ 262,144 bytes
- Receipt: ≤ 16,384 bytes

## Rule DSL

Rules define passport field computations with type-safe expressions:

```text
field battery.capacity_kwh: Dec(6) =
  let b = getFact("Battery","battery:SKU-123");
  let cap = requireSome(recordGet(requireSome(b,"E001","missing"),"capacity_kwh"),"E002","missing");
  toDec(6, cap);
```

Built-ins include: `getFact`, `getFactsByPrefix`, `recordGet`, `isSome`, `unwrapOr`, `requireSome`, `toDec`, `toQty`, `convert`, `sumQty`, `map`, `filter`, `fold`, `emitCompliance`.

## Status Enums

- **job_status**: QUEUED | RUNNING | SUCCEEDED | FAILED | CANCELLED | DEAD_LETTER
- **document_status**: UPLOADED | PARSED | VALIDATED | REJECTED
- **snapshot_status**: BUILDING | READY | SEALED
- **rule_pkg_status**: DRAFT | VALIDATED | PUBLISHED | DEPRECATED | RETIRED
- **passport_status**: COMPILING | SIGNED | ACTIVE | SUPERSEDED | REVOKED

## SSOT.md

The `SSOT.md` file is the **normative specification**. It contains the complete DDL, algorithm definitions, API contracts, and error codes. When in doubt, refer to SSOT.md as the authoritative source.

## Speckit Workflow

This project uses the Speckit template for specification-driven development. Available slash commands:
- `/speckit.specify` - Create/update feature specification
- `/speckit.clarify` - Identify underspecified areas
- `/speckit.plan` - Generate implementation plan
- `/speckit.tasks` - Generate task list
- `/speckit.implement` - Execute implementation plan
