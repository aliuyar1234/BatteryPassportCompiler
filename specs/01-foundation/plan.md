# Implementation Plan: Foundation & Infrastructure

**Branch**: `01-foundation` | **Date**: 2025-12-28 | **Spec**: [spec.md](../../../.specify/features/01-foundation/spec.md)
**Input**: Feature specification from `.specify/features/01-foundation/spec.md`
**Phase**: P0 | **Status**: Planning

## Summary

Establish the complete project infrastructure including Cabal multi-package setup (5 packages: bpc-core, bpc-db, bpc-api, bpc-worker, bpc-cli), Docker/docker-compose configuration for PostgreSQL 16 and RabbitMQ 3, CI/CD pipelines with GitHub Actions, database migrations with dbmate (4 migrations creating 20+ tables), formatting/linting tools (fourmolu, hlint), and development scripts. This is the absolute foundation upon which all other features depend - without it, no development can proceed.

## Technical Context

**Language/Version**: Haskell GHC 9.6.4
**Build System**: Cabal 3.10.2.1 (multi-package workspace)
**Primary Dependencies**: postgresql-simple, warp, wai, wai-extra, aeson, cryptonite, ed25519, bytestring, text, uuid, time
**Storage**: PostgreSQL 16, RabbitMQ 3 (optional message queue for job triggers)
**Testing**: tasty, tasty-hunit, tasty-quickcheck, hspec, QuickCheck
**Target Platform**: Linux server (containers), local dev on Windows/macOS/Linux
**Project Type**: Cabal multi-package workspace with 5 packages following strict dependency rules
**Performance Goals**: CI pipeline < 10 minutes, fresh build < 5 minutes, incremental rebuild < 30 seconds
**Constraints**: All code must compile with `-Wall -Wcompat -Werror` (warnings are errors), fourmolu formatting enforced, hlint warnings checked
**Scale/Scope**: 5 packages, 4 migrations creating 20+ tables, 2 Docker images (api/worker), 2 CI workflows, 6 development scripts

## Constitution Check

*GATE: All checks passed before Phase 0 research*

| Principle | Status | Notes |
|-----------|--------|-------|
| **I. Determinism (NON-NEGOTIABLE)** | ✅ N/A | Infrastructure phase, no compilation logic yet. Database schema prepared for immutable storage of canonical bytes. |
| **II. Canonical Storage** | ✅ READY | Migration 001 creates bytea columns for `payload_canonical`, `proof_canonical`, `receipt_canonical`, `snapshot_canonical`. JSONB stored only for indexing/queries. |
| **III. Immutability** | ✅ READY | Schema designed for immutable entities: DocumentVersion, Facts, SEALED DataSnapshot, PUBLISHED RulePackageVersion, PassportVersion. Status transitions are one-way. |
| **IV. Audit Trail** | ✅ READY | Migration 001 includes events table with hash chain (`prev_event_hash` → `event_hash`). Every write will generate audit events. |
| **V. Layered Architecture** | ✅ ENFORCED | Package structure follows SSOT 3.2 exactly. Import matrix enforced: bpc-core is IO-free, bpc-db can import core, api/worker can import core+db but not each other. |
| **VI. Type-Safe Rules** | ✅ N/A | Rule engine is separate feature (03-rule-engine). Schema prepared for rule_package_versions with dsl_source. |

**Quality Gates Configured:**
- ✅ Formatting: fourmolu.yaml configured per SSOT 4.3.2 (indentation: 2, comma-style: leading, record-brace-space: false, respectful: true)
- ✅ Linting: hlint.yaml configured per SSOT 4.3.3 (ignores fromJust/camelCase warnings, warns on data vs newtype)
- ✅ Compiler: `-Wall -Wcompat -Werror` in cabal.project global ghc-options
- ✅ EditorConfig: .editorconfig enforces UTF-8, LF line endings, trailing whitespace removal

## Project Structure

### Documentation (this feature)

```text
specs/01-foundation/
├── plan.md                  # This file - implementation plan
├── research.md              # Research findings (build system, migrations, tooling decisions)
├── data-model.md            # Complete DDL reference from SSOT 6.2-6.5
├── quickstart.md            # Developer setup guide (5 minute setup)
└── contracts/               # N/A for infrastructure (README explains)
    └── README.md
```

### Source Code (repository root)

```text
.
├── SSOT.md                  # Normative specification (already exists)
├── README.md                # Project overview and quickstart
├── cabal.project            # Multi-package configuration
├── cabal.project.freeze     # Optional: dependency lock file
├── fourmolu.yaml            # Code formatter config (SSOT 4.3.2)
├── hlint.yaml               # Linter config (SSOT 4.3.3)
├── .editorconfig            # Editor consistency (SSOT 4.3.1)
├── .gitignore               # Haskell + Docker + IDE ignores
├── .env.example             # Template for local .env
├── .github/workflows/
│   ├── ci.yml               # CI: format, lint, build, test (SSOT 4.4.1)
│   └── cd.yml               # CD: Docker image build/push on v* tags (SSOT 4.4.2)
├── docker/
│   ├── Dockerfile.api       # Multi-stage build for bpc-api (SSOT 4.5.3)
│   ├── Dockerfile.worker    # Multi-stage build for bpc-worker (SSOT 4.5.4)
│   ├── Dockerfile.cli       # Multi-stage build for bpc-cli
│   └── entrypoint.sh        # Optional: startup script for containers
├── docker-compose.yml       # Local dev: Postgres + RabbitMQ (SSOT 4.5.1)
├── docker-compose.test.yml  # Integration tests: Postgres on port 55432 (SSOT 4.5.2)
├── migrations/              # dbmate migrations (run order: 001 → 002 → 003 → 004)
│   ├── 001_init.sql         # Core schema: tenants, actors, documents, facts, snapshots, passports, jobs (SSOT 6.2)
│   ├── 002_seed_permissions_roles.sql  # 16 permissions, dev tenant, 3 roles (SSOT 6.3)
│   ├── 003_add_policies_webhooks.sql   # Policy engine, webhook tables (SSOT 6.4)
│   └── 004_add_rate_limits.sql         # Rate limiting buckets (SSOT 6.5)
├── scripts/
│   ├── dev-up.sh            # Start docker-compose services
│   ├── dev-down.sh          # Stop docker-compose services
│   ├── migrate.sh           # Run dbmate migrations (SSOT 4.7)
│   ├── seed-dev.sh          # Seed dev tenant + test API key
│   ├── run-integration-tests.sh  # Run integration tests with test DB
│   └── gen-openapi.sh       # Generate OpenAPI spec (future)
└── packages/
    ├── bpc-core/            # Pure domain logic (IO-free)
    │   ├── bpc-core.cabal
    │   └── src/BPC/Core/    # Placeholder Main.hs initially
    ├── bpc-db/              # Database layer (repos, event store)
    │   ├── bpc-db.cabal
    │   └── src/BPC/DB/
    ├── bpc-api/             # HTTP server (REST + GraphQL)
    │   ├── bpc-api.cabal
    │   └── src/BPC/API/
    ├── bpc-worker/          # Job processing worker
    │   ├── bpc-worker.cabal
    │   └── src/BPC/Worker/
    └── bpc-cli/             # Admin CLI (migrate, seed, verify, export)
        ├── bpc-cli.cabal
        └── src/BPC/CLI/
```

**Structure Decision**: Cabal multi-package workspace with 5 packages following SSOT 4.1 exactly. This structure enforces layered architecture (Constitution Principle V) and prevents circular dependencies.

## Implementation Phases

### Phase 0: Pre-Implementation ✅
- [x] Read SSOT sections 4 (Setup & Tooling) and 6 (Datenmodell)
- [x] Verify constitution compliance (all principles satisfied or N/A)
- [x] Create plan.md, research.md, data-model.md, quickstart.md

### Phase 1: Root Configuration Files
**Goal**: Establish project-wide tooling configuration

**Tasks**:
1. Create `cabal.project` with 5 package paths, GHC 9.6.4, global ghc-options `-Wall -Wcompat -Werror` (SSOT 4.2.1)
2. Create `fourmolu.yaml` with config from SSOT 4.3.2
3. Create `hlint.yaml` with config from SSOT 4.3.3
4. Create `.editorconfig` from SSOT 4.3.1 (UTF-8, LF, 2-space indent for .hs)
5. Create `.gitignore` (dist-newstyle/, .cabal-sandbox/, *.swp, .env, .DS_Store)
6. Create `.env.example` with all ENV vars from SSOT 4.6

**Verification**:
- `cabal build all --dry-run` resolves package paths (will fail until packages exist)
- `fourmolu --version` and `hlint --version` work

**SSOT Reference**: Sections 4.2, 4.3

---

### Phase 2: Package Skeleton (5 packages)
**Goal**: Create minimal buildable Cabal packages

**Tasks**:
1. Create directory structure: `packages/{bpc-core,bpc-db,bpc-api,bpc-worker,bpc-cli}/src/`
2. Create `packages/bpc-core/bpc-core.cabal`:
   - library with `hs-source-dirs: src`, `exposed-modules: BPC.Core.Placeholder`
   - `build-depends: base >= 4.17 && < 5`
   - `default-language: Haskell2010`
3. Create `packages/bpc-db/bpc-db.cabal`:
   - library + test suite
   - `build-depends: base, bpc-core, postgresql-simple`
4. Create `packages/bpc-api/bpc-api.cabal`:
   - library + executable `bpc-api`
   - `main-is: Main.hs`, `build-depends: base, bpc-core, bpc-db, warp, wai`
5. Create `packages/bpc-worker/bpc-worker.cabal`:
   - library + executable `bpc-worker`
   - `build-depends: base, bpc-core, bpc-db`
6. Create `packages/bpc-cli/bpc-cli.cabal`:
   - executable `bpc-cli`
   - `build-depends: base, bpc-core, bpc-db`
7. Create placeholder `Main.hs` for executables (just `main = putStrLn "TODO"`)
8. Create placeholder `BPC/Core/Placeholder.hs` (module with dummy function)

**Verification**:
- `cabal build all` succeeds
- `cabal run bpc-api:exe:bpc-api` prints "TODO"

**SSOT Reference**: Sections 3.2, 4.2.2

---

### Phase 3: Docker & Compose
**Goal**: Local development and CI database/queue services

**Tasks**:
1. Create `docker-compose.yml` from SSOT 4.5.1:
   - postgres:16 service (port 5432, user/pass/db=bpc)
   - rabbitmq:3-management service (ports 5672, 15672)
2. Create `docker-compose.test.yml` from SSOT 4.5.2:
   - postgres:16 service (port 55432, db=bpc_test)
3. Create `docker/Dockerfile.api` from SSOT 4.5.3:
   - Multi-stage: haskell:9.6.4 build → debian:bookworm-slim runtime
   - Copy dist-newstyle artifacts, install libpq5 + ca-certificates
4. Create `docker/Dockerfile.worker` from SSOT 4.5.4 (similar to api)
5. Create `docker/Dockerfile.cli` (similar to api/worker)

**Verification**:
- `docker compose up -d` starts Postgres and RabbitMQ
- `docker compose exec postgres pg_isready -U bpc -d bpc` succeeds
- `docker build -f docker/Dockerfile.api .` builds successfully

**SSOT Reference**: Section 4.5

---

### Phase 4: Database Migrations
**Goal**: Complete database schema with 4 migrations

**Tasks**:
1. Install dbmate: `brew install dbmate` (macOS) or download binary (Linux/Windows)
2. Create `migrations/001_init.sql` from SSOT 6.2:
   - ENUMS: actor_type, job_type, job_status, document_kind, document_status, snapshot_status, rule_pkg_status, passport_status, access_decision
   - TABLES: tenants, actors, api_keys, roles, permissions, role_permissions, actor_roles, idempotency_keys, events, documents, document_versions, facts, data_snapshots, snapshot_items, battery_products, passports, rule_packages, rule_package_versions, rule_fields, rule_tests_runs, passport_versions, jobs
   - INDEXES: 15+ indexes on tenant_id, aggregate lookups, job queue
   - CONSTRAINTS: Foreign keys, CHECK constraints, UNIQUE constraints
3. Create `migrations/002_seed_permissions_roles.sql` from SSOT 6.3:
   - INSERT 16 permissions (passport:read, passport:compile, etc.)
   - INSERT dev tenant (slug='dev')
   - INSERT 3 roles (Admin, ComplianceOfficer, Auditor) with role_permissions
4. Create `migrations/003_add_policies_webhooks.sql` from SSOT 6.4:
   - TABLES: policies, policy_versions, webhook_endpoints, webhook_subscriptions, webhook_deliveries
5. Create `migrations/004_add_rate_limits.sql` from SSOT 6.5:
   - TABLE: rate_limit_buckets

**Verification**:
- `./scripts/migrate.sh` runs without errors
- `docker compose exec postgres psql -U bpc -d bpc -c "\dt"` shows 20+ tables
- `docker compose exec postgres psql -U bpc -d bpc -c "SELECT COUNT(*) FROM permissions"` returns 16

**SSOT Reference**: Sections 6.2, 6.3, 6.4, 6.5

---

### Phase 5: Development Scripts
**Goal**: Automate common development tasks

**Tasks**:
1. Create `scripts/migrate.sh` from SSOT 4.7:
   ```bash
   #!/usr/bin/env bash
   set -euo pipefail
   export DATABASE_URL="${DATABASE_URL:-postgres://bpc:bpc@${BPC_DB_HOST:-localhost}:${BPC_DB_PORT:-5432}/${BPC_DB_NAME:-bpc}?sslmode=disable}"
   dbmate up
   ```
2. Create `scripts/seed-dev.sh`:
   - INSERT test actor + API key into dev tenant
   - GRANT Admin role to test actor
3. Create `scripts/dev-up.sh`: `docker compose up -d`
4. Create `scripts/dev-down.sh`: `docker compose down`
5. Create `scripts/run-integration-tests.sh`:
   - Start docker-compose.test.yml
   - Run migrations on test DB
   - Run `cabal test bpc-db:integration bpc-api:integration bpc-worker:integration`
   - Stop test DB
6. Make all scripts executable: `chmod +x scripts/*.sh`

**Verification**:
- `./scripts/dev-up.sh && ./scripts/migrate.sh && ./scripts/seed-dev.sh` completes successfully
- Test actor and API key exist in database
- `./scripts/run-integration-tests.sh` runs (will pass when tests implemented)

**SSOT Reference**: Section 4.7

---

### Phase 6: CI/CD Pipelines
**Goal**: Automated quality gates and deployment

**Tasks**:
1. Create `.github/workflows/ci.yml` from SSOT 4.4.1:
   - Trigger: push to main, pull_request
   - Jobs:
     - Setup: GHC 9.6.4, Cabal 3.10.2.1, cache ~/.cabal/store + dist-newstyle
     - Format: `fourmolu -m check $(git ls-files '*.hs')`
     - Lint: `hlint -h hlint.yaml $(git ls-files '*.hs')`
     - Build: `cabal build all`
     - Unit Tests: `cabal test all`
     - Integration Tests: Start Postgres service (port 55432), run migrations, `cabal test bpc-db:integration bpc-api:integration bpc-worker:integration`
2. Create `.github/workflows/cd.yml` from SSOT 4.4.2:
   - Trigger: push tags v*
   - Jobs:
     - Docker: Build and push ghcr.io/{owner}/bpc-api:$tag and ghcr.io/{owner}/bpc-worker:$tag
     - Uses: docker/setup-buildx-action, docker/login-action, docker/build-push-action

**Verification**:
- Push to branch triggers CI workflow
- All steps pass (format, lint, build, test)
- Tag v0.1.0 triggers CD workflow and pushes images to GHCR

**SSOT Reference**: Section 4.4

---

## Dependencies

### External Tools (Developer Machine)
- GHC 9.6.4 (install via ghcup)
- Cabal 3.10.2.1 (install via ghcup)
- Docker + Docker Compose (v2.x+)
- dbmate (migration tool) - `brew install dbmate` or download binary
- fourmolu (formatter) - `cabal install fourmolu`
- hlint (linter) - `cabal install hlint`

### Runtime Dependencies
- PostgreSQL 16 (Docker container for dev, managed service for prod)
- RabbitMQ 3 (optional, Docker container for dev)

### Haskell Package Dependencies
See individual .cabal files. Key dependencies:
- base >= 4.17 && < 5
- postgresql-simple (DB access)
- warp, wai, wai-extra (HTTP server)
- aeson (JSON)
- cryptonite, ed25519 (crypto)
- bytestring, text, uuid, time (core types)

## Complexity Tracking

**No violations** - this feature follows Constitution exactly:
- Package count: 5 (prescribed by SSOT 3.2)
- No Repository pattern yet (db layer is separate feature)
- No additional abstraction layers beyond what SSOT specifies

## Verification Checklist

### Phase 1: Root Config
- [ ] `cabal.project` exists with 5 package paths
- [ ] `fourmolu.yaml` matches SSOT 4.3.2
- [ ] `hlint.yaml` matches SSOT 4.3.3
- [ ] `.editorconfig` matches SSOT 4.3.1

### Phase 2: Package Skeleton
- [ ] `cabal build all` compiles all 5 packages without warnings
- [ ] `cabal run bpc-api:exe:bpc-api` runs
- [ ] `cabal run bpc-worker:exe:bpc-worker` runs
- [ ] `cabal run bpc-cli:exe:bpc-cli` runs

### Phase 3: Docker
- [ ] `docker compose up -d` starts Postgres and RabbitMQ
- [ ] `docker compose exec postgres pg_isready` succeeds
- [ ] `docker build -f docker/Dockerfile.api .` succeeds
- [ ] `docker build -f docker/Dockerfile.worker .` succeeds

### Phase 4: Migrations
- [ ] `./scripts/migrate.sh` creates all tables
- [ ] `docker compose exec postgres psql -U bpc -d bpc -c "\dt"` shows 20+ tables
- [ ] `docker compose exec postgres psql -U bpc -d bpc -c "SELECT * FROM permissions"` returns 16 rows
- [ ] `docker compose exec postgres psql -U bpc -d bpc -c "SELECT slug FROM tenants"` returns 'dev'

### Phase 5: Scripts
- [ ] `./scripts/seed-dev.sh` creates test actor and API key
- [ ] All scripts are executable (chmod +x)
- [ ] `./scripts/run-integration-tests.sh` completes (may have 0 tests initially)

### Phase 6: CI/CD
- [ ] `fourmolu -m check $(git ls-files '*.hs')` passes
- [ ] `hlint -h hlint.yaml $(git ls-files '*.hs')` passes
- [ ] CI workflow runs in < 10 minutes
- [ ] CD workflow builds Docker images on tag push

### Overall Success Criteria (SSOT-defined)
- [ ] **SC-001**: `cabal build all` completes in < 5 minutes (fresh build)
- [ ] **SC-002**: CI pipeline runs in < 10 minutes
- [ ] **SC-003**: `docker compose up -d && ./scripts/migrate.sh && ./scripts/seed-dev.sh` completes in < 2 minutes
- [ ] **SC-004**: All tooling files (fourmolu.yaml, hlint.yaml, .editorconfig) are byte-identical to SSOT specifications

## Next Steps After Foundation Complete

Once this feature is implemented and verified:

1. **Feature 02: Core Primitives** - Implement canonical JSON, hashing, domain types (BPC.Core.CanonicalJson, BPC.Core.Hash, BPC.Core.Types)
2. **Feature 03: Rule Engine** - Implement DSL parser, typechecker, evaluator (BPC.Core.Rules.*)
3. **Feature 04: Compilation Pipeline** - Implement pure compiler, proof builder, receipt builder (BPC.Core.Eval, BPC.Core.Proof, BPC.Core.Receipt)
4. **Feature 05: Data Layer** - Implement repositories, event store (BPC.DB.Repos.*, BPC.DB.Pool)
5. **Feature 06: API Server** - Implement HTTP handlers, auth middleware (BPC.API.*)
6. **Feature 07: Job Processing** - Implement worker loop, job handlers (BPC.Worker.*)
7. **Feature 08: Advanced Features** - Implement policies, webhooks, rate limiting

## References

- **SSOT.md**: Normative specification (sections 4, 6 for this feature)
- **Constitution**: .specify/memory/constitution.md (Principles I-VI)
- **.specify/features/01-foundation/spec.md**: Feature specification with user stories
