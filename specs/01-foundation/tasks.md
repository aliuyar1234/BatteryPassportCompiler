# Tasks: Foundation & Infrastructure

**Input**: Design documents from `specs/01-foundation/`
**Prerequisites**: plan.md (required), spec.md (required), data-model.md
**Phase**: P0 - Absolute foundation, blocks ALL other features

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US5)
- Include exact file paths in descriptions

## Path Conventions

- **Multi-package Cabal workspace**: `packages/{bpc-core,bpc-db,bpc-api,bpc-worker,bpc-cli}/`
- **Config files**: Repository root
- **Migrations**: `migrations/`
- **Scripts**: `scripts/`
- **Docker**: `docker/`
- **CI/CD**: `.github/workflows/`

---

## Phase 1: Setup (Root Configuration)

**Purpose**: Establish project-wide tooling and configuration

- [X] T001 Create `cabal.project` with 5 package paths, GHC 9.6.4, global ghc-options `-Wall -Wcompat -Werror`
- [X] T002 [P] Create `fourmolu.yaml` with SSOT 4.3.2 config (indentation: 2, comma-style: leading)
- [X] T003 [P] Create `hlint.yaml` with SSOT 4.3.3 config (ignore fromJust/camelCase)
- [X] T004 [P] Create `.editorconfig` with SSOT 4.3.1 config (UTF-8, LF, 2-space .hs)
- [X] T005 [P] Create `.gitignore` (dist-newstyle/, .cabal-sandbox/, *.swp, .env, .DS_Store)
- [X] T006 [P] Create `.env.example` with all ENV vars from SSOT 4.6

**Checkpoint**: Root config files ready for package creation

---

## Phase 2: Foundational (Package Skeleton)

**Purpose**: Create minimal buildable 5-package Cabal workspace - BLOCKS all user stories

**CRITICAL**: No user story work can begin until this phase is complete

- [X] T007 Create directory structure: `packages/{bpc-core,bpc-db,bpc-api,bpc-worker,bpc-cli}/src/BPC/{Core,DB,API,Worker,CLI}/`
- [X] T008 Create `packages/bpc-core/bpc-core.cabal` with library (base, hs-source-dirs: src)
- [X] T009 [P] Create `packages/bpc-core/src/BPC/Core/Placeholder.hs` with dummy module
- [X] T010 Create `packages/bpc-db/bpc-db.cabal` with library + test-suite (depends: bpc-core, postgresql-simple)
- [X] T011 [P] Create `packages/bpc-db/src/BPC/DB/Placeholder.hs` with dummy module
- [X] T012 Create `packages/bpc-api/bpc-api.cabal` with library + executable (depends: bpc-core, bpc-db, warp, wai)
- [X] T013 [P] Create `packages/bpc-api/src/BPC/API/Main.hs` with `main = putStrLn "bpc-api TODO"`
- [X] T014 Create `packages/bpc-worker/bpc-worker.cabal` with library + executable (depends: bpc-core, bpc-db)
- [X] T015 [P] Create `packages/bpc-worker/src/BPC/Worker/Main.hs` with `main = putStrLn "bpc-worker TODO"`
- [X] T016 Create `packages/bpc-cli/bpc-cli.cabal` with executable (depends: bpc-core, bpc-db)
- [X] T017 [P] Create `packages/bpc-cli/src/BPC/CLI/Main.hs` with `main = putStrLn "bpc-cli TODO"`
- [X] T018 Verify `cabal build all` compiles without errors

**Checkpoint**: Foundation ready - `cabal build all` succeeds, user story implementation can begin

---

## Phase 3: User Story 1 - Developer Environment Setup (Priority: P1) MVP

**Goal**: Developer can clone repo, run `docker compose up -d && ./scripts/migrate.sh && cabal build all` successfully

**Independent Test**: Fresh repo clone, run setup commands, all must succeed without errors

### Implementation for User Story 1

- [X] T019 [US1] Create `docker-compose.yml` with postgres:16 (port 5432) and rabbitmq:3-management (ports 5672, 15672)
- [X] T020 [US1] Create `docker-compose.test.yml` with postgres:16 (port 55432, db=bpc_test)
- [X] T021 [US1] Create `migrations/001_init.sql` with all ENUMs from SSOT 6.2 (actor_type, job_type, job_status, etc.)
- [X] T022 [US1] Add to `migrations/001_init.sql`: tenants, actors, api_keys tables
- [X] T023 [US1] Add to `migrations/001_init.sql`: roles, permissions, role_permissions, actor_roles tables
- [X] T024 [US1] Add to `migrations/001_init.sql`: events table with hash chain columns
- [X] T025 [US1] Add to `migrations/001_init.sql`: documents, document_versions, facts tables
- [X] T026 [US1] Add to `migrations/001_init.sql`: data_snapshots, snapshot_items tables
- [X] T027 [US1] Add to `migrations/001_init.sql`: battery_products, passports, passport_versions tables
- [X] T028 [US1] Add to `migrations/001_init.sql`: rule_packages, rule_package_versions, rule_fields, rule_tests_runs tables
- [X] T029 [US1] Add to `migrations/001_init.sql`: jobs, idempotency_keys tables
- [X] T030 [US1] Add to `migrations/001_init.sql`: All indexes (jobs_queue_idx, tenant_id indexes, aggregate lookups)
- [X] T031 [US1] Create `scripts/migrate.sh` with dbmate up command and DATABASE_URL handling
- [X] T032 [US1] Make `scripts/migrate.sh` executable (chmod +x)
- [X] T033 [US1] Create `README.md` with prerequisites (GHC 9.6.4, Cabal, Docker, dbmate) and quickstart

**Checkpoint**: `docker compose up -d && ./scripts/migrate.sh && cabal build all` works on fresh clone

---

## Phase 4: User Story 2 - Code Quality Gates (Priority: P1)

**Goal**: Developer can run format/lint checks locally before committing

**Independent Test**: Run `fourmolu -m check` and `hlint` on codebase

### Implementation for User Story 2

- [X] T034 [US2] Verify `fourmolu.yaml` is correctly configured per SSOT 4.3.2
- [X] T035 [US2] Verify `hlint.yaml` is correctly configured per SSOT 4.3.3
- [X] T036 [US2] Add format check instructions to `README.md`: `fourmolu -m check $(git ls-files '*.hs')`
- [X] T037 [US2] Add lint check instructions to `README.md`: `hlint -h hlint.yaml $(git ls-files '*.hs')`
- [X] T038 [US2] Ensure all placeholder .hs files pass fourmolu and hlint

**Checkpoint**: `fourmolu -m check` and `hlint` pass on all .hs files

---

## Phase 5: User Story 3 - CI Pipeline (Priority: P2)

**Goal**: PRs are automatically checked for format, lint, build, and tests

**Independent Test**: Create PR and observe CI pipeline execution

### Implementation for User Story 3

- [X] T039 [US3] Create `.github/workflows/ci.yml` with trigger on push to main and pull_request
- [X] T040 [US3] Add CI job: Setup GHC 9.6.4 and Cabal 3.10.2.1 with caching
- [X] T041 [US3] Add CI job: Format check with fourmolu
- [X] T042 [US3] Add CI job: Lint check with hlint
- [X] T043 [US3] Add CI job: Build all packages with `cabal build all`
- [X] T044 [US3] Add CI job: Unit tests with `cabal test all`
- [X] T045 [US3] Add CI service: PostgreSQL 16 on port 55432 for integration tests
- [X] T046 [US3] Add CI job: Integration tests with migration + `cabal test bpc-db:integration`

**Checkpoint**: CI pipeline runs on PR, all checks pass

---

## Phase 6: User Story 4 - Database Seeding (Priority: P2)

**Goal**: Developer has preconfigured test data (tenant, roles, permissions) for local development

**Independent Test**: Run `./scripts/seed-dev.sh` and verify dev tenant exists with roles

### Implementation for User Story 4

- [X] T047 [US4] Create `migrations/002_seed_permissions_roles.sql` with INSERT for 16 permissions
- [X] T048 [US4] Add to `migrations/002_seed_permissions_roles.sql`: INSERT dev tenant (slug='dev')
- [X] T049 [US4] Add to `migrations/002_seed_permissions_roles.sql`: INSERT 3 roles (Admin, ComplianceOfficer, Auditor)
- [X] T050 [US4] Add to `migrations/002_seed_permissions_roles.sql`: INSERT role_permissions mappings
- [X] T051 [US4] Create `scripts/seed-dev.sh` to INSERT test actor + API key into dev tenant
- [X] T052 [US4] Add to `scripts/seed-dev.sh`: GRANT Admin role to test actor
- [X] T053 [US4] Make `scripts/seed-dev.sh` executable (chmod +x)

**Checkpoint**: `./scripts/seed-dev.sh` creates dev tenant with Admin, ComplianceOfficer, Auditor roles

---

## Phase 7: User Story 5 - Docker Image Build (Priority: P3)

**Goal**: DevOps can build production Docker images for deployment

**Independent Test**: `docker build -f docker/Dockerfile.api .` succeeds

### Implementation for User Story 5

- [X] T054 [US5] Create `docker/Dockerfile.api` with multi-stage build (haskell:9.6.4 → debian:bookworm-slim)
- [X] T055 [US5] Create `docker/Dockerfile.worker` with multi-stage build
- [X] T056 [US5] Create `docker/Dockerfile.cli` with multi-stage build
- [X] T057 [US5] Create `.github/workflows/cd.yml` with trigger on tags v*
- [X] T058 [US5] Add CD job: Build and push bpc-api image to ghcr.io
- [X] T059 [US5] Add CD job: Build and push bpc-worker image to ghcr.io

**Checkpoint**: Docker images build successfully, CD pushes to GHCR on v* tags

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [X] T060 Create `migrations/003_add_policies_webhooks.sql` with policies, policy_versions, webhook_* tables
- [X] T061 Create `migrations/004_add_rate_limits.sql` with rate_limit_buckets table
- [X] T062 [P] Create `scripts/dev-up.sh` wrapper for docker compose up -d
- [X] T063 [P] Create `scripts/dev-down.sh` wrapper for docker compose down
- [X] T064 [P] Create `scripts/run-integration-tests.sh` for test DB + migrations + tests
- [X] T065 Make all scripts executable (chmod +x scripts/*.sh)
- [X] T066 Verify all Success Criteria from spec.md (SC-001 through SC-004)
- [X] T067 Run quickstart.md validation end-to-end

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - US1 (Dev Environment) can proceed immediately after Foundational
  - US2 (Code Quality) can proceed in parallel with US1
  - US3 (CI Pipeline) depends on US1 + US2 completion
  - US4 (DB Seeding) depends on US1 completion (needs migrations)
  - US5 (Docker Images) can proceed in parallel after Foundational
- **Polish (Phase 8)**: Depends on all user stories being complete

### User Story Dependencies

```
Phase 2: Foundational
     │
     ├───────────────────────────┬────────────────────┐
     ▼                           ▼                    ▼
Phase 3: US1 (Dev Env)    Phase 4: US2 (Quality)   Phase 7: US5 (Docker)
     │                           │
     ├───────────────────────────┤
     ▼                           ▼
Phase 5: US3 (CI) ◄──────────────┘
     │
     ▼
Phase 6: US4 (Seeding)
     │
     ▼
Phase 8: Polish
```

### Within Each Phase

- Tasks without [P] marker must be sequential
- Tasks with [P] marker can run in parallel (different files)
- Migration files (001_init.sql) are sequential additions to same file

### Parallel Opportunities

**Phase 1 (Setup)**: T002, T003, T004, T005, T006 can all run in parallel after T001
**Phase 2 (Foundational)**: T009, T011, T013, T015, T017 can run in parallel
**Phase 8 (Polish)**: T062, T063, T064 can run in parallel

---

## Parallel Example: Phase 1 Setup

```bash
# After T001 (cabal.project) is complete, launch in parallel:
Task: "Create fourmolu.yaml" (T002)
Task: "Create hlint.yaml" (T003)
Task: "Create .editorconfig" (T004)
Task: "Create .gitignore" (T005)
Task: "Create .env.example" (T006)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T006)
2. Complete Phase 2: Foundational (T007-T018) - CRITICAL
3. Complete Phase 3: User Story 1 (T019-T033)
4. **STOP and VALIDATE**: `docker compose up -d && ./scripts/migrate.sh && cabal build all`
5. Developer environment is functional

### Incremental Delivery

1. Setup + Foundational → Cabal workspace builds
2. Add US1 (Dev Environment) → Full local dev setup works
3. Add US2 (Code Quality) → Format/lint gates work
4. Add US3 (CI Pipeline) → Automated PR checks
5. Add US4 (DB Seeding) → Test data available
6. Add US5 (Docker Images) → Deployment ready
7. Polish → All migrations, helper scripts

---

## Summary

- **Total Tasks**: 67
- **Phase 1 (Setup)**: 6 tasks
- **Phase 2 (Foundational)**: 12 tasks
- **Phase 3 (US1 - Dev Env)**: 15 tasks
- **Phase 4 (US2 - Quality)**: 5 tasks
- **Phase 5 (US3 - CI)**: 8 tasks
- **Phase 6 (US4 - Seeding)**: 7 tasks
- **Phase 7 (US5 - Docker)**: 6 tasks
- **Phase 8 (Polish)**: 8 tasks

**MVP Scope**: Phases 1-3 (33 tasks) for functional developer environment
**Parallel Opportunities**: 15 tasks marked [P]

**STATUS**: All 67 tasks completed
