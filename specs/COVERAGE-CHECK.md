# SSOT Coverage Cross-Check

**Date**: 2025-12-28
**SSOT Version**: v0.2

## Phase → Spec Mapping

| SSOT Phase | SSOT Tasks | Spec Coverage | Status |
|------------|------------|---------------|--------|
| **P0** Foundation | P0-T1 to P0-T4 | `01-foundation/` | ✅ |
| **P1** bpc-core | P1-T1 (CJSON, Hash) | `02-core-primitives/` | ✅ |
| **P1** bpc-core | P1-T2 (DSL, Parser, Typecheck) | `03-rule-engine/` | ✅ |
| **P1** bpc-core | P1-T3, P1-T4 (Compile, Proof, Receipt, QR) | `04-compilation-pipeline/` | ✅ |
| **P2** DB+API+Worker | P2-T1 (DB Pool, Repos, Events) | `05-data-layer/` | ✅ |
| **P2** DB+API+Worker | P2-T2 (API, Auth, Handlers) | `06-api-server/` | ✅ |
| **P2** DB+API+Worker | P2-T3 (Worker Loop, Jobs) | `07-job-processing/` | ✅ |
| **P2** DB+API+Worker | P2-T4 (Observability) | `06-api-server/` | ✅ |
| **P3** Pipeline | P3-T1 to P3-T4 (Compile→Activate→Replay) | `06-api-server/`, `07-job-processing/` | ✅ |
| **P4** Advanced | P4-T1 (Policies) | `08-advanced-features/` | ✅ |
| **P4** Advanced | P4-T2 (Rate Limiting) | `08-advanced-features/` | ✅ |
| **P4** Advanced | P4-T3 (Webhooks) | `08-advanced-features/` | ✅ |

## SSOT Section → Spec Mapping

| SSOT Section | Topic | Covered In |
|--------------|-------|------------|
| **0** | Non-negotiable Invariants | All specs (constitution) |
| **2** | Error Codes | `06-api-server/data-model.md` |
| **3** | Contexts & Modules | All specs |
| **3.2** | Package Structure | `01-foundation/` |
| **3.5** | Public Interfaces | Package-specific specs |
| **4** | Setup & Tooling | `01-foundation/` |
| **4.1** | Repo Structure | `01-foundation/plan.md` |
| **4.2** | Cabal | `01-foundation/plan.md` |
| **4.3** | Formatting/Linting | `01-foundation/research.md` |
| **4.4** | CI/CD | `01-foundation/plan.md` |
| **4.5** | Docker | `01-foundation/plan.md` |
| **4.6** | ENV Schema | `01-foundation/data-model.md` |
| **5.5** | Type System | `02-core-primitives/data-model.md` |
| **6.2** | DDL Schema | `01-foundation/data-model.md`, `05-data-layer/data-model.md` |
| **6.3** | Seeds | `01-foundation/` |
| **6.4** | Migration 003 | `08-advanced-features/` |
| **6.5** | Migration 004 | `08-advanced-features/` |
| **6.6** | Retention Defaults | `08-advanced-features/` |
| **7.1** | Canonical JSON (BPC-CJSON-1) | `02-core-primitives/` |
| **7.2** | SHA-256 Hashing | `02-core-primitives/` |
| **7.3** | Base32 | `02-core-primitives/` |
| **7.4** | Event Store Hash Chain | `05-data-layer/` |
| **7.5** | TopoSort | `03-rule-engine/` |
| **7.6** | compilePassportPure | `04-compilation-pipeline/` |
| **7.7** | Snapshot Seal | `05-data-layer/`, `07-job-processing/` |
| **7.8** | QR Payload (BPC-QR-1) | `04-compilation-pipeline/` |
| **7.9** | Rate Limiting (BPC-RL-1) | `08-advanced-features/` |
| **7.10** | Webhook HMAC (BPC-WH-1) | `08-advanced-features/` |
| **7.11** | Idempotency (BPC-IDEMP-1) | `08-advanced-features/` |
| **8.1** | Rule DSL EBNF | `03-rule-engine/data-model.md` |
| **8.2** | Built-in Functions | `03-rule-engine/data-model.md` |
| **8.3** | Type Checking | `03-rule-engine/` |
| **8.4** | DSL Example | `03-rule-engine/quickstart.md` |
| **9.1** | Proof Schema (BPC-PROOF-1) | `04-compilation-pipeline/` |
| **9.2** | Proof Node Hash | `04-compilation-pipeline/` |
| **9.3** | Receipt Schema (BPC-RECEIPT-1) | `04-compilation-pipeline/` |
| **9.4** | Signature (BPC-SIGN-1) | `04-compilation-pipeline/` |
| **9.5** | Retention Policy | `08-advanced-features/` |
| **9.6** | Replay Algorithm | `04-compilation-pipeline/`, `06-api-server/` |
| **10** | API | `06-api-server/` |
| **10.4** | Endpoints | `06-api-server/contracts/openapi.yaml` |
| **10.5** | OpenAPI | `06-api-server/contracts/` |
| **10.6** | Webhook Events | `08-advanced-features/` |
| **11** | Observability | `06-api-server/` |
| **12** | Deployment/Ops | `01-foundation/` |
| **13** | Testing | All specs (verification checklists) |
| **14** | Tasks | Mapped to implementation phases |
| **14.7** | Permissions Matrix | `01-foundation/data-model.md`, `06-api-server/` |

## Feature Completeness

### MVP Features (P0-P3)

| Feature | SSOT Reference | Spec | Status |
|---------|----------------|------|--------|
| Repo scaffold | P0-T1 | `01-foundation/` | ✅ |
| CI/CD | P0-T2, P0-T4 | `01-foundation/` | ✅ |
| Docker Compose | P0-T3 | `01-foundation/` | ✅ |
| Migrations | P0-T3 | `01-foundation/` | ✅ |
| Canonical JSON | P1-T1, 7.1 | `02-core-primitives/` | ✅ |
| SHA-256 Hashing | P1-T1, 7.2 | `02-core-primitives/` | ✅ |
| Domain Types | 5.5 | `02-core-primitives/` | ✅ |
| Quantity/Units | 5.5, 8.3 | `02-core-primitives/` | ✅ |
| Rule DSL Parser | P1-T2, 8.1 | `03-rule-engine/` | ✅ |
| Rule Typechecker | P1-T2, 8.3 | `03-rule-engine/` | ✅ |
| TopoSort | P1-T2, 7.5 | `03-rule-engine/` | ✅ |
| Built-in Functions | 8.2 | `03-rule-engine/` | ✅ |
| compilePassportPure | P1-T3, 7.6 | `04-compilation-pipeline/` | ✅ |
| Proof Builder | P1-T3, 9.1-9.2 | `04-compilation-pipeline/` | ✅ |
| Receipt Builder | P1-T3, 9.3 | `04-compilation-pipeline/` | ✅ |
| ED25519 Signing | P1-T3, 9.4 | `04-compilation-pipeline/` | ✅ |
| QR Payload | P1-T3, 7.8 | `04-compilation-pipeline/` | ✅ |
| Golden Tests | P1-T4, 13.4 | `04-compilation-pipeline/` | ✅ |
| DB Pool | P2-T1 | `05-data-layer/` | ✅ |
| Event Store | P2-T1, 7.4 | `05-data-layer/` | ✅ |
| Repositories | P2-T1 | `05-data-layer/` | ✅ |
| Job Queue | P2-T1 | `05-data-layer/`, `07-job-processing/` | ✅ |
| Auth Middleware | P2-T2 | `06-api-server/` | ✅ |
| API Handlers | P2-T2 | `06-api-server/` | ✅ |
| Pagination | P2-T2, 10 | `06-api-server/` | ✅ |
| Worker Loop | P2-T3 | `07-job-processing/` | ✅ |
| PARSE_FACTS | P2-T3 | `07-job-processing/` | ✅ |
| Snapshot Seal | P2-T3, 7.7 | `07-job-processing/` | ✅ |
| Observability | P2-T4, 11 | `06-api-server/` | ✅ |
| Passport Handlers | P3-T1 | `06-api-server/` | ✅ |
| COMPILE_PASSPORT | P3-T2 | `07-job-processing/` | ✅ |
| SIGN_PASSPORT | P3-T2 | `07-job-processing/` | ✅ |
| GENERATE_QR | P3-T2 | `07-job-processing/` | ✅ |
| Activate | P3-T3 | `06-api-server/` | ✅ |
| Replay | P3-T3, 9.6 | `06-api-server/` | ✅ |
| E2E Tests | P3-T4 | `06-api-server/`, `07-job-processing/` | ✅ |

### Advanced Features (P4)

| Feature | SSOT Reference | Spec | Status |
|---------|----------------|------|--------|
| Policy Engine | P4-T1 | `08-advanced-features/` | ✅ |
| Rate Limiting | P4-T2, 7.9 | `08-advanced-features/` | ✅ |
| Webhooks | P4-T3, 7.10, 10.6 | `08-advanced-features/` | ✅ |
| Idempotency Store | 7.11 | `08-advanced-features/` | ✅ |
| Data Retention | 9.5 | `08-advanced-features/` | ✅ |
| Access Denied Audit | Invariant 0 | `08-advanced-features/` | ✅ |

### Optional/MVP Subset

| Feature | SSOT Reference | Spec | Status |
|---------|----------------|------|--------|
| GraphQL (MVP subset) | 10.4 | `06-api-server/` | ✅ (marked MVP) |
| OpenAPI Generation | 10.5 | `06-api-server/contracts/` | ✅ |

## Gaps Analysis

### ✅ No Gaps Found

All SSOT sections and tasks (P0-T1 through P4-T3) are covered by the specs:

1. **Foundation (P0)**: Complete in `01-foundation/`
2. **Core Primitives (P1-T1)**: Complete in `02-core-primitives/`
3. **Rule Engine (P1-T2)**: Complete in `03-rule-engine/`
4. **Compilation (P1-T3, P1-T4)**: Complete in `04-compilation-pipeline/`
5. **Data Layer (P2-T1)**: Complete in `05-data-layer/`
6. **API Server (P2-T2, P2-T4)**: Complete in `06-api-server/`
7. **Job Processing (P2-T3)**: Complete in `07-job-processing/`
8. **Passport Pipeline (P3-T1 to P3-T4)**: Split across `06-api-server/` and `07-job-processing/`
9. **Advanced Features (P4-T1 to P4-T3)**: Complete in `08-advanced-features/`

### Contracts Coverage

| Contract Type | Location | Status |
|---------------|----------|--------|
| OpenAPI 3.0 | `06-api-server/contracts/openapi.yaml` | ✅ |
| Policy Endpoints | `08-advanced-features/contracts/policy-endpoints.yaml` | ✅ |
| Webhook Endpoints | `08-advanced-features/contracts/webhook-endpoints.yaml` | ✅ |
| Rate Limit Headers | `08-advanced-features/contracts/rate-limit-headers.yaml` | ✅ |

## Job Types Coverage

SSOT defines 9 job types in the `job_type` enum:

| Job Type | Spec Coverage | Status |
|----------|---------------|--------|
| INGEST_DOCUMENT | `07-job-processing/` (marked "future") | ⏳ Future |
| PARSE_FACTS | `07-job-processing/` | ✅ |
| BUILD_SNAPSHOT | `07-job-processing/` | ✅ |
| COMPILE_PASSPORT | `07-job-processing/` | ✅ |
| RUN_RULE_TESTS | `07-job-processing/` | ✅ |
| SIGN_PASSPORT | `07-job-processing/` | ✅ |
| GENERATE_QR | `07-job-processing/` | ✅ |
| EXPORT_PASSPORT | `07-job-processing/` (marked "future") | ⏳ Future |
| DELIVER_WEBHOOK | `08-advanced-features/` | ✅ |

**Note**: INGEST_DOCUMENT and EXPORT_PASSPORT are in the enum but marked as "future" - not part of current implementation scope.

## Package Coverage

| Package | Primary Spec | Additional Coverage |
|---------|--------------|---------------------|
| bpc-core | `02-core-primitives/`, `03-rule-engine/`, `04-compilation-pipeline/` | ✅ Complete |
| bpc-db | `05-data-layer/` | ✅ Complete |
| bpc-api | `06-api-server/` | ✅ Complete |
| bpc-worker | `07-job-processing/` | ✅ Complete |
| bpc-cli | `01-foundation/` (skeleton), `05-data-layer/` (verify), `08-advanced-features/` (retention) | ✅ Complete |

## Optional Features (SSOT marked optional)

| Feature | SSOT Status | Spec Status |
|---------|-------------|-------------|
| GraphQL endpoint | "Optional", "MVP subset" | ✅ Covered in `06-api-server/` |
| RabbitMQ queue | Optional (BPC_QUEUE_ENABLED=false default) | ✅ Documented, DB polling is primary |
| Coverage CI gate P4 | "optional" | ✅ Mentioned in specs |

## Future/Out of Scope

| Item | Status | Notes |
|------|--------|-------|
| INGEST_DOCUMENT job | ⏳ Stub documented | Enum exists, provisional payload in `07-job-processing/` |
| EXPORT_PASSPORT job | ⏳ Stub documented | Enum exists, provisional payload in `07-job-processing/` |
| v2 API | Future | Only mentioned as "Breaking change ⇒ /v2" |

**Note**: Future job handlers now have stub documentation in:
- `specs/07-job-processing/plan.md` → "Future Job Handlers" section
- `specs/07-job-processing/data-model.md` → "Future Job Types" section

These include provisional payloads and blocking issues to address before implementation.

## Conclusion

**All SSOT scope (MVP P0-P3 + Advanced P4) is captured in the specs folder.**

- 8 feature specs covering all 5 phases (P0-P4)
- 46 documentation files total
- 4 API contract files (OpenAPI + specialized)
- All 19 SSOT tasks mapped to implementation phases
- 7 of 9 job types fully specified (2 marked future)
- All 5 packages covered
- Optional features documented
