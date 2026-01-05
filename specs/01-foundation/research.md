# Research: Foundation & Infrastructure

**Feature**: 01-foundation
**Date**: 2025-12-28
**Status**: Complete

## Research Summary

All technical decisions for foundation are prescribed by SSOT. This document confirms alignment.

---

## 1. Build System: Cabal Multi-Package

**Decision**: Use Cabal 3.10.2.1 with multi-package project

**Rationale**:
- SSOT 4.2 mandates Cabal multi-package structure
- 5 packages with clear dependency boundaries
- GHC 9.6.4 provides modern Haskell features (GADTs, DataKinds needed for Rule DSL)

**Alternatives Considered**:
- Stack: Rejected (SSOT specifies Cabal)
- Nix: Could be added later for reproducibility, but not required initially

**SSOT Reference**: Section 4.2

---

## 2. Database: PostgreSQL 16

**Decision**: PostgreSQL 16 with dbmate migrations

**Rationale**:
- SSOT 4.5.1 specifies Postgres 16
- dbmate is simple, language-agnostic migration tool
- DDL fully specified in SSOT 6.2-6.5

**Alternatives Considered**:
- Hasql migrations: Rejected (dbmate is simpler, specified)
- Flyway: Overkill for this project

**SSOT Reference**: Section 6.1

---

## 3. Formatting & Linting

**Decision**: Fourmolu + HLint with exact configs from SSOT

**Rationale**:
- SSOT 4.3.2 provides fourmolu.yaml content
- SSOT 4.3.3 provides hlint.yaml content
- Must be enforced in CI

**Configuration** (from SSOT):

```yaml
# fourmolu.yaml
indentation: 2
comma-style: leading
record-brace-space: false
respectful: true
```

```yaml
# hlint.yaml
- ignore: {name: "Use fromJust"}
- ignore: {name: "Use camelCase"}
- warn: {name: "Use newtype instead of data", within: ["BPC.Core.Types"]}
```

**SSOT Reference**: Section 4.3

---

## 4. CI/CD: GitHub Actions

**Decision**: GitHub Actions with Postgres service container

**Rationale**:
- SSOT 4.4.1 provides complete ci.yml
- SSOT 4.4.2 provides complete cd.yml
- Uses haskell/actions/setup for GHC/Cabal

**Key CI Steps**:
1. Setup GHC 9.6.4, Cabal 3.10.2.1
2. Cache ~/.cabal/store and dist-newstyle
3. Run fourmolu check
4. Run hlint
5. Build all packages
6. Run unit tests
7. Start Postgres service (port 55432)
8. Run migrations
9. Run integration tests

**SSOT Reference**: Section 4.4

---

## 5. Docker Images

**Decision**: Multi-stage builds with debian:bookworm-slim runtime

**Rationale**:
- SSOT 4.5.3/4.5.4 provides Dockerfiles
- Build stage uses haskell:9.6.4
- Runtime only needs libpq5 and ca-certificates

**Image Targets**:
- `ghcr.io/{owner}/bpc-api:v*`
- `ghcr.io/{owner}/bpc-worker:v*`

**SSOT Reference**: Section 4.5

---

## 6. Environment Variables

**Decision**: All configuration via ENV per SSOT 4.6

**Key Variables**:
| Name | Default | Required in Prod |
|------|---------|------------------|
| BPC_DB_HOST | localhost | Yes |
| BPC_DB_PORT | 5432 | Yes |
| BPC_DB_USER | postgres | Yes |
| BPC_DB_PASSWORD | postgres | Yes |
| BPC_DB_NAME | bpc | Yes |
| BPC_DB_POOL_SIZE | 10 | No |
| BPC_SIGNING_ED25519_PRIVATE_KEY_BASE64 | (none) | Yes |
| BPC_API_KEY_PEPPER_BASE64 | (none) | Yes |

**SSOT Reference**: Section 4.6

---

## 7. Migration Strategy

**Decision**: dbmate with SQL files in migrations/

**Rationale**:
- Simple, proven tool
- SQL files are version-controlled
- Idempotent (safe to run multiple times)

**Migration Order**:
1. 001_init.sql - Core schema (tenants, actors, documents, facts, snapshots, passports, jobs)
2. 002_seed_permissions_roles.sql - Permissions and dev tenant
3. 003_add_policies_webhooks.sql - Policy engine, webhooks
4. 004_add_rate_limits.sql - Rate limiting tables

**SSOT Reference**: Section 6.2-6.5

---

## Open Questions

None - all decisions prescribed by SSOT.

## Next Steps

Proceed to implementation following plan.md phases.
