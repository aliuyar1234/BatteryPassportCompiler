# Quickstart: Foundation & Infrastructure

**Feature**: 01-foundation
**Date**: 2025-12-28

## Prerequisites

Install these tools before starting:

```bash
# GHC and Cabal (use ghcup)
ghcup install ghc 9.6.4
ghcup install cabal 3.10.2.1
ghcup set ghc 9.6.4
ghcup set cabal 3.10.2.1

# Verify versions
ghc --version    # The Glorious Glasgow Haskell Compilation System, version 9.6.4
cabal --version  # cabal-install version 3.10.2.1

# Docker
docker --version  # Docker version 24.x+
docker compose version  # Docker Compose version v2.x+

# dbmate (migration tool)
# macOS: brew install dbmate
# Linux: curl -fsSL -o /usr/local/bin/dbmate https://github.com/amacneil/dbmate/releases/latest/download/dbmate-linux-amd64 && chmod +x /usr/local/bin/dbmate

# Formatting/Linting
cabal install fourmolu hlint
```

## Quick Setup (5 minutes)

```bash
# 1. Clone and enter repository
git clone <repository-url>
cd BatteryPassportCompiler

# 2. Start database and queue
docker compose up -d postgres rabbitmq

# 3. Wait for Postgres to be ready
docker compose exec postgres pg_isready -U bpc -d bpc

# 4. Run migrations
./scripts/migrate.sh

# 5. Seed development data
./scripts/seed-dev.sh

# 6. Build all packages
cabal build all

# 7. Run tests
cabal test all
```

## Verification

After setup, verify everything works:

```bash
# Check Docker containers
docker compose ps
# Expected: postgres and rabbitmq running

# Check database tables
docker compose exec postgres psql -U bpc -d bpc -c "\dt"
# Expected: 20+ tables listed

# Check dev tenant exists
docker compose exec postgres psql -U bpc -d bpc -c "SELECT slug, name FROM tenants"
# Expected: dev | Dev Tenant

# Check roles
docker compose exec postgres psql -U bpc -d bpc -c "SELECT r.name FROM roles r JOIN tenants t USING(tenant_id) WHERE t.slug='dev'"
# Expected: Admin, ComplianceOfficer, Auditor

# Check code formatting
fourmolu -m check $(git ls-files '*.hs')

# Check linting
hlint -h hlint.yaml $(git ls-files '*.hs')
```

## Running Services

```bash
# Run API server (port 8080)
cabal run bpc-api:exe:bpc-api

# Run worker (in separate terminal)
cabal run bpc-worker:exe:bpc-worker
```

## Environment Variables

Copy `.env.example` to `.env` for local development:

```bash
cp .env.example .env
```

Key variables for local development:

```bash
BPC_ENV=dev
BPC_HTTP_HOST=0.0.0.0
BPC_HTTP_PORT=8080
BPC_DB_HOST=localhost
BPC_DB_PORT=5432
BPC_DB_USER=bpc
BPC_DB_PASSWORD=bpc
BPC_DB_NAME=bpc
BPC_DB_POOL_SIZE=10
BPC_QUEUE_ENABLED=false
BPC_LOG_LEVEL=debug
```

## Common Tasks

### Rebuild after code changes

```bash
cabal build all
```

### Run specific package tests

```bash
cabal test bpc-core
cabal test bpc-db
cabal test bpc-api
```

### Run integration tests (requires running DB)

```bash
cabal test bpc-db:integration
cabal test bpc-api:integration
cabal test bpc-worker:integration
```

### Reset database

```bash
docker compose down -v
docker compose up -d postgres
./scripts/migrate.sh
./scripts/seed-dev.sh
```

### Format code

```bash
fourmolu -i $(git ls-files '*.hs')
```

### Check for issues before committing

```bash
fourmolu -m check $(git ls-files '*.hs') && \
hlint -h hlint.yaml $(git ls-files '*.hs') && \
cabal build all && \
cabal test all
```

## Troubleshooting

### Postgres connection refused

```bash
# Check if container is running
docker compose ps

# Check container logs
docker compose logs postgres

# Restart containers
docker compose restart postgres
```

### Cabal build fails with dependency errors

```bash
cabal update
cabal clean
cabal build all
```

### Migration fails

```bash
# Check DATABASE_URL
echo $DATABASE_URL

# Run with explicit URL
DATABASE_URL="postgres://bpc:bpc@localhost:5432/bpc?sslmode=disable" dbmate up

# Check migration status
DATABASE_URL="postgres://bpc:bpc@localhost:5432/bpc?sslmode=disable" dbmate status
```

### Fourmolu/HLint not found

```bash
# Ensure ~/.cabal/bin is in PATH
export PATH="$HOME/.cabal/bin:$PATH"

# Or install globally
cabal install fourmolu hlint --install-method=copy --overwrite-policy=always
```

## Next Steps

Once foundation is complete:

1. **02-core-primitives**: Implement canonical JSON, hashing, domain types
2. **03-rule-engine**: Implement DSL parser and typechecker
3. **04-compilation-pipeline**: Implement pure compiler
4. **05-data-layer**: Implement repositories
5. **06-api-server**: Implement HTTP handlers
6. **07-job-processing**: Implement worker handlers
8. **08-advanced-features**: Policies, webhooks, rate limiting
