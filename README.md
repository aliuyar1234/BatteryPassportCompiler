# Battery Passport Compiler (BPC)

A deterministic compiler for Battery Passports with cryptographic proof objects, ED25519 signatures, QR codes, and audit replay capability.

## Prerequisites

- **GHC 9.6.4** - Install via [ghcup](https://www.haskell.org/ghcup/)
- **Cabal 3.10.2.1** - Install via ghcup
- **Docker + Docker Compose** - [Docker Desktop](https://www.docker.com/products/docker-desktop/) or Docker Engine v2.x+
- **dbmate** - Database migration tool
  - macOS: `brew install dbmate`
  - Linux: Download from [releases](https://github.com/amacneil/dbmate/releases)
  - Windows: Download from releases or use WSL

## Quick Start

```bash
# 1. Clone the repository
git clone <repo-url>
cd BatteryPassportCompiler

# 2. Start database and message queue
docker compose up -d

# 3. Wait for services to be healthy
docker compose exec postgres pg_isready -U bpc -d bpc

# 4. Run database migrations
./scripts/migrate.sh

# 5. Seed development data (optional)
./scripts/seed-dev.sh

# 6. Build all packages
cabal build all

# 7. Run tests
cabal test all
```

## Project Structure

```
.
├── packages/
│   ├── bpc-core/     # Pure domain logic (IO-free)
│   ├── bpc-db/       # Database layer (repositories, event store)
│   ├── bpc-api/      # HTTP API server
│   ├── bpc-worker/   # Job processing worker
│   └── bpc-cli/      # Admin CLI
├── migrations/       # Database migrations (dbmate)
├── scripts/          # Development scripts
└── docker/           # Docker build files
```

## Development Commands

```bash
# Start services
./scripts/dev-up.sh     # or: docker compose up -d

# Stop services
./scripts/dev-down.sh   # or: docker compose down

# Run migrations
./scripts/migrate.sh

# Build
cabal build all

# Run specific service
cabal run bpc-api:exe:bpc-api
cabal run bpc-worker:exe:bpc-worker
cabal run bpc-cli:exe:bpc-cli

# Run tests
cabal test all                      # All tests
cabal test bpc-core:unit           # Unit tests for bpc-core
cabal test bpc-db:integration      # Integration tests for bpc-db

# Format code
fourmolu -m check $(git ls-files '*.hs')
fourmolu -m inplace $(git ls-files '*.hs')  # Auto-fix

# Lint code
hlint -h hlint.yaml $(git ls-files '*.hs')
```

## Architecture

### Package Dependencies

```
bpc-core  (pure, IO-free)
    ↑
bpc-db    (database layer)
    ↑
bpc-api / bpc-worker / bpc-cli
```

- `bpc-core` is IO-free; cannot import DB/HTTP/Queue
- `bpc-db` can import `bpc-core`; cannot import api/worker
- `bpc-api`/`bpc-worker`/`bpc-cli` can import `bpc-core` + `bpc-db`
- `bpc-api` and `bpc-worker` cannot import each other

### Core Pipeline

```
Upload DocumentVersion → Parse Facts → Build+Seal Snapshot
    → Compile (pure) → Sign → QR → Activate → Replay
```

## Critical Invariants

1. **Determinism**: Same inputs produce byte-identical artifacts
2. **Canonical Storage**: Storage is always canonical bytes; JSON pretty-print is never the source of truth
3. **Immutability**: DocumentVersion, Facts, SEALED Snapshots, PUBLISHED Rules, PassportVersion are immutable
4. **Audit Trail**: Every write and denied access generates an Audit Event

## Configuration

Copy `.env.example` to `.env` and adjust values:

```bash
cp .env.example .env
```

Key environment variables:
- `BPC_DB_HOST`, `BPC_DB_PORT`, `BPC_DB_NAME` - Database connection
- `BPC_SIGNING_ED25519_PRIVATE_KEY_BASE64` - **Required in prod** for passport signing
- `BPC_API_KEY_PEPPER_BASE64` - **Required in prod** for API key hashing

See `.env.example` for all available options.

## Database

### Running Migrations

```bash
# Development database
./scripts/migrate.sh

# Custom database
DATABASE_URL=postgres://user:pass@host:5432/db?sslmode=disable dbmate up

# Test database
BPC_DB_PORT=55432 BPC_DB_NAME=bpc_test ./scripts/migrate.sh
```

### Checking Database

```bash
# List tables
docker compose exec postgres psql -U bpc -d bpc -c "\dt"

# Check permissions
docker compose exec postgres psql -U bpc -d bpc -c "SELECT * FROM permissions"

# Check tenants
docker compose exec postgres psql -U bpc -d bpc -c "SELECT * FROM tenants"
```

## License

BSD-3-Clause
