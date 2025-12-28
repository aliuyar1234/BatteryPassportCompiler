# SSOT – BatteryPassportCompiler (BPC)
**Version:** v0.2 (normativ)
**Datum:** 2025-12-28
## 0. Nicht verhandelbare Invarianten
- Determinismus: gleiche Inputs ⇒ byte-identische Artefakte (payload/proof/receipt).
- Storage ist immer canonical bytes; JSON pretty-print ist nie Source of Truth.
- Immutable: DocumentVersion, Facts, SEALED Snapshot, PUBLISHED Rules, PassportVersion.
- Jeder Write + jeder denied access ⇒ Audit Event (Event Store).
## Inhaltsverzeichnis
- 1. Produkt
- 2. Glossar & Error Codes
- 3. Contexts & Module
- 4. Setup & Tooling
- 5. Haskell Architektur
- 6. Datenmodell (SQL DDL + Seeds + Migrationen)
- 7. Algorithmen
- 8. Rule-DSL
- 9. Proof & Receipt & Replay
- 10. API (Patterns + OpenAPI)
- 11. Observability
- 12. Deployment/Ops
- 13. Testing
- 14. Tasks
## 1. Produkt
- **Einzeiler:** Deterministischer Compiler für Battery Passports inkl. Proof Objects, Signatur, QR, Audit-Replay.
- **Kernpipeline:** Upload DocumentVersion → Parse Facts → Build+Seal Snapshot → Compile (pure) → Sign → QR → Activate → Replay.
- **QR Pflicht:** ab 2027-02-18 (als versionierte Policy/Rule, nicht hardcodiert).
## 2. Glossar & Error Codes
### 2.1 Abkürzungen
- **API:** Application Programming Interface.
- **AST:** Abstract Syntax Tree.
- **BOM:** Bill of Materials.
- **BPC:** BatteryPassportCompiler.
- **CI/CD:** Continuous Integration / Continuous Deployment.
- **DDL:** Data Definition Language (SQL Schema).
- **DD:** Due Diligence.
- **DSL:** Domain Specific Language (Regelsprache).
- **ED25519:** Edwards-curve Digital Signature Algorithm.
- **FK:** Foreign Key.
- **GADT:** Generalized Algebraic Data Type.
- **HMAC:** Hash-based Message Authentication Code.
- **JSON:** JavaScript Object Notation.
- **PCF:** Product Carbon Footprint.
- **QR:** Quick Response code.
- **RBAC:** Role Based Access Control.
- **RLS:** Row Level Security (Postgres).
- **SHA-256:** Secure Hash Algorithm 256-bit.
- **SSOT:** Single Source of Truth.
- **UUID:** Universally Unique Identifier.
### 2.2 Status-Werte (vollständig)
- `job_status`: QUEUED | RUNNING | SUCCEEDED | FAILED | CANCELLED | DEAD_LETTER
- `document_status`: UPLOADED | PARSED | VALIDATED | REJECTED
- `snapshot_status`: BUILDING | READY | SEALED
- `rule_pkg_status`: DRAFT | VALIDATED | PUBLISHED | DEPRECATED | RETIRED
- `passport_status`: COMPILING | SIGNED | ACTIVE | SUPERSEDED | REVOKED
### 2.3 Fachbegriffe (präzise)
- **Tenant:** Mandant; isolierter Daten-/Namensraum. Alle Daten sind tenant-scoped.
- **Actor:** Identität, die Aktionen ausführt (USER/API_CLIENT/SERVICE).
- **API Key:** Secret für API Auth; server speichert nur SHA-256(key+pepper).
- **Permission:** String, der eine Aktion autorisiert (z.B. passport:read).
- **Role:** Tenant-scoped Bündel von Permissions.
- **Policy:** Versionierte Access-Regel, die RBAC ergänzt; first-match entscheidet.
- **Document:** Container für Versionen eines Uploads (BOM/PCF/DD).
- **DocumentVersion:** Immutable Upload-Version: content bytes + sha256 + status.
- **Fact:** Strukturiertes Datum abgeleitet aus DocumentVersion: payload jsonb + canonical bytes + payload_hash.
- **DataSnapshot:** Menge von Facts; nach Seal immutable; hat snapshot_hash.
- **RulePackage:** Container für RulePackageVersions.
- **RulePackageVersion:** Immutable Version (dsl_source + tests_source + status).
- **Passport:** Container für PassportVersions pro BatteryProduct.
- **PassportVersion:** Immutable Ergebnis einer Compilation: payload/proof/receipt/signature/qr.
- **Receipt:** Maschinenprüfbarer Beleg: Input hashes + Output hashes + Signatur.
- **Proof Object:** Derivation Tree mit Node Hashes; verifiziert Rechenschritte.
- **Event Store:** Append-only Log aller Events; Hash-Chain macht Manipulation nachweisbar.
- **Aggregate:** Event-Sourcing Einheit; identifiziert durch (aggregate_type, aggregate_id).
- **Idempotency Key:** Client-supplied String; server speichert request_hash und response.
### 2.4 Error Codes (normativ)
| Code | HTTP | Retryable | Beschreibung |
|---|---:|:---:|---|
| CONFIG_MISSING | 500 | N | Service startete ohne notwendige ENV; Prozess beendet. |
| UNAUTHORIZED | 401 | N | Kein/ungültiger Authorization Header. |
| API_KEY_REVOKED | 401 | N | API Key wurde widerrufen. |
| FORBIDDEN | 403 | N | Permission/Policy denied. |
| NOT_FOUND | 404 | N | Ressource existiert nicht im Tenant Scope. |
| VALIDATION_ERROR | 422 | N | Request Schema/Constraint verletzt. |
| IDEMPOTENCY_CONFLICT | 409 | N | Gleicher Idempotency-Key mit anderem Request-Hash. |
| EVENT_VERSION_CONFLICT | 409 | Y | Optimistic concurrency failure im Event Store. |
| SNAPSHOT_SEALED | 409 | N | Snapshot ist SEALED und kann nicht verändert werden. |
| SNAPSHOT_NOT_READY | 409 | N | Snapshot muss READY sein, bevor seal() möglich ist. |
| RULE_PKG_NOT_PUBLISHED | 409 | N | RulePackageVersion ist nicht PUBLISHED/zulässig für Compile. |
| RULE_TESTS_FAILED | 409 | N | Publish blocked: Tests fehlen/failed/cases<500. |
| RULE_PARSE_ERROR | 422 | N | DSL Source kann nicht geparst werden. |
| RULE_TYPE_ERROR | 422 | N | Typfehler in RulePackageVersion (unit mismatch etc.). |
| RULE_CYCLE_DETECTED | 422 | N | Zyklus in Feldabhängigkeiten entdeckt. |
| DIVISION_BY_ZERO | 422 | N | Division durch 0 in Rule Eval. |
| UNIT_MISMATCH | 422 | N | Unit-Konflikt oder unsupported conversion. |
| PROOF_TOO_LARGE | 422 | N | Proof überschreitet MaxSize; Compilation abgebrochen. |
| PAYLOAD_TOO_LARGE | 422 | N | Payload überschreitet MaxSize; Compilation abgebrochen. |
| RECEIPT_TOO_LARGE | 422 | N | Receipt überschreitet MaxSize; Compilation abgebrochen. |
| SIGNING_KEY_MISSING | 500 | N | Signing Key nicht konfiguriert. |
| SIGNATURE_INVALID | 422 | N | Receipt/Signature verifiziert nicht. |
| REPLAY_MISMATCH | 409 | N | Replay ergab andere Hashes als gespeicherte Version. |
| RATE_LIMITED | 429 | Y | Rate limit überschritten. |
| QUEUE_UNAVAILABLE | 503 | Y | Message Queue nicht erreichbar. |
| DB_UNAVAILABLE | 503 | Y | Postgres nicht erreichbar. |
| INTERNAL_ERROR | 500 | Y | Unklassifizierter Fehler; correlation_id beachten. |
## 3. Contexts & Module
### 3.1 Bounded Contexts
- **Auth:** AuthN via API Keys; RBAC; Policy Engine; stellt AuthContext bereit.
- **Audit:** Event Store append/verify; Audit Events; Replay evidence.
- **Documents:** Upload + Versionierung von Dokumenten; Content Storage; Status machine.
- **Facts:** Parsing/Validation; Fact schemas; canonicalization + hashing.
- **Snapshots:** Build/Seal snapshots; snapshot hashing; immutability enforcement.
- **Rules:** DSL parsing/typecheck; RulePackageVersion lifecycle; rule tests runner.
- **Compiler:** Pure evaluator + dependency graph; proof emission; passport builder.
- **Passports:** Version management; activation/superseding; revoke; QR; exports.
- **Jobs:** Async engine; leasing; backoff; idempotency for job handlers.
- **API:** REST+GraphQL surface; error format; pagination; rate limiting.
- **Observability:** Logging/metrics/health; correlation IDs.
- **Ops:** Migrations, seeds, backup/retention jobs, deploy tooling.
### 3.2 Packages (Cabal)
| Package | Pfad | Verantwortung |
|---|---|---|
| bpc-core | `packages/bpc-core` | Pure Domain/DSL/Evaluator/Canonicalization/Proof/Receipt builder (pure). |
| bpc-db | `packages/bpc-db` | DB Pool + Repos + Event append/verify + migration helpers. |
| bpc-api | `packages/bpc-api` | HTTP server + handlers + auth/rate-limit middleware + OpenAPI/GraphQL. |
| bpc-worker | `packages/bpc-worker` | Worker loop + job dispatch + handlers (parse/build/compile/sign/qr/webhook). |
| bpc-cli | `packages/bpc-cli` | Admin/ops CLI (migrate/seed/verify/export verify). |
### 3.3 Package Dependency Graph (Mermaid)
```mermaid
flowchart LR
  core[bpc-core] --> db[bpc-db]
  core --> api[bpc-api]
  core --> worker[bpc-worker]
  core --> cli[bpc-cli]
  db --> api
  db --> worker
  db --> cli
```

### 3.3.1 Modul-Abhängigkeiten (Mermaid, MUST)
```mermaid
flowchart TB

#### 3.4.1 Modul Import Matrix (MUST)
| Importer Prefix | Darf importieren | Darf NICHT importieren |
|---|---|---|
| `BPC.Core.*` | `BPC.Core.*` | `BPC.DB.*`, `BPC.API.*`, `BPC.Worker.*` |
| `BPC.DB.*` | `BPC.Core.*`, `BPC.DB.*` | `BPC.API.*`, `BPC.Worker.*` |
| `BPC.API.*` | `BPC.Core.*`, `BPC.DB.*` | `BPC.Worker.*` |
| `BPC.Worker.*` | `BPC.Core.*`, `BPC.DB.*` | `BPC.API.*` |
| `BPC.CLI.*` | `BPC.Core.*`, `BPC.DB.*` | `BPC.API.*`, `BPC.Worker.*` |

  subgraph core[bpc-core]
    CJ[BPC.Core.CanonicalJson]-->H[BPC.Core.Hash]
    RP[BPC.Core.Rules.Parser]-->RA[BPC.Core.Rules.AST]
    RT[BPC.Core.Rules.Typecheck]-->RA
    EV[BPC.Core.Eval]-->RT
    PR[BPC.Core.Proof]-->H
    RC[BPC.Core.Receipt]-->H
    QR[BPC.Core.QR]-->H
  end
  subgraph db[bpc-db]
    POOL[BPC.DB.Pool]-->RE[BPC.DB.Repos.Events]
    POOL-->RD[BPC.DB.Repos.Documents]
    POOL-->RF[BPC.DB.Repos.Facts]
    POOL-->RS[BPC.DB.Repos.Snapshots]
    POOL-->RJ[BPC.DB.Repos.Jobs]
    POOL-->RP2[BPC.DB.Repos.Passports]
  end
  subgraph api[bpc-api]
    AUTH[BPC.API.Middleware.Auth]-->POOL
    HAND[BPC.API.Handlers.*]-->POOL
    HAND-->CJ
  end
  subgraph worker[bpc-worker]
    RUN[BPC.Worker.Runner]-->RJ
    WH[BPC.Worker.Handlers.*]-->RP2
    WH-->RS
    WH-->RF
    WH-->EV
    WH-->RC
    WH-->QR
  end
```

### 3.4 Dependency Rules (MUST)
- core ist IO-frei; darf kein DB/HTTP/Queue importieren.
- db darf core importieren; darf nicht api/worker importieren.
- api/worker/cli dürfen core+db importieren.
### 3.5 Public Interfaces (Exports, vollständig)
#### 3.5.1 bpc-core Exports
- `BPC.Core.Types` — Domain IDs, Money/Dec/Qty wrappers, status enums (Haskell-side).
- `BPC.Core.CanonicalJson` — canonicalEncode/canonicalDecode; float rejection.
- `BPC.Core.Hash` — sha256, base32NoPad, nodeHash helpers.
- `BPC.Core.Facts.Schema` — Fact type definitions + schema versions + validators.
- `BPC.Core.Rules.AST` — Typed AST (GADTs) + Ty/Unit definitions.
- `BPC.Core.Rules.Parser` — DSL parser text->untyped AST.
- `BPC.Core.Rules.Typecheck` — untyped AST -> typed Expr; rule dependency extraction.
- `BPC.Core.Rules.Tests` — example/property test runner (pure).
- `BPC.Core.Eval` — evalExpr + compilePassportPure (pure).
- `BPC.Core.Proof` — Proof types + verifyProof (pure).
- `BPC.Core.Passport.Schema` — Payload builder BPC-PASSPORT-1 (Value + canonical).
- `BPC.Core.Receipt` — Receipt builder BPC-RECEIPT-1 (Value + canonical + hash).
- `BPC.Core.QR` — QR payload string builder (pure).
#### 3.5.2 bpc-db Exports
- `BPC.DB.Pool` — mkPool, withConn.
- `BPC.DB.Migrations` — dbmate wrapper + schema check.
- `BPC.DB.Repos.Events` — appendEvent, verifyChain.
- `BPC.DB.Repos.Auth` — Actors/ApiKeys/Roles/Permissions/Policies.
- `BPC.DB.Repos.Documents` — Documents + DocumentVersions.
- `BPC.DB.Repos.Facts` — Facts insert/query.
- `BPC.DB.Repos.Snapshots` — Snapshots + snapshot_items.
- `BPC.DB.Repos.Rules` — RulePackages + Versions + rule_fields + rule_tests_runs.
- `BPC.DB.Repos.Passports` — Passports + PassportVersions.
- `BPC.DB.Repos.Jobs` — enqueue, acquireLease, complete/fail, backoff update.
- `BPC.DB.Repos.Webhooks` — webhook endpoints/subscriptions/deliveries.
#### 3.5.3 bpc-api Exports
- `BPC.API.Main` — WAI/Warp server start.
- `BPC.API.Routes` — Route table.
- `BPC.API.Middleware.Auth` — AuthN/AuthZ + policy evaluation.
- `BPC.API.Middleware.RateLimit` — token bucket limiter.
- `BPC.API.Middleware.CorrelationId` — X-Correlation-Id handling.
- `BPC.API.Handlers.*` — Thin handlers; no business logic beyond orchestration.
- `BPC.API.GraphQL` — MVP GraphQL server.
- `BPC.API.OpenAPI` — Serves /docs/openapi.yaml or embedded spec.
#### 3.5.4 bpc-worker Exports
- `BPC.Worker.Main` — Start worker process.
- `BPC.Worker.Runner` — Job loop + leasing + metrics.
- `BPC.Worker.Dispatch` — Job type to handler mapping.
- `BPC.Worker.Handlers.*` — Concrete job handlers (ParseFacts, BuildSnapshot, CompilePassport, SignPassport, GenerateQR, RunRuleTests, DeliverWebhook).
## 4. Setup & Tooling
### 4.1 Repo Struktur (exakt)
```text
.
├── SSOT.md
├── README.md
├── cabal.project
├── cabal.project.freeze
├── fourmolu.yaml
├── hlint.yaml
├── .editorconfig
├── .gitignore
├── .env.example
├── .github/workflows
│   ├── ci.yml
│   └── cd.yml
├── docker
│   ├── Dockerfile.api
│   ├── Dockerfile.worker
│   ├── Dockerfile.cli
│   └── entrypoint.sh
├── docker-compose.yml
├── docker-compose.test.yml
├── migrations
│   ├── 001_init.sql
│   ├── 002_seed_permissions_roles.sql
│   ├── 003_add_policies_webhooks.sql
│   └── 004_add_rate_limits.sql
├── scripts
│   ├── dev-up.sh
│   ├── dev-down.sh
│   ├── migrate.sh
│   ├── seed-dev.sh
│   ├── run-integration-tests.sh
│   └── gen-openapi.sh
├── docs
│   └── openapi.yaml
└── packages
    ├── bpc-core
    ├── bpc-db
    ├── bpc-api
    ├── bpc-worker
    └── bpc-cli
```
### 4.2 Cabal (MUST)
#### 4.2.1 cabal.project (root)
```text
packages:
  packages/bpc-core
  packages/bpc-db
  packages/bpc-api
  packages/bpc-worker
  packages/bpc-cli

with-compiler: ghc-9.6.4

package *:
  ghc-options: -Wall -Wcompat -Werror
  optimization: 2
```
#### 4.2.2 Beispiel: packages/bpc-api/bpc-api.cabal (relevant Teile)
```cabal
cabal-version:      3.0
name:               bpc-api
version:            0.1.0.0
build-type:         Simple

executable bpc-api
  main-is:          Main.hs
  hs-source-dirs:   src
  default-language: Haskell2010
  build-depends:
      base
    , bpc-core
    , bpc-db
    , aeson
    , warp
    , wai
    , wai-extra
    , postgresql-simple
```
### 4.3 Formatting & Linting (MUST, Dateien komplett)
#### 4.3.1 .editorconfig
```ini
root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.hs]
indent_style = space
indent_size = 2
```
#### 4.3.2 fourmolu.yaml
```yaml
indentation: 2
comma-style: leading
record-brace-space: false
respectful: true
```
#### 4.3.3 hlint.yaml
```yaml
- ignore: {name: "Use fromJust"}
- ignore: {name: "Use camelCase"}
- warn: {name: "Use newtype instead of data", within: ["BPC.Core.Types"]}
```
### 4.4 CI/CD (MUST, Dateien komplett)
#### 4.4.1 .github/workflows/ci.yml
```yaml
name: ci
on:
  push: {branches: [main]}
  pull_request: {}
jobs:
  build:
    runs-on: ubuntu-latest
    services:
      postgres:
        image: postgres:16
        env:
          POSTGRES_PASSWORD: bpc
          POSTGRES_USER: bpc
          POSTGRES_DB: bpc
        ports: ["55432:5432"]
        options: >-
          --health-cmd="pg_isready -U bpc -d bpc"
          --health-interval=5s
          --health-timeout=5s
          --health-retries=20
    steps:
      - uses: actions/checkout@v4
      - uses: haskell/actions/setup@v2
        with:
          ghc-version: "9.6.4"
          cabal-version: "3.10.2.1"
      - uses: actions/cache@v4
        with:
          path: |
            ~/.cabal/store
            dist-newstyle
          key: ${{ runner.os }}-cabal-${{ hashFiles('**/*.cabal','cabal.project') }}
      - run: fourmolu -m check $(git ls-files '*.hs')
      - run: hlint -h hlint.yaml $(git ls-files '*.hs')
      - run: cabal build all
      - run: cabal test all
      - env:
          BPC_DB_HOST: localhost
          BPC_DB_PORT: "55432"
          BPC_DB_USER: bpc
          BPC_DB_PASSWORD: bpc
          BPC_DB_NAME: bpc
        run: |
          ./scripts/migrate.sh
          cabal test bpc-db:integration
          cabal test bpc-api:integration
          cabal test bpc-worker:integration
```
#### 4.4.2 .github/workflows/cd.yml
```yaml
name: cd
on:
  push:
    tags: ["v*"]
permissions:
  contents: read
  packages: write
jobs:
  docker:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3
      - name: Login to GHCR
        uses: docker/login-action@v3
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}
      - name: Build and push api
        uses: docker/build-push-action@v6
        with:
          context: .
          file: docker/Dockerfile.api
          push: true
          tags: ghcr.io/${{ github.repository_owner }}/bpc-api:${{ github.ref_name }}
      - name: Build and push worker
        uses: docker/build-push-action@v6
        with:
          context: .
          file: docker/Dockerfile.worker
          push: true
          tags: ghcr.io/${{ github.repository_owner }}/bpc-worker:${{ github.ref_name }}
```
### 4.5 Docker/Container Setup (MUST, Dateien komplett)
#### 4.5.1 docker-compose.yml (dev)
```yaml
services:
  postgres:
    image: postgres:16
    environment:
      POSTGRES_PASSWORD: bpc
      POSTGRES_USER: bpc
      POSTGRES_DB: bpc
    ports: ["5432:5432"]
  rabbitmq:
    image: rabbitmq:3-management
    ports: ["5672:5672","15672:15672"]
```
#### 4.5.2 docker-compose.test.yml (integration tests)
```yaml
services:
  postgres:
    image: postgres:16
    environment:
      POSTGRES_PASSWORD: bpc
      POSTGRES_USER: bpc
      POSTGRES_DB: bpc_test
    ports: ["55432:5432"]
```
#### 4.5.3 docker/Dockerfile.api
```dockerfile
FROM haskell:9.6.4 as build
WORKDIR /src
COPY . .
RUN cabal update && cabal build bpc-api:exe:bpc-api

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y libpq5 ca-certificates && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY --from=build /src/dist-newstyle/ /app/dist-newstyle/
CMD ["/app/dist-newstyle/build/x86_64-linux/ghc-9.6.4/bpc-api-0.1.0.0/x/bpc-api/build/bpc-api/bpc-api"]
```
#### 4.5.4 docker/Dockerfile.worker
```dockerfile
FROM haskell:9.6.4 as build
WORKDIR /src
COPY . .
RUN cabal update && cabal build bpc-worker:exe:bpc-worker

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y libpq5 ca-certificates && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY --from=build /src/dist-newstyle/ /app/dist-newstyle/
CMD ["/app/dist-newstyle/build/x86_64-linux/ghc-9.6.4/bpc-worker-0.1.0.0/x/bpc-worker/build/bpc-worker/bpc-worker"]
```
### 4.6 ENV Schema (vollständig)
| Name | Typ | Default | Beschreibung |
|---|---|---|---|
| BPC_ENV | Text | dev | dev|test|prod |
| BPC_HTTP_HOST | Text | 0.0.0.0 | API bind host |
| BPC_HTTP_PORT | Int | 8080 | API bind port |
| BPC_COMPILER_BUILD_ID | Text | UNKNOWN | Build/Commit ID; in CI setzen |
| BPC_WORKER_ID | Text | worker-1 | Worker lease owner id |
| BPC_DB_HOST | Text | localhost | Postgres host |
| BPC_DB_PORT | Int | 5432 | Postgres port |
| BPC_DB_USER | Text | postgres | Postgres user |
| BPC_DB_PASSWORD | Text | postgres | Postgres password |
| BPC_DB_NAME | Text | bpc | DB name |
| BPC_DB_POOL_SIZE | Int | 10 | Connection pool size |
| BPC_DB_SSLMODE | Text | disable | disable|require |
| BPC_QUEUE_ENABLED | Bool | false | If true: use RabbitMQ trigger |
| BPC_QUEUE_URL | Text | amqp://guest:guest@localhost:5672/ | RabbitMQ URL |
| BPC_QUEUE_EXCHANGE | Text | bpc | AMQP exchange |
| BPC_QUEUE_ROUTING_KEY | Text | jobs.trigger | AMQP routing key |
| BPC_JOBS_POLL_INTERVAL_MS | Int | 1000 | DB polling interval if queue disabled |
| BPC_JOBS_LEASE_SECONDS | Int | 60 | Lease TTL seconds |
| BPC_JOBS_LEASE_RENEW_SECONDS | Int | 30 | Renew interval seconds |
| BPC_JOBS_MAX_ATTEMPTS_DEFAULT | Int | 10 | Default max attempts |
| BPC_PROOF_MAX_BYTES | Int | 20000000 | Max proof canonical bytes |
| BPC_PAYLOAD_MAX_BYTES | Int | 5000000 | Max payload canonical bytes |
| BPC_RECEIPT_MAX_BYTES | Int | 500000 | Max receipt canonical bytes |
| BPC_SIGNING_ED25519_PRIVATE_KEY_BASE64 | Text |  | ED25519 private key; REQUIRED in prod |
| BPC_SIGNING_KEY_ID | Text | dev-key-1 | Signing key identifier |
| BPC_API_KEY_PEPPER_BASE64 | Text |  | Pepper for API key hashing; REQUIRED in prod |
| BPC_RATE_LIMIT_ENABLED | Bool | false | Enable rate limiting |
| BPC_RATE_LIMIT_RPS | Int | 50 | Refill tokens per second |
| BPC_RATE_LIMIT_BURST | Int | 100 | Bucket capacity |
| BPC_RETENTION_EVENTS_DAYS | Int | 5475 | Retention 15y |
| BPC_RETENTION_PASSPORT_VERSIONS_DAYS | Int | 5475 | Retention 15y |
| BPC_RETENTION_DOCUMENT_VERSIONS_DAYS | Int | 3650 | Retention 10y |
| BPC_LOG_LEVEL | Text | info | debug|info|warn|error |
| BPC_LOG_JSON | Bool | true | Structured JSON logs |
| BPC_METRICS_ENABLED | Bool | true | Expose /v1/metrics |
| BPC_HEALTH_REQUIRE_QUEUE | Bool | false | Ready fails if queue down |
### 4.7 Local Development Setup (Schritt-für-Schritt, MUST)
```bash
# 1) Dependencies: ghc/cabal, docker, dbmate, fourmolu, hlint
docker compose up -d postgres rabbitmq

# 2) DB schema + seed
./scripts/migrate.sh
./scripts/seed-dev.sh

# 3) Build + tests
cabal build all
cabal test all

# 4) Run services
cabal run bpc-api:exe:bpc-api
cabal run bpc-worker:exe:bpc-worker
```
scripts/migrate.sh (komplett):
```bash
#!/usr/bin/env bash
set -euo pipefail
export DATABASE_URL="${DATABASE_URL:-postgres://bpc:bpc@${BPC_DB_HOST:-localhost}:${BPC_DB_PORT:-5432}/${BPC_DB_NAME:-bpc}?sslmode=disable}"
dbmate up
```
## 5. Haskell Architektur
### 5.1 Effect System (MUST)
- `ReaderT Env (ExceptT AppError IO)` für API/Worker; core bleibt IO-frei.
```haskell
newtype AppM a = AppM { unAppM :: ReaderT Env (ExceptT AppError IO) a }
  deriving newtype (Functor,Applicative,Monad,MonadIO,MonadReader Env,MonadError AppError)

compilePassportPure :: CompileInput -> Either CompileError CompileOutput
```
### 5.2 Error Handling (MUST)
- API/Worker boundary: nur `AppError` nach außen; `CompileError` wird zu `AppError` gemappt.
- DB errors kategorisiert: unique violation→409; connectivity→503 DB_UNAVAILABLE.
### 5.3 JSON (MUST)
- API JSON via Aeson (Generics ok).
- Canonical JSON für Hashing: BPC-CJSON-1 (kein float; key-order).
### 5.4 DB Access Pattern (MUST)
- `postgresql-simple` + pool; jede Repo-Funktion nimmt `TenantId` und filtert `tenant_id`.
### 5.5 Core Type Signatures (MUST, Auswahl)
```haskell
canonicalEncode :: Value -> Either CanonicalError ByteString
canonicalDecode :: ByteString -> Either CanonicalError Value

sha256Hex :: ByteString -> Text
base32NoPad :: ByteString -> Text

topoSortFields :: Map FieldPath UntypedExpr -> Either CycleError [FieldPath]

compilePassportPure :: CompileInput -> Either CompileError CompileOutput
verifyProof :: ByteString -> Either ProofError ()

buildReceiptUnsigned :: ReceiptInput -> ReceiptUnsigned
hashReceiptUnsigned :: ReceiptUnsigned -> ReceiptHash
signReceiptHash :: Ed25519PrivateKey -> ReceiptHash -> Signature

buildQrPayload :: QrInput -> Text
```
## 6. Datenmodell (SQL DDL + Seeds + Migrationen)
### 6.1 Tool
- Migrations Tool: **dbmate**; deploy order: migrate → api → worker.
### 6.2 001_init.sql (alle Tabellen, Constraints, Indizes)
```sql
BEGIN;
CREATE EXTENSION IF NOT EXISTS pgcrypto;
-- ENUMS
DO $$ BEGIN CREATE TYPE actor_type AS ENUM ('USER','API_CLIENT','SERVICE'); EXCEPTION WHEN duplicate_object THEN null; END $$;
DO $$ BEGIN CREATE TYPE job_type AS ENUM ('INGEST_DOCUMENT','PARSE_FACTS','BUILD_SNAPSHOT','COMPILE_PASSPORT','RUN_RULE_TESTS','SIGN_PASSPORT','GENERATE_QR','EXPORT_PASSPORT','DELIVER_WEBHOOK'); EXCEPTION WHEN duplicate_object THEN null; END $$;
DO $$ BEGIN CREATE TYPE job_status AS ENUM ('QUEUED','RUNNING','SUCCEEDED','FAILED','CANCELLED','DEAD_LETTER'); EXCEPTION WHEN duplicate_object THEN null; END $$;
DO $$ BEGIN CREATE TYPE document_kind AS ENUM ('BOM','PCF','DUE_DILIGENCE','OTHER'); EXCEPTION WHEN duplicate_object THEN null; END $$;
DO $$ BEGIN CREATE TYPE document_status AS ENUM ('UPLOADED','PARSED','VALIDATED','REJECTED'); EXCEPTION WHEN duplicate_object THEN null; END $$;
DO $$ BEGIN CREATE TYPE snapshot_status AS ENUM ('BUILDING','READY','SEALED'); EXCEPTION WHEN duplicate_object THEN null; END $$;
DO $$ BEGIN CREATE TYPE rule_pkg_status AS ENUM ('DRAFT','VALIDATED','PUBLISHED','DEPRECATED','RETIRED'); EXCEPTION WHEN duplicate_object THEN null; END $$;
DO $$ BEGIN CREATE TYPE passport_status AS ENUM ('COMPILING','SIGNED','ACTIVE','SUPERSEDED','REVOKED'); EXCEPTION WHEN duplicate_object THEN null; END $$;
DO $$ BEGIN CREATE TYPE access_decision AS ENUM ('ALLOW','DENY'); EXCEPTION WHEN duplicate_object THEN null; END $$;
-- TENANTS
CREATE TABLE IF NOT EXISTS tenants (
  tenant_id uuid PRIMARY KEY,
  slug text NOT NULL UNIQUE CHECK (slug ~ '^[a-z0-9-]{3,64}$'),
  name text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now()
);
-- ACTORS
CREATE TABLE IF NOT EXISTS actors (
  actor_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  type actor_type NOT NULL,
  display_name text NOT NULL,
  email text NULL,
  is_active boolean NOT NULL DEFAULT true,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(tenant_id, email)
);
CREATE INDEX IF NOT EXISTS actors_by_tenant ON actors(tenant_id);
-- API KEYS
CREATE TABLE IF NOT EXISTS api_keys (
  api_key_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  actor_id uuid NOT NULL REFERENCES actors(actor_id) ON DELETE CASCADE,
  key_prefix text NOT NULL,
  key_hash bytea NOT NULL,
  label text NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  revoked_at timestamptz NULL,
  UNIQUE(tenant_id, key_prefix),
  UNIQUE(tenant_id, key_hash)
);
CREATE INDEX IF NOT EXISTS api_keys_by_tenant ON api_keys(tenant_id);
-- ROLES & PERMISSIONS
CREATE TABLE IF NOT EXISTS roles (
  role_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  name text NOT NULL,
  UNIQUE(tenant_id, name)
);
CREATE TABLE IF NOT EXISTS permissions (
  permission text PRIMARY KEY
);
CREATE TABLE IF NOT EXISTS role_permissions (
  role_id uuid NOT NULL REFERENCES roles(role_id) ON DELETE CASCADE,
  permission text NOT NULL REFERENCES permissions(permission) ON DELETE CASCADE,
  PRIMARY KEY(role_id, permission)
);
CREATE TABLE IF NOT EXISTS actor_roles (
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  actor_id uuid NOT NULL REFERENCES actors(actor_id) ON DELETE CASCADE,
  role_id uuid NOT NULL REFERENCES roles(role_id) ON DELETE CASCADE,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(tenant_id, actor_id, role_id)
);
CREATE INDEX IF NOT EXISTS actor_roles_by_actor ON actor_roles(tenant_id, actor_id);
-- IDEMPOTENCY KEYS (HTTP)
CREATE TABLE IF NOT EXISTS idempotency_keys (
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  key text NOT NULL,
  request_hash bytea NOT NULL,
  response_status int NOT NULL,
  response_headers jsonb NOT NULL,
  response_body bytea NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(tenant_id, key)
);
CREATE INDEX IF NOT EXISTS idempotency_by_tenant_time ON idempotency_keys(tenant_id, created_at DESC);
-- EVENT STORE
CREATE TABLE IF NOT EXISTS events (
  event_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  aggregate_type text NOT NULL,
  aggregate_id uuid NOT NULL,
  aggregate_version bigint NOT NULL CHECK (aggregate_version >= 1),
  event_type text NOT NULL,
  occurred_at timestamptz NOT NULL DEFAULT now(),
  actor_id uuid NULL REFERENCES actors(actor_id) ON DELETE SET NULL,
  payload jsonb NOT NULL,
  payload_canonical bytea NOT NULL,
  payload_hash bytea NOT NULL,
  prev_event_hash bytea NULL,
  event_hash bytea NOT NULL,
  UNIQUE(tenant_id, aggregate_type, aggregate_id, aggregate_version)
);
CREATE INDEX IF NOT EXISTS events_by_aggregate ON events(tenant_id, aggregate_type, aggregate_id, aggregate_version);
CREATE INDEX IF NOT EXISTS events_by_type_time ON events(tenant_id, event_type, occurred_at);
-- DOCUMENTS
CREATE TABLE IF NOT EXISTS documents (
  document_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  kind document_kind NOT NULL,
  external_ref text NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  created_by uuid NULL REFERENCES actors(actor_id) ON DELETE SET NULL
);
CREATE INDEX IF NOT EXISTS documents_by_tenant ON documents(tenant_id);
CREATE TABLE IF NOT EXISTS document_versions (
  document_version_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  document_id uuid NOT NULL REFERENCES documents(document_id) ON DELETE CASCADE,
  version int NOT NULL CHECK (version >= 1),
  status document_status NOT NULL,
  mime_type text NOT NULL,
  sha256 bytea NOT NULL,
  content bytea NOT NULL,
  uploaded_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(tenant_id, document_id, version),
  UNIQUE(tenant_id, sha256)
);
CREATE INDEX IF NOT EXISTS document_versions_by_document ON document_versions(tenant_id, document_id, version);
-- FACTS
CREATE TABLE IF NOT EXISTS facts (
  fact_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  fact_type text NOT NULL,
  fact_key text NOT NULL,
  schema_version int NOT NULL CHECK (schema_version >= 1),
  source_document_version_id uuid NOT NULL REFERENCES document_versions(document_version_id) ON DELETE RESTRICT,
  payload jsonb NOT NULL,
  payload_canonical bytea NOT NULL,
  payload_hash bytea NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(tenant_id, fact_type, fact_key, schema_version, payload_hash)
);
CREATE INDEX IF NOT EXISTS facts_lookup ON facts(tenant_id, fact_type, fact_key);
CREATE INDEX IF NOT EXISTS facts_by_docver ON facts(tenant_id, source_document_version_id);
-- SNAPSHOTS
CREATE TABLE IF NOT EXISTS data_snapshots (
  snapshot_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  status snapshot_status NOT NULL,
  label text NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  sealed_at timestamptz NULL,
  created_by uuid NULL REFERENCES actors(actor_id) ON DELETE SET NULL,
  snapshot_canonical bytea NULL,
  snapshot_hash bytea NULL
);
CREATE INDEX IF NOT EXISTS snapshots_by_tenant ON data_snapshots(tenant_id, created_at);
CREATE TABLE IF NOT EXISTS snapshot_items (
  snapshot_id uuid NOT NULL REFERENCES data_snapshots(snapshot_id) ON DELETE CASCADE,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  fact_id uuid NOT NULL REFERENCES facts(fact_id) ON DELETE RESTRICT,
  PRIMARY KEY(snapshot_id, fact_id)
);
CREATE INDEX IF NOT EXISTS snapshot_items_by_snapshot ON snapshot_items(tenant_id, snapshot_id);
-- BATTERY PRODUCTS
CREATE TABLE IF NOT EXISTS battery_products (
  battery_product_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  sku text NOT NULL,
  name text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(tenant_id, sku)
);
CREATE INDEX IF NOT EXISTS battery_products_by_tenant ON battery_products(tenant_id);
-- PASSPORTS
CREATE TABLE IF NOT EXISTS passports (
  passport_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  battery_product_id uuid NOT NULL REFERENCES battery_products(battery_product_id) ON DELETE CASCADE,
  current_passport_version_id uuid NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(tenant_id, battery_product_id)
);
CREATE INDEX IF NOT EXISTS passports_by_tenant ON passports(tenant_id);
-- RULE PACKAGES
CREATE TABLE IF NOT EXISTS rule_packages (
  rule_package_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  name text NOT NULL,
  description text NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  created_by uuid NULL REFERENCES actors(actor_id) ON DELETE SET NULL,
  UNIQUE(tenant_id, name)
);
CREATE INDEX IF NOT EXISTS rule_packages_by_tenant ON rule_packages(tenant_id);
CREATE TABLE IF NOT EXISTS rule_package_versions (
  rule_package_version_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  rule_package_id uuid NOT NULL REFERENCES rule_packages(rule_package_id) ON DELETE CASCADE,
  version int NOT NULL CHECK (version >= 1),
  status rule_pkg_status NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  published_at timestamptz NULL,
  compiler_min_build text NOT NULL,
  dsl_source bytea NOT NULL,
  dsl_sha256 bytea NOT NULL,
  tests_source bytea NOT NULL,
  tests_sha256 bytea NOT NULL,
  UNIQUE(tenant_id, rule_package_id, version),
  UNIQUE(tenant_id, dsl_sha256, tests_sha256)
);
CREATE INDEX IF NOT EXISTS rule_pkg_versions_by_pkg ON rule_package_versions(tenant_id, rule_package_id, version);
CREATE TABLE IF NOT EXISTS rule_fields (
  rule_field_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  rule_package_version_id uuid NOT NULL REFERENCES rule_package_versions(rule_package_version_id) ON DELETE CASCADE,
  field_path text NOT NULL,
  field_type text NOT NULL,
  required boolean NOT NULL DEFAULT true,
  UNIQUE(tenant_id, rule_package_version_id, field_path)
);
CREATE INDEX IF NOT EXISTS rule_fields_by_version ON rule_fields(tenant_id, rule_package_version_id);
CREATE TABLE IF NOT EXISTS rule_tests_runs (
  run_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  rule_package_version_id uuid NOT NULL REFERENCES rule_package_versions(rule_package_version_id) ON DELETE CASCADE,
  seed bigint NOT NULL,
  cases int NOT NULL CHECK (cases >= 1),
  status text NOT NULL CHECK (status IN ('PASSED','FAILED')),
  failure jsonb NULL,
  created_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS rule_tests_runs_by_version ON rule_tests_runs(tenant_id, rule_package_version_id, created_at DESC);
-- PASSPORT VERSIONS
CREATE TABLE IF NOT EXISTS passport_versions (
  passport_version_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  passport_id uuid NOT NULL REFERENCES passports(passport_id) ON DELETE CASCADE,
  status passport_status NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  activated_at timestamptz NULL,
  superseded_at timestamptz NULL,
  revoked_at timestamptz NULL,
  snapshot_id uuid NOT NULL REFERENCES data_snapshots(snapshot_id) ON DELETE RESTRICT,
  rule_package_version_id uuid NOT NULL REFERENCES rule_package_versions(rule_package_version_id) ON DELETE RESTRICT,
  compiler_build_id text NOT NULL,
  payload_canonical bytea NOT NULL,
  payload_hash bytea NOT NULL,
  proof_canonical bytea NOT NULL,
  proof_root_hash bytea NOT NULL,
  receipt_canonical bytea NOT NULL,
  receipt_hash bytea NOT NULL,
  signature_alg text NOT NULL CHECK (signature_alg IN ('ED25519')),
  signature bytea NULL,
  signing_key_id text NULL,
  qr_png bytea NULL,
  qr_payload text NULL,
  UNIQUE(tenant_id, passport_id, payload_hash)
);
CREATE INDEX IF NOT EXISTS passport_versions_lookup ON passport_versions(tenant_id, passport_id, created_at DESC);
CREATE INDEX IF NOT EXISTS passport_versions_by_hash ON passport_versions(tenant_id, payload_hash);
-- link current_passport_version_id
ALTER TABLE passports
  DROP CONSTRAINT IF EXISTS passports_current_passport_version_id_fkey;
ALTER TABLE passports
  ADD CONSTRAINT passports_current_passport_version_id_fkey
  FOREIGN KEY (current_passport_version_id)
  REFERENCES passport_versions(passport_version_id)
  ON DELETE SET NULL;
-- JOBS
CREATE TABLE IF NOT EXISTS jobs (
  job_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  type job_type NOT NULL,
  status job_status NOT NULL,
  priority int NOT NULL DEFAULT 100,
  attempts int NOT NULL DEFAULT 0 CHECK (attempts >= 0),
  max_attempts int NOT NULL DEFAULT 10 CHECK (max_attempts >= 1),
  idempotency_key text NOT NULL,
  payload jsonb NOT NULL,
  scheduled_at timestamptz NOT NULL DEFAULT now(),
  started_at timestamptz NULL,
  finished_at timestamptz NULL,
  lease_owner text NULL,
  lease_expires_at timestamptz NULL,
  last_error jsonb NULL,
  UNIQUE(tenant_id, type, idempotency_key)
);
CREATE INDEX IF NOT EXISTS jobs_queue ON jobs(tenant_id, status, priority, scheduled_at);
CREATE INDEX IF NOT EXISTS jobs_lease ON jobs(tenant_id, status, lease_expires_at);
COMMIT;
```
### 6.3 002_seed_permissions_roles.sql (Seed Data)
```sql
BEGIN;

INSERT INTO permissions(permission) VALUES
  ('passport:read'),
  ('passport:compile'),
  ('passport:sign'),
  ('passport:replay'),
  ('passport:revoke'),
  ('rules:read'),
  ('rules:write'),
  ('rules:publish'),
  ('rules:test'),
  ('docs:upload'),
  ('docs:read'),
  ('snapshot:build'),
  ('snapshot:seal'),
  ('audit:read'),
  ('access:admin'),
  ('tenant:admin')
ON CONFLICT DO NOTHING;

INSERT INTO tenants(tenant_id, slug, name)
VALUES (gen_random_uuid(), 'dev', 'Dev Tenant')
ON CONFLICT (slug) DO NOTHING;

DO $$
DECLARE t uuid;
DECLARE r_admin uuid;
DECLARE r_comp uuid;
DECLARE r_aud uuid;
BEGIN
  SELECT tenant_id INTO t FROM tenants WHERE slug='dev';
  IF t IS NULL THEN RAISE EXCEPTION 'dev tenant missing'; END IF;

  INSERT INTO roles(role_id, tenant_id, name) VALUES (gen_random_uuid(), t, 'Admin')
  ON CONFLICT (tenant_id, name) DO NOTHING;
  SELECT role_id INTO r_admin FROM roles WHERE tenant_id=t AND name='Admin';

  INSERT INTO roles(role_id, tenant_id, name) VALUES (gen_random_uuid(), t, 'ComplianceOfficer')
  ON CONFLICT (tenant_id, name) DO NOTHING;
  SELECT role_id INTO r_comp FROM roles WHERE tenant_id=t AND name='ComplianceOfficer';

  INSERT INTO roles(role_id, tenant_id, name) VALUES (gen_random_uuid(), t, 'Auditor')
  ON CONFLICT (tenant_id, name) DO NOTHING;
  SELECT role_id INTO r_aud FROM roles WHERE tenant_id=t AND name='Auditor';

  INSERT INTO role_permissions(role_id, permission)
    SELECT r_admin, p.permission FROM permissions p
  ON CONFLICT DO NOTHING;

  INSERT INTO role_permissions(role_id, permission) VALUES
    (r_comp,'passport:read'),
    (r_comp,'passport:compile'),
    (r_comp,'passport:replay'),
    (r_comp,'rules:read'),
    (r_comp,'docs:read'),
    (r_comp,'snapshot:build'),
    (r_comp,'snapshot:seal')
  ON CONFLICT DO NOTHING;

  INSERT INTO role_permissions(role_id, permission) VALUES
    (r_aud,'passport:read'),
    (r_aud,'rules:read'),
    (r_aud,'audit:read')
  ON CONFLICT DO NOTHING;
END $$;

COMMIT;
```
### 6.4 003_add_policies_webhooks.sql
```sql
BEGIN;

CREATE TABLE IF NOT EXISTS policies (
  policy_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  name text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  created_by uuid NULL REFERENCES actors(actor_id) ON DELETE SET NULL,
  UNIQUE(tenant_id, name)
);

CREATE TABLE IF NOT EXISTS policy_versions (
  policy_version_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  policy_id uuid NOT NULL REFERENCES policies(policy_id) ON DELETE CASCADE,
  version int NOT NULL CHECK (version >= 1),
  is_active boolean NOT NULL DEFAULT true,
  effect access_decision NOT NULL,
  target text NOT NULL,
  priority int NOT NULL DEFAULT 100,
  policy_json jsonb NOT NULL,
  policy_canonical bytea NOT NULL,
  policy_hash bytea NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(tenant_id, policy_id, version)
);

CREATE INDEX IF NOT EXISTS policy_versions_by_policy ON policy_versions(tenant_id, policy_id, version);
CREATE INDEX IF NOT EXISTS policy_versions_active ON policy_versions(tenant_id, target, is_active, priority);

CREATE TABLE IF NOT EXISTS webhook_endpoints (
  webhook_endpoint_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  url text NOT NULL,
  secret_base64 text NOT NULL,
  is_active boolean NOT NULL DEFAULT true,
  created_at timestamptz NOT NULL DEFAULT now(),
  created_by uuid NULL REFERENCES actors(actor_id) ON DELETE SET NULL
);

CREATE TABLE IF NOT EXISTS webhook_subscriptions (
  webhook_subscription_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  webhook_endpoint_id uuid NOT NULL REFERENCES webhook_endpoints(webhook_endpoint_id) ON DELETE CASCADE,
  event_type text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(tenant_id, webhook_endpoint_id, event_type)
);

CREATE TABLE IF NOT EXISTS webhook_deliveries (
  webhook_delivery_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  webhook_endpoint_id uuid NOT NULL REFERENCES webhook_endpoints(webhook_endpoint_id) ON DELETE CASCADE,
  event_id uuid NOT NULL REFERENCES events(event_id) ON DELETE CASCADE,
  attempt int NOT NULL DEFAULT 0,
  status text NOT NULL CHECK (status IN ('PENDING','DELIVERED','FAILED')),
  last_error jsonb NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  delivered_at timestamptz NULL
);

CREATE INDEX IF NOT EXISTS webhook_deliveries_by_status ON webhook_deliveries(tenant_id, status, created_at);

COMMIT;
```
### 6.5 004_add_rate_limits.sql
```sql
BEGIN;

CREATE TABLE IF NOT EXISTS rate_limit_buckets (
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  key_hash bytea NOT NULL,
  capacity int NOT NULL,
  tokens numeric NOT NULL,
  refill_per_second numeric NOT NULL,
  updated_at timestamptz NOT NULL,
  PRIMARY KEY(tenant_id, key_hash)
);

CREATE INDEX IF NOT EXISTS rate_limit_buckets_by_tenant ON rate_limit_buckets(tenant_id);

COMMIT;
```
### 6.6 Retention Defaults (MUST)
- Defaults via ENV (4.6). Retention sweep job emittiert `RetentionApplied` Event.
## 7. Algorithmen
### 7.1 Canonical JSON (BPC-CJSON-1)
- Object keys sortiert nach UTF-8 bytes; Floats/exponent numbers verboten; keine Whitespaces.
```text
canonicalEncode(v):
  Null->"null"; Bool->"true/false"; String->escape
  Number(n): allow only integer (no exp/fraction) else FAIL CanonicalNumberNotAllowed
  Array: "[" + join(",", map(canonicalEncode)) + "]"
  Object: "{" + join(",", for k in sortUtf8(keys): escape(k)+":"+canonicalEncode(v[k])) + "}"
```
Beispiel: input `{"b":2,"a":1}` ⇒ bytes `{"a":1,"b":2}`.
### 7.2 SHA-256 (BPC-HASH-1)
- Beispiel bytes: `{"a":1,"b":2}`
- sha256_hex: `43258cff783fe7036d8a43033f830adfc60ec037382473548ac742b888292777`
### 7.3 Snapshot Seal (BPC-SNAPSHOT-1)
```text
sealSnapshot(snapshot):
  require snapshot.status==READY
  items = list (fact_type,fact_key,payload_hash_hex) for snapshot_items
  sort items by (fact_type,fact_key,payload_hash_hex)
  canon = canonicalEncode({snapshot_version:"BPC-SNAPSHOT-1",snapshot_id,facts:items})
  hash = sha256_hex(canon)
  persist status=SEALED, sealed_at=now(), snapshot_canonical=canon, snapshot_hash=hash
```
Edge cases: READY!=true ⇒ SNAPSHOT_NOT_READY; SEALED ⇒ SNAPSHOT_SEALED.
### 7.4 Event Hash Chain (BPC-EVENT-1)
```text
event_hash = sha256_hex(canonicalEncode({
  tenant_id, aggregate_type, aggregate_id, aggregate_version,
  event_type, occurred_at, actor_id, payload_hash, prev_event_hash
}))
```
Edge: concurrent append ⇒ EVENT_VERSION_CONFLICT (retryable).
### 7.5 Topo Sort Rules (BPC-GRAPH-1)
- Kahn + lexical tie-break; cycle ⇒ RULE_CYCLE_DETECTED(cycle_fields).
### 7.6 Compile (BPC-COMPILE-1)
- Eval rules in topo order; emit proof nodes; build payload; build receipt unsigned; hash all; size limits:
  - payload ≤ 131072 bytes
  - proof ≤ 262144 bytes
  - receipt ≤ 16384 bytes
### 7.7 Receipt Sign (BPC-SIGN-1)
- receipt_hash = sha256_hex(canonicalEncode(receipt without `signature` field))
- signature = ED25519.sign(private_key, receipt_hash_bytes)
### 7.8 QR Payload (BPC-QR-1)
- Format: `BPC1|pv=<uuid>|ph=<b32>|pr=<b32>|rh=<b32>` (base32 RFC4648, no padding).
### 7.9 Token Bucket Rate Limit (BPC-RL-1)
- Atomic consume via `SELECT ... FOR UPDATE`; deny if tokens<1; return Retry-After.
### 7.10 Webhook HMAC (BPC-WH-1)
- `X-BPC-Signature: sha256=<hex(hmac_sha256(secret, body_bytes))>`.
### 7.11 HTTP Idempotency (BPC-IDEMP-1)
- On first request: store (request_hash,response_status,headers,body) in idempotency_keys.
- On repeat: same request_hash ⇒ replay stored response; different ⇒ IDEMPOTENCY_CONFLICT.
## 8. Rule-DSL (BPC-RULES-1)
### 8.1 Vollständige EBNF (normativ)
```ebnf
source        = { rule | test } ;
rule          = "field" , fieldPath , ":" , typeExpr , "=" , expr , ";" ;

test          = exampleTest | propertyTest ;
exampleTest   = "example" , identifier , ":" , "{" , factFixtures , "}" , "=>" , assertions , ";" ;
propertyTest  = "property" , identifier , ":" , generatorDecl , "=>" , propAssertions , ";" ;

factFixtures  = { factFixture } ;
factFixture   = "fact" , identifier , "(" , stringLit , "," , stringLit , ")" , "=" , jsonObject , ";" ;

assertions    = { assertion } ;
assertion     = "expect" , "(" , fieldPath , "," , comparator , literal , ")" , ";" ;
comparator    = "==" | "!=" | "<" | "<=" | ">" | ">=" ;

generatorDecl = "cases" , "(" , intLit , ")" , "seed" , "(" , intLit , ")" ;
propAssertions = { propAssertion } ;
propAssertion  = "forall" , identifier , ":" , typeExpr , "." , propExpr , ";" ;
propExpr       = "implies" , "(" , expr , "," , expr , ")" ;

fieldPath     = identifier , { "." , identifier } ;

typeExpr      = primType
              | "Opt(" , typeExpr , ")"
              | "List(" , typeExpr , ")"
              | "Map(Text," , typeExpr , ")"
              | "Qty(" , unitExpr , ")"
              | "Dec(" , intLit , ")"
              | "Record(" , recordFields , ")"
              ;

recordFields  = identifier , ":" , typeExpr , { "," , identifier , ":" , typeExpr } ;

primType      = "Bool" | "Int" | "Text" | "Date" ;

unitExpr      = "kg" | "g" | "kWh" | "Wh" | "gCO2e" | "kgCO2e" | "gCO2e_per_kWh" | "pct" | "each" ;

expr          = letExpr | ifExpr | assertExpr | logicExpr ;
letExpr       = "let" , identifier , "=" , expr , ";" , expr ;
ifExpr        = "if" , "(" , expr , ")" , "then" , expr , "else" , expr ;
assertExpr    = "assert" , "(" , expr , "," , stringLit , "," , stringLit , ")" , ";" , expr ;

logicExpr     = orExpr ;
orExpr        = andExpr , { "||" , andExpr } ;
andExpr       = cmpExpr , { "&&" , cmpExpr } ;
cmpExpr       = addExpr , [ ( "==" | "!=" | "<" | "<=" | ">" | ">=" ) , addExpr ] ;
addExpr       = mulExpr , { ( "+" | "-" ) , mulExpr } ;
mulExpr       = unaryExpr , { ( "*" | "/" ) , unaryExpr } ;
unaryExpr     = primary | "!" , unaryExpr ;

primary       = literal
              | identifier
              | "(" , expr , ")"
              | funcCall
              ;

funcCall      = identifier , "(" , [ args ] , ")" ;
args          = expr , { "," , expr } ;

literal       = boolLit | intLit | decLit | stringLit | dateLit | qtyLit | "none" ;
boolLit       = "true" | "false" ;
intLit        = ["-"] , digit , { digit } ;
decLit        = ["-"] , digit , { digit } , "." , digit , { digit } ;
dateLit       = "date" , "(" , stringLit , ")" ;
qtyLit        = "qty" , "(" , decLit , "," , unitExpr , ")" ;

jsonObject    = "{" , { jsonPair } , "}" ;
jsonPair      = stringLit , ":" , jsonValue , [ "," ] ;
jsonValue     = stringLit | intLit | "true" | "false" | "null" | jsonObject | jsonArray ;
jsonArray     = "[" , { jsonValue , [ "," ] } , "]" ;

identifier    = ? [A-Za-z_][A-Za-z0-9_]* ? ;
stringLit     = "\"" , { stringChar } , "\"" ;
stringChar    = unescapedChar | escapeSeq ;
unescapedChar = ? any Unicode char except '"' or '\\' or control chars ? ;
escapeSeq     = "\\" , ( "\"" | "\\" | "/" | "b" | "f" | "n" | "r" | "t" | ( "u" , hex , hex , hex , hex ) ) ;
hex           = digit | "A" | "B" | "C" | "D" | "E" | "F" | "a" | "b" | "c" | "d" | "e" | "f" ;
digit         = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
```
### 8.2 Built-ins (vollständig)
| fn | Signatur | Errors/Notes |
|---|---|---|
| getFact | (Text,Text) -> Opt(Record) | OK |
| getFactsByPrefix | (Text,Text) -> List(Record) | OK |
| field | (Text) -> any | RULE_TYPE_ERROR wenn unbekannt |
| recordGet | (Record,Text) -> Opt(any) | OK |
| isSome | Opt(a) -> Bool | OK |
| unwrapOr | (Opt(a),a) -> a | OK |
| requireSome | (Opt(a),Text,Text) -> a | OK |
| toDec | (Int,Int|Dec) -> Dec(scale) | OK |
| toQty | (Text,Dec|Int) -> Qty(unit) | UNIT_MISMATCH wenn unit unbekannt |
| convert | (Text,Text,Qty(u)) -> Qty(v) | UNIT_MISMATCH |
| sumQty | (Text,List(Qty(u))) -> Qty(u) | UNIT_MISMATCH |
| sumDec | (Int,List(Dec(_))) -> Dec(scale) | OK |
| map | (List(a), (a->b)) -> List(b) | OK |
| filter | (List(a), (a->Bool)) -> List(a) | OK |
| fold | (List(a), b, (b,a->b)) -> b | OK |
| emitCompliance | (Text,Text,Text) -> Bool | OK |
### 8.3 Typechecking Regeln (MUST)
- `+/-`: nur gleiche Unit/Scale. `*/`: Qty×Dec, Qty÷Dec. Comparisons nur kompatibel.
- Unknown field refs/unknown units ⇒ RULE_TYPE_ERROR / UNIT_MISMATCH.
### 8.4 Minimal DSL Beispiel
```text
-- Example rules for a minimal passport

field battery.capacity_kwh: Dec(6) =
  let b = getFact("Battery","battery:SKU-123");
  let cap = requireSome(recordGet(requireSome(b,"E001","battery missing"),"capacity_kwh"),"E002","capacity missing");
  toDec(6, cap);

field sustainability.pcf_gco2e_per_kwh: Dec(2) =
  let p = getFact("PCF","pcf:SKU-123");
  let total = requireSome(recordGet(requireSome(p,"E010","pcf missing"),"total_gco2e"),"E011","missing total");
  let cap = field("battery.capacity_kwh");
  -- division: gCO2e / kWh => gCO2e_per_kWh, represented as Dec(2) string
  toDec(2, toDec(2,total) / cap);
```
## 9. Proof & Receipt & Replay
### 9.1 Proof JSON Schema (BPC-PROOF-1)
```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://bpc.example/schema/BPC-PROOF-1.json",
  "title": "BPC-PROOF-1 DerivationTree",
  "type": "object",
  "required": [
    "proof_version",
    "root_hash",
    "nodes",
    "field_index"
  ],
  "properties": {
    "proof_version": {
      "const": "BPC-PROOF-1"
    },
    "root_hash": {
      "type": "string",
      "pattern": "^[a-f0-9]{64}$"
    },
    "nodes": {
      "type": "array",
      "items": {
        "$ref": "#/$defs/node"
      },
      "minItems": 1
    },
    "field_index": {
      "type": "object",
      "additionalProperties": {
        "type": "integer",
        "minimum": 0
      }
    }
  },
  "$defs": {
    "hash": {
      "type": "string",
      "pattern": "^[a-f0-9]{64}$"
    },
    "node": {
      "type": "object",
      "required": [
        "id",
        "type",
        "hash",
        "data",
        "children"
      ],
      "properties": {
        "id": {
          "type": "integer",
          "minimum": 0
        },
        "type": {
          "type": "string",
          "enum": [
            "CONST",
            "FACT_GET",
            "FIELD_REF",
            "OP",
            "ASSERT",
            "COMPLIANCE_EMIT"
          ]
        },
        "hash": {
          "$ref": "#/$defs/hash"
        },
        "data": {
          "type": [
            "object",
            "string",
            "number",
            "boolean",
            "null"
          ]
        },
        "children": {
          "type": "array",
          "items": {
            "type": "integer",
            "minimum": 0
          }
        }
      }
    }
  }
}
```
### 9.2 Node Hashing (BPC-PROOF-HASH-1)
- `node.hash = sha256_hex(canonicalEncode({id,type,data,children_hashes}))`
- `children_hashes` ist ordered array der child hashes.
### 9.3 Receipt JSON Schema (BPC-RECEIPT-1)
```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://bpc.example/schema/BPC-RECEIPT-1.json",
  "title": "BPC-RECEIPT-1 Receipt",
  "type": "object",
  "required": [
    "receipt_version",
    "issued_at",
    "tenant_id",
    "passport_version_id",
    "passport_id",
    "battery_product_id",
    "snapshot_id",
    "snapshot_hash",
    "rule_package_version_id",
    "dsl_sha256",
    "tests_sha256",
    "compiler_build_id",
    "payload_hash",
    "proof_root_hash",
    "signature_alg",
    "signing_key_id",
    "signature"
  ],
  "properties": {
    "receipt_version": {
      "const": "BPC-RECEIPT-1"
    },
    "issued_at": {
      "type": "string",
      "format": "date-time"
    },
    "tenant_id": {
      "type": "string",
      "format": "uuid"
    },
    "passport_version_id": {
      "type": "string",
      "format": "uuid"
    },
    "passport_id": {
      "type": "string",
      "format": "uuid"
    },
    "battery_product_id": {
      "type": "string",
      "format": "uuid"
    },
    "snapshot_id": {
      "type": "string",
      "format": "uuid"
    },
    "snapshot_hash": {
      "type": "string",
      "pattern": "^[a-f0-9]{64}$"
    },
    "rule_package_version_id": {
      "type": "string",
      "format": "uuid"
    },
    "dsl_sha256": {
      "type": "string",
      "pattern": "^[a-f0-9]{64}$"
    },
    "tests_sha256": {
      "type": "string",
      "pattern": "^[a-f0-9]{64}$"
    },
    "compiler_build_id": {
      "type": "string"
    },
    "payload_hash": {
      "type": "string",
      "pattern": "^[a-f0-9]{64}$"
    },
    "proof_root_hash": {
      "type": "string",
      "pattern": "^[a-f0-9]{64}$"
    },
    "signature_alg": {
      "type": "string",
      "enum": [
        "ED25519"
      ]
    },
    "signing_key_id": {
      "type": "string"
    },
    "signature": {
      "type": "string",
      "pattern": "^[A-Za-z0-9+/=]+$"
    }
  }
}
```
### 9.4 Storage/Retrieval (MUST)
- `passport_versions.payload_canonical`, `proof_canonical`, `receipt_canonical` sind Source of Truth (bytea).
- API liefert JSON durch `canonicalDecode` der gespeicherten bytes; keine Rekonstruktion/Pretty-formatting.
### 9.5 Retention Policy (MUST)
- `events` und `passport_versions`: default 15 Jahre (`BPC_RETENTION_*_DAYS`).
- `document_versions`: default 10 Jahre.
- Retention job löscht tenant-scoped und emittiert `RetentionApplied` Event (counts+cutoff).
### 9.6 Replay Algorithmus (BPC-REPLAY-1, exakt)
```text
replay(passport_version_id):
  pv = load passport_versions row
  require pv.status in {SIGNED,ACTIVE,SUPERSEDED,REVOKED}
  snapshot = load data_snapshots (must be SEALED)
  rules = load rule_package_versions (must be PUBLISHED/DEPRECATED/RETIRED)
  battery = load via passports -> battery_products

  assert sha256_hex(snapshot.snapshot_canonical) == snapshot.snapshot_hash

  receipt = canonicalDecode(pv.receipt_canonical)
  issued_at = receipt.issued_at

  out = compilePassportPure(CompileInput{snapshot,rules,battery,issued_at,pv.compiler_build_id})

  assert out.payload_hash == pv.payload_hash
  assert out.proof_root_hash == pv.proof_root_hash
  assert out.receipt_hash == pv.receipt_hash

  assert ED25519.verify(pubKey(receipt.signing_key_id), pv.receipt_hash_bytes, pv.signature_bytes)

  return OK
```
## 10. API (Patterns + OpenAPI)
### 10.1 Versioning
- REST Prefix `/v1`. Breaking change ⇒ `/v2`.
### 10.2 Pagination (cursor)
- Query: `limit` (1..200, default 50), `cursor` (base64url(JSON({t,id}))).
- Response: `{items:[...], next_cursor:null|string}`.
### 10.3 Rate limit / Idempotency
- Rate limit: token bucket (DB) via ENV; 429 => RATE_LIMITED + Retry-After.
- Idempotency-Key: required for POST/PUT/PATCH/DELETE; conflict => 409 IDEMPOTENCY_CONFLICT.
### 10.4 Endpoint Tabelle (vollständig)
| Method | Path | Permission | Idem | Beschreibung |
|---|---|---|---|---|
| GET | `/v1/health/live` | — | — | Liveness check |
| GET | `/v1/health/ready` | — | — | Readiness check |
| GET | `/v1/metrics` | — | — | Prometheus metrics |
| POST | `/v1/documents` | docs:upload | Required | Create document |
| POST | `/v1/documents/{document_id}/versions` | docs:upload | Required | Upload new document version (bytes base64) |
| GET | `/v1/documents` | docs:read | — | List documents (cursor) |
| GET | `/v1/document-versions/{document_version_id}` | docs:read | — | Get document version metadata |
| GET | `/v1/document-versions/{document_version_id}/content` | docs:read | — | Download document bytes |
| GET | `/v1/facts` | docs:read | — | List facts (cursor, filters) |
| POST | `/v1/snapshots` | snapshot:build | Required | Create snapshot |
| POST | `/v1/snapshots/{snapshot_id}/items` | snapshot:build | Required | Add fact to snapshot |
| POST | `/v1/snapshots/{snapshot_id}/seal` | snapshot:seal | Required | Seal snapshot |
| GET | `/v1/snapshots/{snapshot_id}` | snapshot:build | — | Get snapshot metadata |
| POST | `/v1/rule-packages` | rules:write | Required | Create rule package |
| POST | `/v1/rule-packages/{rule_package_id}/versions` | rules:write | Required | Create rule package version |
| POST | `/v1/rule-package-versions/{rule_package_version_id}/tests:run` | rules:test | Required | Run rule tests |
| POST | `/v1/rule-package-versions/{rule_package_version_id}:publish` | rules:publish | Required | Publish rule version |
| POST | `/v1/battery-products` | tenant:admin | Required | Create battery product |
| POST | `/v1/passports` | passport:compile | Required | Create passport for product |
| POST | `/v1/passports/{passport_id}/compile` | passport:compile | Required | Enqueue compile job |
| POST | `/v1/passport-versions/{passport_version_id}/activate` | passport:compile | Required | Activate version |
| POST | `/v1/passport-versions/{passport_version_id}/revoke` | passport:revoke | Required | Revoke version |
| POST | `/v1/passport-versions/{passport_version_id}/replay` | passport:replay | Required | Replay/verify version |
| GET | `/v1/passport-versions/{passport_version_id}` | passport:read | — | Get passport version metadata |
| GET | `/v1/passport-versions/{passport_version_id}/payload` | passport:read | — | Get payload canonical JSON |
| GET | `/v1/passport-versions/{passport_version_id}/proof` | passport:read | — | Get proof canonical JSON |
| GET | `/v1/passport-versions/{passport_version_id}/receipt` | passport:read | — | Get receipt canonical JSON |
| GET | `/v1/passport-versions/{passport_version_id}/qr.png` | passport:read | — | Get QR PNG |
| GET | `/v1/audit/events` | audit:read | — | List events (cursor) |
| POST | `/v1/webhook-endpoints` | tenant:admin | Required | Create webhook endpoint |
| POST | `/v1/webhook-subscriptions` | tenant:admin | Required | Subscribe webhook to event_type |
| POST | `/v1/graphql` | passport:read | Optional | GraphQL endpoint (MVP subset) |
### 10.5 OpenAPI (docs/openapi.yaml, normativ, komprimiert)
```yaml
openapi: 3.0.3
info: {title: BatteryPassportCompiler API, version: "1.0.0"}
servers: [{url: "/"}]
security: [{bearerAuth: []}]
components:
  securitySchemes:
    bearerAuth: {type: http, scheme: bearer, bearerFormat: APIKey}
  parameters:
    IdempotencyKey: {name: Idempotency-Key, in: header, required: false, schema: {type: string}}
    Limit: {name: limit, in: query, required: false, schema: {type: integer, default: 50, minimum: 1, maximum: 200}}
    Cursor: {name: cursor, in: query, required: false, schema: {type: string}}
  schemas:
    ErrorEnvelope:
      type: object
      required: [error]
      properties:
        error:
          type: object
          required: [code,message,correlation_id]
          properties:
            code: {type: string}
            message: {type: string}
            correlation_id: {type: string, format: uuid}
            details: {type: object, additionalProperties: true}
    CursorPage:
      type: object
      required: [items,next_cursor]
      properties:
        items: {type: array, items: {type: object}}
        next_cursor: {type: string, nullable: true}
paths:
  /v1/health/live:
    get: {security: [], responses: {"200": {description: OK}}}
  /v1/health/ready:
    get:
      security: []
      responses:
        "200": {description: OK}
        "503": {description: Not Ready, content: {application/json: {schema: {$ref: "#/components/schemas/ErrorEnvelope"}}}}

  /v1/metrics:
    get:
      security: []
      responses:
        "200": {description: Prometheus text}

  /v1/graphql:
    post:
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"200": {description: OK}}

  /v1/documents:
    post:
      parameters: [{$ref: "#/components/parameters/IdempotencyKey"}]
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"201": {description: Created}}
    get:
      parameters: [{$ref: "#/components/parameters/Limit"},{$ref: "#/components/parameters/Cursor"}]
      responses: {"200": {description: OK, content: {application/json: {schema: {$ref: "#/components/schemas/CursorPage"}}}}}
  /v1/documents/{document_id}:
    get:
      parameters: [{name: document_id, in: path, required: true, schema: {type: string, format: uuid}}]
      responses: {"200": {description: OK}}
  /v1/documents/{document_id}/versions:
    post:
      parameters:
        - {name: document_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/IdempotencyKey"}
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"201": {description: Created}}

  /v1/document-versions/{document_version_id}:
    get:
      parameters: [{name: document_version_id, in: path, required: true, schema: {type: string, format: uuid}}]
      responses: {"200": {description: OK}}
  /v1/document-versions/{document_version_id}/content:
    get:
      parameters: [{name: document_version_id, in: path, required: true, schema: {type: string, format: uuid}}]
      responses: {"200": {description: OK, content: {application/octet-stream: {schema: {type: string, format: binary}}}}}

  /v1/facts:
    get:
      parameters:
        - {$ref: "#/components/parameters/Limit"}
        - {$ref: "#/components/parameters/Cursor"}
        - {name: fact_type, in: query, required: false, schema: {type: string}}
        - {name: fact_key_prefix, in: query, required: false, schema: {type: string}}
      responses: {"200": {description: OK, content: {application/json: {schema: {$ref: "#/components/schemas/CursorPage"}}}}}

  /v1/snapshots:
    post:
      parameters: [{$ref: "#/components/parameters/IdempotencyKey"}]
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"201": {description: Created}}
    get:
      parameters: [{$ref: "#/components/parameters/Limit"},{$ref: "#/components/parameters/Cursor"}]
      responses: {"200": {description: OK, content: {application/json: {schema: {$ref: "#/components/schemas/CursorPage"}}}}}
  /v1/snapshots/{snapshot_id}:
    get:
      parameters: [{name: snapshot_id, in: path, required: true, schema: {type: string, format: uuid}}]
      responses: {"200": {description: OK}}
  /v1/snapshots/{snapshot_id}/items:
    post:
      parameters:
        - {name: snapshot_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/IdempotencyKey"}
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"200": {description: OK}}
  /v1/snapshots/{snapshot_id}/seal:
    post:
      parameters:
        - {name: snapshot_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/IdempotencyKey"}
      responses: {"200": {description: OK}}

  /v1/rule-packages:
    post:
      parameters: [{$ref: "#/components/parameters/IdempotencyKey"}]
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"201": {description: Created}}
    get:
      parameters: [{$ref: "#/components/parameters/Limit"},{$ref: "#/components/parameters/Cursor"}]
      responses: {"200": {description: OK, content: {application/json: {schema: {$ref: "#/components/schemas/CursorPage"}}}}}
  /v1/rule-packages/{rule_package_id}:
    get:
      parameters: [{name: rule_package_id, in: path, required: true, schema: {type: string, format: uuid}}]
      responses: {"200": {description: OK}}
  /v1/rule-packages/{rule_package_id}/versions:
    post:
      parameters:
        - {name: rule_package_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/IdempotencyKey"}
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"201": {description: Created}}
  /v1/rule-package-versions/{rule_package_version_id}:
    get:
      parameters: [{name: rule_package_version_id, in: path, required: true, schema: {type: string, format: uuid}}]
      responses: {"200": {description: OK}}
    delete:
      parameters:
        - {name: rule_package_version_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/IdempotencyKey"}
      responses: {"200": {description: Retired}}
  /v1/rule-package-versions/{rule_package_version_id}/tests:run:
    post:
      parameters:
        - {name: rule_package_version_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/IdempotencyKey"}
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"200": {description: OK}}
  /v1/rule-package-versions/{rule_package_version_id}:publish:
    post:
      parameters:
        - {name: rule_package_version_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/IdempotencyKey"}
      responses: {"200": {description: OK}}

  /v1/battery-products:
    post:
      parameters: [{$ref: "#/components/parameters/IdempotencyKey"}]
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"201": {description: Created}}
    get:
      parameters: [{$ref: "#/components/parameters/Limit"},{$ref: "#/components/parameters/Cursor"}]
      responses: {"200": {description: OK, content: {application/json: {schema: {$ref: "#/components/schemas/CursorPage"}}}}}
  /v1/battery-products/{battery_product_id}:
    get:
      parameters: [{name: battery_product_id, in: path, required: true, schema: {type: string, format: uuid}}]
      responses: {"200": {description: OK}}

  /v1/passports:
    post:
      parameters: [{$ref: "#/components/parameters/IdempotencyKey"}]
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"201": {description: Created}}
    get:
      parameters: [{$ref: "#/components/parameters/Limit"},{$ref: "#/components/parameters/Cursor"}]
      responses: {"200": {description: OK, content: {application/json: {schema: {$ref: "#/components/schemas/CursorPage"}}}}}
  /v1/passports/{passport_id}:
    get:
      parameters: [{name: passport_id, in: path, required: true, schema: {type: string, format: uuid}}]
      responses: {"200": {description: OK}}
  /v1/passports/{passport_id}/versions:
    get:
      parameters:
        - {name: passport_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/Limit"}
        - {$ref: "#/components/parameters/Cursor"}
      responses: {"200": {description: OK, content: {application/json: {schema: {$ref: "#/components/schemas/CursorPage"}}}}}
  /v1/passports/{passport_id}/compile:
    post:
      parameters:
        - {name: passport_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/IdempotencyKey"}
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"202": {description: Accepted}}

  /v1/passport-versions/{passport_version_id}:
    get:
      parameters: [{name: passport_version_id, in: path, required: true, schema: {type: string, format: uuid}}]
      responses: {"200": {description: OK}}
  /v1/passport-versions/{passport_version_id}/payload:
    get:
      parameters: [{name: passport_version_id, in: path, required: true, schema: {type: string, format: uuid}}]
      responses: {"200": {description: OK, content: {application/json: {schema: {type: object, additionalProperties: true}}}}}
  /v1/passport-versions/{passport_version_id}/proof:
    get:
      parameters: [{name: passport_version_id, in: path, required: true, schema: {type: string, format: uuid}}]
      responses: {"200": {description: OK, content: {application/json: {schema: {type: object, additionalProperties: true}}}}}
  /v1/passport-versions/{passport_version_id}/receipt:
    get:
      parameters: [{name: passport_version_id, in: path, required: true, schema: {type: string, format: uuid}}]
      responses: {"200": {description: OK, content: {application/json: {schema: {type: object, additionalProperties: true}}}}}
  /v1/passport-versions/{passport_version_id}/qr.png:
    get:
      parameters: [{name: passport_version_id, in: path, required: true, schema: {type: string, format: uuid}}]
      responses: {"200": {description: OK, content: {image/png: {schema: {type: string, format: binary}}}}}
  /v1/passport-versions/{passport_version_id}/activate:
    post:
      parameters:
        - {name: passport_version_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/IdempotencyKey"}
      responses: {"200": {description: OK}}
  /v1/passport-versions/{passport_version_id}/revoke:
    post:
      parameters:
        - {name: passport_version_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/IdempotencyKey"}
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"200": {description: OK}}
  /v1/passport-versions/{passport_version_id}/replay:
    post:
      parameters:
        - {name: passport_version_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/IdempotencyKey"}
      responses:
        "200": {description: OK}
        "409": {description: Replay mismatch, content: {application/json: {schema: {$ref: "#/components/schemas/ErrorEnvelope"}}}}

  /v1/audit/events:
    get:
      parameters:
        - {$ref: "#/components/parameters/Limit"}
        - {$ref: "#/components/parameters/Cursor"}
      responses: {"200": {description: OK, content: {application/json: {schema: {$ref: "#/components/schemas/CursorPage"}}}}}

  /v1/webhook-endpoints:
    post:
      parameters: [{$ref: "#/components/parameters/IdempotencyKey"}]
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"201": {description: Created}}
    get:
      parameters: [{$ref: "#/components/parameters/Limit"},{$ref: "#/components/parameters/Cursor"}]
      responses: {"200": {description: OK, content: {application/json: {schema: {$ref: "#/components/schemas/CursorPage"}}}}}
  /v1/webhook-endpoints/{webhook_endpoint_id}:
    delete:
      parameters:
        - {name: webhook_endpoint_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/IdempotencyKey"}
      responses: {"200": {description: Deleted}}

  /v1/webhook-subscriptions:
    post:
      parameters: [{$ref: "#/components/parameters/IdempotencyKey"}]
      requestBody: {required: true, content: {application/json: {schema: {type: object}}}}
      responses: {"201": {description: Created}}
    get:
      parameters: [{$ref: "#/components/parameters/Limit"},{$ref: "#/components/parameters/Cursor"}]
      responses: {"200": {description: OK, content: {application/json: {schema: {$ref: "#/components/schemas/CursorPage"}}}}}
  /v1/webhook-subscriptions/{webhook_subscription_id}:
    delete:
      parameters:
        - {name: webhook_subscription_id, in: path, required: true, schema: {type: string, format: uuid}}
        - {$ref: "#/components/parameters/IdempotencyKey"}
      responses: {"200": {description: Deleted}}
```
### 10.6 Webhooks
- Event types: passport.version.signed | passport.version.active | passport.version.revoked | rules.version.published | document.version.validated
- Signature: `X-BPC-Signature: sha256=<hex(hmac_sha256(secret, body))>`.
Webhook payload schema (BPC-WEBHOOK-1):
```json
{
  "type": "object",
  "required": [
    "event_type",
    "event_id",
    "occurred_at",
    "tenant_id",
    "data"
  ],
  "properties": {
    "event_type": {
      "type": "string"
    },
    "event_id": {
      "type": "string",
      "format": "uuid"
    },
    "occurred_at": {
      "type": "string",
      "format": "date-time"
    },
    "tenant_id": {
      "type": "string",
      "format": "uuid"
    },
    "data": {
      "type": "object"
    }
  }
}
```
## 11. Observability
### 11.1 Logging (structured JSON, MUST)
- `level`: debug|info|warn|error
- debug: evaluator trace, SQL timings (niemals secrets)
- info: request/job lifecycle, publish/activate/replay success
- warn: retryable errors, rate limit, policy deny
- error: non-retryable, DB down, internal
Example log line:
```json
{
  "ts": "2025-12-28T12:34:56Z",
  "level": "info",
  "component": "api",
  "msg": "request completed",
  "correlation_id": "b3a4fefe-7d3f-4e88-9a0f-7d1c2fa0b4e4",
  "http": {
    "method": "GET",
    "path": "/v1/health/live",
    "status": 200,
    "duration_ms": 2
  }
}
```
### 11.2 Metrics (Prometheus, MUST)
| Metric | Type | Labels |
|---|---|---|
| bpc_http_requests_total | counter | method,path,status |
| bpc_http_request_duration_seconds | histogram | method,path |
| bpc_jobs_acquired_total | counter | type |
| bpc_jobs_completed_total | counter | type,status |
| bpc_job_duration_seconds | histogram | type |
| bpc_compiles_total | counter | result |
| bpc_compile_duration_seconds | histogram | result |
| bpc_proof_bytes | histogram | — |
| bpc_payload_bytes | histogram | — |
| bpc_receipt_bytes | histogram | — |
| bpc_db_pool_in_use | gauge | — |
| bpc_rate_limited_total | counter | — |
| bpc_webhook_deliveries_total | counter | status |
### 11.3 Health Endpoints (Schema)
- `/v1/health/live` → `{status:"OK"}`
- `/v1/health/ready` → `{status:"OK"}` oder 503 ErrorEnvelope
## 12. Deployment/Ops
### 12.1 Target
- Docker images; prod via Kubernetes Deployments (`bpc-api`, `bpc-worker`).
### 12.2 Secrets Management (MUST)
- Dev: `.env` (nicht committen).
- Prod: Kubernetes Secrets/Vault. REQUIRED: signing key + pepper + DB password.
### 12.3 Database Backups (MUST)
- Daily full backup (pg_dump) oder WAL archiving; retention >= 35 Tage; monatlicher restore drill.
## 13. Testing
### 13.1 Frameworks
- Unit: Hspec
- Property: Hedgehog
- Golden: tasty-golden
- Integration: docker-compose Postgres (4.5.2)
### 13.2 Coverage (MUST)
- bpc-core ≥ 90%, bpc-db ≥ 80%, bpc-worker ≥ 75%, bpc-api ≥ 70% (CI gate in Phase P4 optional).
### 13.3 Konkrete Property Tests (MUST, Signaturen + Kern-Assertion)
```haskell
prop_canonicalEncode_deterministic :: Hedgehog.Property
prop_canonicalEncode_deterministic = property $ do
  v <- forAll genJsonWithoutFloats
  canonicalEncode v === canonicalEncode v

prop_snapshot_seal_stable :: Hedgehog.Property
prop_snapshot_seal_stable = withDb property $ do
  s <- liftIO createReadySnapshotWithRandomFacts
  (h1,c1) <- liftIO sealAndFetch s
  (h2,c2) <- liftIO recomputeSealPure s
  h1 === h2
  c1 === c2
```
### 13.4 Golden Tests (MUST)
- Byte-exakter Vergleich von canonical JSON (payload/proof/receipt unsigned).
- Fixtures unter `packages/bpc-core/test/golden/fixtures/<name>/...`.
## 14. Tasks (granular, pro Phase, MUST)
### 14.1 Task Dependencies pro Phase (Mermaid)
```mermaid
flowchart LR
  P0T1[P0-T1]-->P0T2[P0-T2]-->P0T3[P0-T3]-->P0T4[P0-T4]
  P1T1[P1-T1]-->P1T2[P1-T2]-->P1T3[P1-T3]-->P1T4[P1-T4]
  P2T1[P2-T1]-->P2T2[P2-T2]-->P2T3[P2-T3]-->P2T4[P2-T4]
  P3T1[P3-T1]-->P3T2[P3-T2]-->P3T3[P3-T3]-->P3T4[P3-T4]
  P4T1[P4-T1]-->P4T2[P4-T2]-->P4T3[P4-T3]
```
### 14.2 Phase P0
##### P0-T1
- **Was:** Repo scaffold (Struktur 4.1) + Cabal multi-package + README quickstart.
- **Input:** Leeres repo.
- **Output:** Repo tree + cabal.project + package skeletons + README.
- **Abhängigkeiten:** —
- **Geschätzte Komplexität:** M
- **Akzeptanzkriterien:**
  - `cabal build all` läuft erfolgreich.
  - Repo tree matcht 4.1.
  - README Quickstart ist copy&paste ausführbar.
- **Testfälle:**
  - TC-P0-T1-01: cabal build all exit 0
  - TC-P0-T1-02: tree matcht Struktur
  - TC-P0-T1-03: README steps ok
- **Dateien die erstellt/geändert werden:**
  - `cabal.project`
  - `packages/*/*.cabal`
  - `README.md`
##### P0-T2
- **Was:** Tooling: Fourmolu+HLint + CI skeleton.
- **Input:** P0-T1.
- **Output:** fourmolu.yaml, hlint.yaml, ci.yml.
- **Abhängigkeiten:** P0-T1
- **Geschätzte Komplexität:** M
- **Akzeptanzkriterien:**
  - CI läuft auf PR und main.
  - Format und HLint sind gates.
  - CI cached cabal store.
- **Testfälle:**
  - TC-P0-T2-01: CI green
  - TC-P0-T2-02: format violation fails
  - TC-P0-T2-03: hlint violation fails
- **Dateien die erstellt/geändert werden:**
  - `fourmolu.yaml`
  - `hlint.yaml`
  - `.github/workflows/ci.yml`
##### P0-T3
- **Was:** Docker compose dev + dbmate migrations + seed.
- **Input:** P0-T2 + Docker.
- **Output:** docker-compose.yml + migrations/001..002 + scripts/migrate.sh/seed-dev.sh.
- **Abhängigkeiten:** P0-T2
- **Geschätzte Komplexität:** M
- **Akzeptanzkriterien:**
  - `docker compose up` startet Postgres/RabbitMQ.
  - `dbmate up` erstellt Schema 6.2.
  - Seed erstellt dev tenant+roles+permissions.
- **Testfälle:**
  - TC-P0-T3-01: compose up ok
  - TC-P0-T3-02: dbmate up ok
  - TC-P0-T3-03: seed ok
- **Dateien die erstellt/geändert werden:**
  - `docker-compose.yml`
  - `migrations/001_init.sql`
  - `migrations/002_seed_permissions_roles.sql`
  - `scripts/migrate.sh`
  - `scripts/seed-dev.sh`
##### P0-T4
- **Was:** Dockerfiles für api/worker + cd.yml (build on tag).
- **Input:** P0-T3.
- **Output:** Dockerfiles + cd.yml.
- **Abhängigkeiten:** P0-T3
- **Geschätzte Komplexität:** S
- **Akzeptanzkriterien:**
  - docker build api ok.
  - docker build worker ok.
  - cd job triggert auf tags v*.
- **Testfälle:**
  - TC-P0-T4-01: docker build api
  - TC-P0-T4-02: docker build worker
  - TC-P0-T4-03: cd triggers
- **Dateien die erstellt/geändert werden:**
  - `docker/Dockerfile.api`
  - `docker/Dockerfile.worker`
  - `.github/workflows/cd.yml`
### 14.3 Phase P1 (bpc-core)
##### P1-T1
- **Was:** Canonical JSON + Hash helpers (7.1 + 7.2).
- **Input:** P0 complete.
- **Output:** BPC.Core.CanonicalJson + BPC.Core.Hash + unit/property tests.
- **Abhängigkeiten:** P0-T4
- **Geschätzte Komplexität:** M
- **Akzeptanzkriterien:**
  - Keys sorted; floats rejected.
  - sha256Hex matches fixture in 7.2.
  - Property tests pass.
- **Testfälle:**
  - TC-P1-T1-01: cjson sort
  - TC-P1-T1-02: reject float
  - TC-P1-T1-03: hash fixture
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-core/src/BPC/Core/CanonicalJson.hs`
  - `packages/bpc-core/src/BPC/Core/Hash.hs`
  - `packages/bpc-core/test/*`
##### P1-T2
- **Was:** DSL Parser + Typecheck + topoSort.
- **Input:** P1-T1.
- **Output:** Parser/AST/Typecheck modules + tests; cycle detection.
- **Abhängigkeiten:** P1-T1
- **Geschätzte Komplexität:** L
- **Akzeptanzkriterien:**
  - DSL parses according to 8.1.
  - Type errors mapped to RULE_TYPE_ERROR.
  - Cycles mapped to RULE_CYCLE_DETECTED.
- **Testfälle:**
  - TC-P1-T2-01: parse DSL example
  - TC-P1-T2-02: unit mismatch
  - TC-P1-T2-03: cycle
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-core/src/BPC/Core/Rules/Parser.hs`
  - `packages/bpc-core/src/BPC/Core/Rules/Typecheck.hs`
##### P1-T3
- **Was:** Evaluator + Proof + Receipt sign helpers.
- **Input:** P1-T2.
- **Output:** compilePassportPure + verifyProof + buildReceiptUnsigned + sign/verify.
- **Abhängigkeiten:** P1-T2
- **Geschätzte Komplexität:** L
- **Akzeptanzkriterien:**
  - compilePassportPure is deterministic.
  - Proof verify returns Right ().
  - Receipt signature verifies.
- **Testfälle:**
  - TC-P1-T3-01: compile ok
  - TC-P1-T3-02: proof verify
  - TC-P1-T3-03: sign verify
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-core/src/BPC/Core/Eval.hs`
  - `packages/bpc-core/src/BPC/Core/Proof.hs`
  - `packages/bpc-core/src/BPC/Core/Receipt.hs`
##### P1-T4
- **Was:** Golden tests (byte-identisch) für payload/proof/receipt unsigned.
- **Input:** P1-T3.
- **Output:** tasty-golden fixtures + CI execution.
- **Abhängigkeiten:** P1-T3
- **Geschätzte Komplexität:** M
- **Akzeptanzkriterien:**
  - ≥3 fixtures exist.
  - Golden compares exact bytes.
  - Determinism across runs.
- **Testfälle:**
  - TC-P1-T4-01: golden minimal
  - TC-P1-T4-02: golden medium
  - TC-P1-T4-03: golden edge
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-core/test/golden/GoldenSpec.hs`
  - `packages/bpc-core/test/golden/fixtures/**`
### 14.4 Phase P2 (DB+API base+Worker loop)
##### P2-T1
- **Was:** bpc-db pool + repos: events/documents/facts/snapshots/jobs (tenant-safe).
- **Input:** P0 migrations.
- **Output:** Repo modules + integration tests.
- **Abhängigkeiten:** P0-T3
- **Geschätzte Komplexität:** L
- **Akzeptanzkriterien:**
  - Event append/verify works.
  - Documents store bytes + sha256.
  - Jobs leasing works (no double-run).
- **Testfälle:**
  - TC-P2-T1-01: event chain ok
  - TC-P2-T1-02: document bytes ok
  - TC-P2-T1-03: job lease ok
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-db/src/BPC/DB/Pool.hs`
  - `packages/bpc-db/src/BPC/DB/Repos/*.hs`
  - `packages/bpc-db/test/integration/**`
##### P2-T2
- **Was:** bpc-api: auth middleware + docs/snapshots/facts endpoints + error envelope.
- **Input:** P2-T1.
- **Output:** API server exposes endpoints; RBAC enforced.
- **Abhängigkeiten:** P2-T1
- **Geschätzte Komplexität:** L
- **Akzeptanzkriterien:**
  - 401/403 mapping correct.
  - Cursor pagination works.
  - Idempotency enforced on POST.
- **Testfälle:**
  - TC-P2-T2-01: unauthorized 401
  - TC-P2-T2-02: forbidden 403
  - TC-P2-T2-03: idempotency conflict 409
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-api/src/BPC/API/Main.hs`
  - `packages/bpc-api/src/BPC/API/Middleware/Auth.hs`
  - `packages/bpc-api/src/BPC/API/Handlers/*.hs`
##### P2-T3
- **Was:** Worker loop + PARSE_FACTS handler + snapshot seal.
- **Input:** P2-T1 + P1 canonical/hash.
- **Output:** Worker executes jobs; parse inserts facts; seal computes snapshot_hash.
- **Abhängigkeiten:** P2-T1, P1-T1
- **Geschätzte Komplexität:** L
- **Akzeptanzkriterien:**
  - Valid docs => DocumentVersion VALIDATED.
  - Facts canonical+hash stored.
  - Seal matches BPC-SNAPSHOT-1 exactly.
- **Testfälle:**
  - TC-P2-T3-01: parse ok
  - TC-P2-T3-02: fact hashing ok
  - TC-P2-T3-03: seal ok
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-worker/src/BPC/Worker/Runner.hs`
  - `packages/bpc-worker/src/BPC/Worker/Handlers/ParseFacts.hs`
  - `packages/bpc-db/src/BPC/DB/Repos/Snapshots.hs`
##### P2-T4
- **Was:** Observability base: correlation id, JSON logs, /metrics.
- **Input:** P2-T2,P2-T3.
- **Output:** X-Correlation-Id; structured logs; metrics endpoint.
- **Abhängigkeiten:** P2-T2, P2-T3
- **Geschätzte Komplexität:** M
- **Akzeptanzkriterien:**
  - Correlation id always present.
  - Logs are JSON when enabled.
  - /v1/metrics exposes expected names.
- **Testfälle:**
  - TC-P2-T4-01: header present
  - TC-P2-T4-02: log contains correlation_id
  - TC-P2-T4-03: metrics contains bpc_http_requests_total
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-api/src/BPC/API/Middleware/CorrelationId.hs`
  - `packages/bpc-api/src/BPC/API/Handlers/Metrics.hs`
### 14.5 Phase P3 (Compile→Sign→QR→Activate→Replay)
##### P3-T1
- **Was:** Passports domain: repos + API endpoints + compile job enqueue.
- **Input:** P2-T2.
- **Output:** POST /battery-products, /passports, /passports/{id}/compile.
- **Abhängigkeiten:** P2-T2
- **Geschätzte Komplexität:** L
- **Akzeptanzkriterien:**
  - Creates product+passport rows.
  - Compile enqueues COMPILE_PASSPORT job.
  - Permissions enforced.
- **Testfälle:**
  - TC-P3-T1-01: create product
  - TC-P3-T1-02: create passport
  - TC-P3-T1-03: enqueue compile returns 202
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-db/src/BPC/DB/Repos/Passports.hs`
  - `packages/bpc-api/src/BPC/API/Handlers/Passports.hs`
##### P3-T2
- **Was:** Worker handlers: COMPILE_PASSPORT + SIGN_PASSPORT + GENERATE_QR.
- **Input:** P3-T1 + P1 compile/sign/qr + P2 worker.
- **Output:** Pipeline yields SIGNED version with qr_png.
- **Abhängigkeiten:** P3-T1, P1-T3, P2-T3
- **Geschätzte Komplexität:** L
- **Akzeptanzkriterien:**
  - Compile uses SEALED snapshot + published rules.
  - Signature verifies.
  - QR payload matches BPC-QR-1.
- **Testfälle:**
  - TC-P3-T2-01: compile ok
  - TC-P3-T2-02: sign ok
  - TC-P3-T2-03: qr ok
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-worker/src/BPC/Worker/Handlers/CompilePassport.hs`
  - `packages/bpc-worker/src/BPC/Worker/Handlers/SignPassport.hs`
  - `packages/bpc-worker/src/BPC/Worker/Handlers/GenerateQR.hs`
##### P3-T3
- **Was:** Activate + Replay endpoints.
- **Input:** P3-T2.
- **Output:** POST /passport-versions/{id}/activate + /replay.
- **Abhängigkeiten:** P3-T2
- **Geschätzte Komplexität:** M
- **Akzeptanzkriterien:**
  - Activate only from SIGNED.
  - Supersedes prior ACTIVE.
  - Replay verifies hashes+signature.
- **Testfälle:**
  - TC-P3-T3-01: activate ok
  - TC-P3-T3-02: supersede ok
  - TC-P3-T3-03: replay ok
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-api/src/BPC/API/Handlers/Replay.hs`
  - `packages/bpc-api/src/BPC/API/Handlers/Passports.hs`
##### P3-T4
- **Was:** E2E integration tests (upload→active→replay).
- **Input:** P3-T3.
- **Output:** CI integration suite passes using docker-compose.test.yml.
- **Abhängigkeiten:** P3-T3
- **Geschätzte Komplexität:** L
- **Akzeptanzkriterien:**
  - Happy path passes.
  - Replay OK at end.
  - SEALED snapshot immutable.
- **Testfälle:**
  - TC-P3-T4-01: e2e ok
  - TC-P3-T4-02: replay ok
  - TC-P3-T4-03: sealed immutable
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-api/test/integration/E2ESpec.hs`
  - `packages/bpc-worker/test/integration/WorkerE2ESpec.hs`
### 14.6 Phase P4 (Policies/Webhooks/RateLimit/Retention)
##### P4-T1
- **Was:** Policies in auth middleware (deny/allow) + CRUD minimal.
- **Input:** P2-T2 + migration 003.
- **Output:** Policy evaluation active.
- **Abhängigkeiten:** P2-T2
- **Geschätzte Komplexität:** L
- **Akzeptanzkriterien:**
  - Deny policy blocks access even with permission.
  - Priority order enforced.
  - policy_hash stable (canonical).
- **Testfälle:**
  - TC-P4-T1-01: deny
  - TC-P4-T1-02: priority
  - TC-P4-T1-03: hash stable
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-api/src/BPC/API/Middleware/Auth.hs`
  - `packages/bpc-db/src/BPC/DB/Repos/Auth.hs`
##### P4-T2
- **Was:** Rate limiting middleware (BPC-RL-1) backed by rate_limit_buckets.
- **Input:** P2-T4 + migration 004.
- **Output:** 429 + headers + DB state.
- **Abhängigkeiten:** P2-T4
- **Geschätzte Komplexität:** M
- **Akzeptanzkriterien:**
  - Limiter disabled => no-op.
  - Limiter enabled => burst enforced.
  - Separate keys independent.
- **Testfälle:**
  - TC-P4-T2-01: burst 429
  - TC-P4-T2-02: refill ok
  - TC-P4-T2-03: independent keys
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-api/src/BPC/API/Middleware/RateLimit.hs`
  - `packages/bpc-db/src/BPC/DB/Repos/RateLimit.hs`
##### P4-T3
- **Was:** Webhooks delivery job with HMAC signature + retries.
- **Input:** P3-T3 + migration 003.
- **Output:** Webhook endpoints/subscriptions/deliveries + DELIVER_WEBHOOK handler.
- **Abhängigkeiten:** P3-T3
- **Geschätzte Komplexität:** L
- **Akzeptanzkriterien:**
  - Signature header valid.
  - Retries with backoff.
  - Delivery status persisted.
- **Testfälle:**
  - TC-P4-T3-01: deliver ok
  - TC-P4-T3-02: retries
  - TC-P4-T3-03: status persisted
- **Dateien die erstellt/geändert werden:**
  - `packages/bpc-worker/src/BPC/Worker/Handlers/DeliverWebhook.hs`
  - `packages/bpc-db/src/BPC/DB/Repos/Webhooks.hs`
### 14.7 Permissions Matrix (normativ)
| Permission | Admin | ComplianceOfficer | Auditor |
|---|:---:|:---:|:---:|
| passport:read | ✅ | ✅ | ✅ |
| passport:compile | ✅ | ✅ | ❌ |
| passport:sign | ✅ | ❌ | ❌ |
| passport:replay | ✅ | ✅ | ❌ |
| passport:revoke | ✅ | ❌ | ❌ |
| rules:read | ✅ | ✅ | ✅ |
| rules:write | ✅ | ❌ | ❌ |
| rules:publish | ✅ | ❌ | ❌ |
| rules:test | ✅ | ❌ | ❌ |
| docs:upload | ✅ | ❌ | ❌ |
| docs:read | ✅ | ✅ | ❌ |
| snapshot:build | ✅ | ✅ | ❌ |
| snapshot:seal | ✅ | ✅ | ❌ |
| audit:read | ✅ | ❌ | ✅ |
| access:admin | ✅ | ❌ | ❌ |
| tenant:admin | ✅ | ❌ | ❌ |
### 14.8 Mermaid: Dataflow + State Machines (MUST)
```mermaid
flowchart LR
  U[Upload DocumentVersion]-->P[PARSE_FACTS]-->F[Facts]
  F-->S[Snapshot BUILDING/READY]-->SS[Seal SEALED]
  SS-->C[COMPILE_PASSPORT]-->SIG[SIGN_PASSPORT]-->QR[GENERATE_QR]-->A[Activate]-->R[Replay]
```
```mermaid
stateDiagram-v2
  [*]-->COMPILING-->SIGNED-->ACTIVE-->SUPERSEDED
  ACTIVE-->REVOKED
  SIGNED-->REVOKED
  SUPERSEDED-->REVOKED
```
