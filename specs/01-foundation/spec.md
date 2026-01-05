# Feature Specification: Foundation & Infrastructure

**Feature Branch**: `01-foundation`
**Created**: 2025-12-28
**Status**: Draft
**SSOT Reference**: Sektion 4 (Setup & Tooling), 6.2-6.5 (Migrations)
**Phase**: P0

## User Scenarios & Testing

### User Story 1 - Developer Environment Setup (Priority: P1)

Als Entwickler muss ich das Repository klonen und mit wenigen Befehlen eine funktionierende Entwicklungsumgebung haben.

**Why this priority**: Ohne funktionierende Entwicklungsumgebung kann keine weitere Arbeit stattfinden. Dies ist die absolute Basis.

**Independent Test**: Repository klonen, `docker compose up -d && ./scripts/migrate.sh && cabal build all` ausführen - alles muss ohne Fehler durchlaufen.

**Acceptance Scenarios**:

1. **Given** frisches Repository-Klon, **When** `docker compose up -d` ausgeführt wird, **Then** starten Postgres und RabbitMQ Container erfolgreich
2. **Given** laufende Docker-Container, **When** `./scripts/migrate.sh` ausgeführt wird, **Then** wird das komplette DB-Schema erstellt
3. **Given** migrierte Datenbank, **When** `cabal build all` ausgeführt wird, **Then** kompilieren alle 5 Packages ohne Fehler

---

### User Story 2 - Code Quality Gates (Priority: P1)

Als Entwickler muss ich sicherstellen, dass mein Code den Projektstandards entspricht bevor ich committe.

**Why this priority**: Konsistente Code-Qualität ist essentiell für Wartbarkeit und Team-Zusammenarbeit.

**Independent Test**: Format- und Lint-Checks lokal ausführen können.

**Acceptance Scenarios**:

1. **Given** Haskell-Datei mit falscher Formatierung, **When** `fourmolu -m check` ausgeführt wird, **Then** schlägt der Check fehl mit klarer Fehlermeldung
2. **Given** Haskell-Datei mit HLint-Warnung, **When** `hlint -h hlint.yaml` ausgeführt wird, **Then** werden Verbesserungsvorschläge angezeigt
3. **Given** korrekt formatierter Code, **When** beide Checks laufen, **Then** passieren sie erfolgreich

---

### User Story 3 - CI Pipeline (Priority: P2)

Als Entwickler erwarte ich, dass Pull Requests automatisch auf Qualität geprüft werden.

**Why this priority**: Automatisierte Checks verhindern Regressionen und sichern Qualitätsstandards.

**Independent Test**: PR erstellen und CI-Pipeline beobachten.

**Acceptance Scenarios**:

1. **Given** PR auf main Branch, **When** CI Pipeline läuft, **Then** werden Format, Lint, Build und Tests ausgeführt
2. **Given** fehlgeschlagener Test, **When** CI Pipeline abgeschlossen ist, **Then** wird PR als "failing" markiert
3. **Given** alle Checks bestanden, **When** CI Pipeline abgeschlossen ist, **Then** wird PR als "passing" markiert

---

### User Story 4 - Database Seeding (Priority: P2)

Als Entwickler benötige ich vorkonfigurierte Testdaten für lokale Entwicklung.

**Why this priority**: Ohne Seed-Daten (Tenant, Rollen, Permissions) können API-Tests nicht durchgeführt werden.

**Independent Test**: `./scripts/seed-dev.sh` ausführen und verifizieren, dass dev Tenant mit Rollen existiert.

**Acceptance Scenarios**:

1. **Given** leere Datenbank nach Migration, **When** `./scripts/seed-dev.sh` ausgeführt wird, **Then** existiert ein `dev` Tenant
2. **Given** geseedete Datenbank, **When** Rollen abgefragt werden, **Then** existieren Admin, ComplianceOfficer, Auditor Rollen
3. **Given** geseedete Datenbank, **When** Permissions abgefragt werden, **Then** existieren alle 16 definierten Permissions

---

### User Story 5 - Docker Image Build (Priority: P3)

Als DevOps-Ingenieur muss ich Docker-Images für Produktion bauen können.

**Why this priority**: Deployment-Readiness ist wichtig, aber nicht für initiale Entwicklung kritisch.

**Independent Test**: `docker build -f docker/Dockerfile.api .` erfolgreich ausführen.

**Acceptance Scenarios**:

1. **Given** vollständiges Repository, **When** `docker build -f docker/Dockerfile.api .` ausgeführt wird, **Then** wird ein funktionierendes Image erstellt
2. **Given** vollständiges Repository, **When** `docker build -f docker/Dockerfile.worker .` ausgeführt wird, **Then** wird ein funktionierendes Image erstellt
3. **Given** Git-Tag `v*`, **When** CD Pipeline triggert, **Then** werden Images nach GHCR gepusht

---

### Edge Cases

- Was passiert wenn Postgres-Container nicht startet? → docker-compose logs prüfen
- Was passiert bei fehlenden GHC/Cabal Versionen? → README dokumentiert Voraussetzungen
- Wie verhält sich Migration bei bereits existierendem Schema? → dbmate ist idempotent

## Requirements

### Functional Requirements

- **FR-001**: System MUSS Cabal multi-package Struktur mit 5 Packages unterstützen (bpc-core, bpc-db, bpc-api, bpc-worker, bpc-cli) [SSOT 3.2]
- **FR-002**: System MUSS mit GHC 9.6.4 und Cabal 3.10.2.1 kompilieren [SSOT 4.2]
- **FR-003**: System MUSS `-Wall -Wcompat -Werror` Compiler-Flags verwenden [SSOT 4.2.1]
- **FR-004**: System MUSS Fourmolu Formatierung erzwingen [SSOT 4.3.2]
- **FR-005**: System MUSS HLint Regeln aus hlint.yaml anwenden [SSOT 4.3.3]
- **FR-006**: CI MUSS auf Push zu main und Pull Requests triggern [SSOT 4.4.1]
- **FR-007**: CI MUSS Postgres 16 Service für Integration Tests bereitstellen [SSOT 4.4.1]
- **FR-008**: Docker Compose MUSS Postgres und RabbitMQ für Entwicklung bereitstellen [SSOT 4.5.1]
- **FR-009**: Migrations MÜSSEN via **dbmate** ausgeführt werden (deploy order: migrate → api → worker) [SSOT 6.1]
- **FR-010**: Migration 001_init.sql MUSS alle Tabellen gemäß SSOT 6.2 erstellen
- **FR-011**: Migration 002 MUSS Permissions und Rollen seeden [SSOT 6.3]
- **FR-012**: CD MUSS auf Git-Tags v* triggern und Images zu GHCR pushen [SSOT 4.4.2]

### Key Entities

- **Tenant**: Mandant mit isoliertem Daten-/Namensraum
- **Actor**: Identität (USER/API_CLIENT/SERVICE)
- **Role**: Tenant-scoped Bündel von Permissions
- **Permission**: String der eine Aktion autorisiert (z.B. passport:read)

### Repository Structure (SSOT 4.1)

```
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
├── .github/workflows/
│   ├── ci.yml
│   └── cd.yml
├── docker/
│   ├── Dockerfile.api
│   ├── Dockerfile.worker
│   ├── Dockerfile.cli
│   └── entrypoint.sh
├── docker-compose.yml
├── docker-compose.test.yml
├── migrations/
│   ├── 001_init.sql
│   ├── 002_seed_permissions_roles.sql
│   ├── 003_add_policies_webhooks.sql
│   └── 004_add_rate_limits.sql
├── scripts/
│   ├── dev-up.sh
│   ├── dev-down.sh
│   ├── migrate.sh
│   ├── seed-dev.sh
│   ├── run-integration-tests.sh
│   └── gen-openapi.sh
└── packages/
    ├── bpc-core/
    ├── bpc-db/
    ├── bpc-api/
    ├── bpc-worker/
    └── bpc-cli/
```

## Success Criteria

### Measurable Outcomes

- **SC-001**: `cabal build all` kompiliert ohne Fehler in < 5 Minuten (frischer Build)
- **SC-002**: CI Pipeline läuft vollständig in < 10 Minuten
- **SC-003**: `docker compose up -d && ./scripts/migrate.sh && ./scripts/seed-dev.sh` funktioniert in < 2 Minuten
- **SC-004**: Alle Tooling-Dateien (fourmolu.yaml, hlint.yaml, etc.) sind identisch zu SSOT-Vorgaben
