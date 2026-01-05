# Data Model: Foundation & Infrastructure

**Feature**: 01-foundation
**Date**: 2025-12-28
**SSOT Reference**: Section 6.2-6.5

## Overview

This document describes the complete database schema established by the foundation migrations. All DDL is normatively defined in SSOT Section 6.

## Entity Relationship Diagram

```
tenants
    │
    ├─< actors ─< api_keys
    │       │
    │       └─< actor_roles >── roles ─< role_permissions >── permissions
    │
    ├─< events
    │
    ├─< documents ─< document_versions ─< facts
    │
    ├─< data_snapshots ─< snapshot_items >── facts
    │
    ├─< battery_products ─< passports ─< passport_versions
    │                                          │
    │                                          └── data_snapshots
    │                                          └── rule_package_versions
    │
    ├─< rule_packages ─< rule_package_versions ─< rule_fields
    │                          │
    │                          └─< rule_tests_runs
    │
    ├─< jobs
    │
    ├─< idempotency_keys
    │
    ├─< policies ─< policy_versions
    │
    ├─< webhook_endpoints ─< webhook_subscriptions
    │         │
    │         └─< webhook_deliveries >── events
    │
    └─< rate_limit_buckets
```

## Core Entities

### Tenants (Multi-Tenancy Root)

```sql
CREATE TABLE tenants (
  tenant_id uuid PRIMARY KEY,
  slug text NOT NULL UNIQUE CHECK (slug ~ '^[a-z0-9-]{3,64}$'),
  name text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now()
);
```

**Purpose**: Root entity for multi-tenant isolation. All data is scoped by tenant_id.

### Actors & Authentication

```sql
CREATE TABLE actors (
  actor_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  type actor_type NOT NULL,  -- USER | API_CLIENT | SERVICE
  display_name text NOT NULL,
  email text NULL,
  is_active boolean NOT NULL DEFAULT true,
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE api_keys (
  api_key_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  actor_id uuid NOT NULL REFERENCES actors(actor_id),
  key_prefix text NOT NULL,
  key_hash bytea NOT NULL,
  label text NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  revoked_at timestamptz NULL
);
```

**Purpose**: Identity management. API keys store SHA-256(key+pepper), never plaintext.

### RBAC

```sql
CREATE TABLE roles (
  role_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  name text NOT NULL,
  UNIQUE(tenant_id, name)
);

CREATE TABLE permissions (
  permission text PRIMARY KEY
);

CREATE TABLE role_permissions (
  role_id uuid NOT NULL REFERENCES roles(role_id),
  permission text NOT NULL REFERENCES permissions(permission),
  PRIMARY KEY(role_id, permission)
);

CREATE TABLE actor_roles (
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  actor_id uuid NOT NULL REFERENCES actors(actor_id),
  role_id uuid NOT NULL REFERENCES roles(role_id),
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(tenant_id, actor_id, role_id)
);
```

**Purpose**: Role-based access control. 16 permissions defined in SSOT.

### Event Store

```sql
CREATE TABLE events (
  event_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  aggregate_type text NOT NULL,
  aggregate_id uuid NOT NULL,
  aggregate_version bigint NOT NULL CHECK (aggregate_version >= 1),
  event_type text NOT NULL,
  occurred_at timestamptz NOT NULL DEFAULT now(),
  actor_id uuid NULL REFERENCES actors(actor_id),
  payload jsonb NOT NULL,
  payload_canonical bytea NOT NULL,
  payload_hash bytea NOT NULL,
  prev_event_hash bytea NULL,
  event_hash bytea NOT NULL,
  UNIQUE(tenant_id, aggregate_type, aggregate_id, aggregate_version)
);
```

**Purpose**: Append-only audit log with hash chain for tamper detection.

## Document Pipeline Entities

### Documents & Versions

```sql
CREATE TABLE documents (
  document_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  kind document_kind NOT NULL,  -- BOM | PCF | DUE_DILIGENCE | OTHER
  external_ref text NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  created_by uuid NULL REFERENCES actors(actor_id)
);

CREATE TABLE document_versions (
  document_version_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  document_id uuid NOT NULL REFERENCES documents(document_id),
  version int NOT NULL CHECK (version >= 1),
  status document_status NOT NULL,  -- UPLOADED | PARSED | VALIDATED | REJECTED
  mime_type text NOT NULL,
  sha256 bytea NOT NULL,
  content bytea NOT NULL,
  uploaded_at timestamptz NOT NULL DEFAULT now()
);
```

### Facts

```sql
CREATE TABLE facts (
  fact_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  fact_type text NOT NULL,
  fact_key text NOT NULL,
  schema_version int NOT NULL CHECK (schema_version >= 1),
  source_document_version_id uuid NOT NULL REFERENCES document_versions(document_version_id),
  payload jsonb NOT NULL,
  payload_canonical bytea NOT NULL,
  payload_hash bytea NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(tenant_id, fact_type, fact_key, schema_version, payload_hash)
);
```

### Snapshots

```sql
CREATE TABLE data_snapshots (
  snapshot_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  status snapshot_status NOT NULL,  -- BUILDING | READY | SEALED
  label text NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  sealed_at timestamptz NULL,
  created_by uuid NULL REFERENCES actors(actor_id),
  snapshot_canonical bytea NULL,
  snapshot_hash bytea NULL
);

CREATE TABLE snapshot_items (
  snapshot_id uuid NOT NULL REFERENCES data_snapshots(snapshot_id),
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  fact_id uuid NOT NULL REFERENCES facts(fact_id),
  PRIMARY KEY(snapshot_id, fact_id)
);
```

## Passport Entities

### Battery Products

```sql
CREATE TABLE battery_products (
  battery_product_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  sku text NOT NULL,
  name text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(tenant_id, sku)
);
```

### Passports & Versions

```sql
CREATE TABLE passports (
  passport_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  battery_product_id uuid NOT NULL REFERENCES battery_products(battery_product_id),
  current_passport_version_id uuid NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(tenant_id, battery_product_id)
);

CREATE TABLE passport_versions (
  passport_version_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  passport_id uuid NOT NULL REFERENCES passports(passport_id),
  status passport_status NOT NULL,  -- COMPILING | SIGNED | ACTIVE | SUPERSEDED | REVOKED
  created_at timestamptz NOT NULL DEFAULT now(),
  activated_at timestamptz NULL,
  superseded_at timestamptz NULL,
  revoked_at timestamptz NULL,
  snapshot_id uuid NOT NULL REFERENCES data_snapshots(snapshot_id),
  rule_package_version_id uuid NOT NULL REFERENCES rule_package_versions(rule_package_version_id),
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
  qr_payload text NULL
);
```

## Rule Entities

### Rule Packages & Versions

```sql
CREATE TABLE rule_packages (
  rule_package_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  name text NOT NULL,
  description text NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  created_by uuid NULL REFERENCES actors(actor_id),
  UNIQUE(tenant_id, name)
);

CREATE TABLE rule_package_versions (
  rule_package_version_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  rule_package_id uuid NOT NULL REFERENCES rule_packages(rule_package_id),
  version int NOT NULL CHECK (version >= 1),
  status rule_pkg_status NOT NULL,  -- DRAFT | VALIDATED | PUBLISHED | DEPRECATED | RETIRED
  created_at timestamptz NOT NULL DEFAULT now(),
  published_at timestamptz NULL,
  compiler_min_build text NOT NULL,
  dsl_source bytea NOT NULL,
  dsl_sha256 bytea NOT NULL,
  tests_source bytea NOT NULL,
  tests_sha256 bytea NOT NULL
);
```

## Job Queue

```sql
CREATE TABLE jobs (
  job_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  type job_type NOT NULL,
  status job_status NOT NULL,  -- QUEUED | RUNNING | SUCCEEDED | FAILED | CANCELLED | DEAD_LETTER
  priority int NOT NULL DEFAULT 100,
  attempts int NOT NULL DEFAULT 0,
  max_attempts int NOT NULL DEFAULT 10,
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
```

## Seeded Data (Migration 002)

### Permissions (16 total)

| Permission | Description |
|------------|-------------|
| passport:read | Read passport data |
| passport:compile | Trigger compilation |
| passport:sign | Sign passport |
| passport:replay | Verify/replay passport |
| passport:revoke | Revoke passport |
| rules:read | Read rules |
| rules:write | Create/edit rules |
| rules:publish | Publish rules |
| rules:test | Run rule tests |
| docs:upload | Upload documents |
| docs:read | Read documents |
| snapshot:build | Create/edit snapshots |
| snapshot:seal | Seal snapshots |
| audit:read | Read audit events |
| access:admin | Manage access |
| tenant:admin | Full tenant admin |

### Default Roles

| Role | Permissions |
|------|-------------|
| Admin | All 16 |
| ComplianceOfficer | passport:read, passport:compile, passport:replay, rules:read, docs:read, snapshot:build, snapshot:seal |
| Auditor | passport:read, rules:read, audit:read |

## Status Enums Summary

| Enum | Values |
|------|--------|
| actor_type | USER, API_CLIENT, SERVICE |
| job_type | INGEST_DOCUMENT, PARSE_FACTS, BUILD_SNAPSHOT, COMPILE_PASSPORT, RUN_RULE_TESTS, SIGN_PASSPORT, GENERATE_QR, EXPORT_PASSPORT, DELIVER_WEBHOOK |
| job_status | QUEUED, RUNNING, SUCCEEDED, FAILED, CANCELLED, DEAD_LETTER |
| document_kind | BOM, PCF, DUE_DILIGENCE, OTHER |
| document_status | UPLOADED, PARSED, VALIDATED, REJECTED |
| snapshot_status | BUILDING, READY, SEALED |
| rule_pkg_status | DRAFT, VALIDATED, PUBLISHED, DEPRECATED, RETIRED |
| passport_status | COMPILING, SIGNED, ACTIVE, SUPERSEDED, REVOKED |
| access_decision | ALLOW, DENY |
