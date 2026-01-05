-- migrate:up
-- BPC Database Schema - Migration 001: Core Schema
-- SSOT Reference: Section 6.2

-- ============================================================================
-- ENUM TYPES
-- ============================================================================

CREATE TYPE actor_type AS ENUM ('USER', 'API_CLIENT', 'SERVICE');

CREATE TYPE job_type AS ENUM (
  'INGEST_DOCUMENT',
  'PARSE_FACTS',
  'BUILD_SNAPSHOT',
  'COMPILE_PASSPORT',
  'RUN_RULE_TESTS',
  'SIGN_PASSPORT',
  'GENERATE_QR',
  'EXPORT_PASSPORT',
  'DELIVER_WEBHOOK'
);

CREATE TYPE job_status AS ENUM (
  'QUEUED',
  'RUNNING',
  'SUCCEEDED',
  'FAILED',
  'CANCELLED',
  'DEAD_LETTER'
);

CREATE TYPE document_kind AS ENUM ('BOM', 'PCF', 'DUE_DILIGENCE', 'OTHER');

CREATE TYPE document_status AS ENUM ('UPLOADED', 'PARSED', 'VALIDATED', 'REJECTED');

CREATE TYPE snapshot_status AS ENUM ('BUILDING', 'READY', 'SEALED');

CREATE TYPE rule_pkg_status AS ENUM ('DRAFT', 'VALIDATED', 'PUBLISHED', 'DEPRECATED', 'RETIRED');

CREATE TYPE passport_status AS ENUM ('COMPILING', 'SIGNED', 'ACTIVE', 'SUPERSEDED', 'REVOKED');

CREATE TYPE access_decision AS ENUM ('ALLOW', 'DENY');

-- ============================================================================
-- TENANTS (Multi-Tenancy Root)
-- ============================================================================

CREATE TABLE tenants (
  tenant_id uuid PRIMARY KEY,
  slug text NOT NULL UNIQUE CHECK (slug ~ '^[a-z0-9-]{3,64}$'),
  name text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now()
);

-- ============================================================================
-- ACTORS & AUTHENTICATION
-- ============================================================================

CREATE TABLE actors (
  actor_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  type actor_type NOT NULL,
  display_name text NOT NULL,
  email text NULL,
  is_active boolean NOT NULL DEFAULT true,
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX idx_actors_tenant ON actors(tenant_id);

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

CREATE INDEX idx_api_keys_tenant ON api_keys(tenant_id);
CREATE INDEX idx_api_keys_prefix ON api_keys(key_prefix);

-- ============================================================================
-- RBAC (Roles and Permissions)
-- ============================================================================

CREATE TABLE roles (
  role_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  name text NOT NULL,
  UNIQUE(tenant_id, name)
);

CREATE INDEX idx_roles_tenant ON roles(tenant_id);

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

-- ============================================================================
-- IDEMPOTENCY
-- ============================================================================

CREATE TABLE idempotency_keys (
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  key text NOT NULL,
  response_status int NOT NULL,
  response_body bytea NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  expires_at timestamptz NOT NULL,
  PRIMARY KEY(tenant_id, key)
);

CREATE INDEX idx_idempotency_expires ON idempotency_keys(expires_at);

-- ============================================================================
-- EVENT STORE (Audit Trail with Hash Chain)
-- ============================================================================

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

CREATE INDEX idx_events_tenant ON events(tenant_id);
CREATE INDEX idx_events_aggregate ON events(tenant_id, aggregate_type, aggregate_id);
CREATE INDEX idx_events_occurred ON events(tenant_id, occurred_at);

-- ============================================================================
-- DOCUMENTS & VERSIONS
-- ============================================================================

CREATE TABLE documents (
  document_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  kind document_kind NOT NULL,
  external_ref text NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  created_by uuid NULL REFERENCES actors(actor_id)
);

CREATE INDEX idx_documents_tenant ON documents(tenant_id);

CREATE TABLE document_versions (
  document_version_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  document_id uuid NOT NULL REFERENCES documents(document_id),
  version int NOT NULL CHECK (version >= 1),
  status document_status NOT NULL,
  mime_type text NOT NULL,
  sha256 bytea NOT NULL,
  content bytea NOT NULL,
  uploaded_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(document_id, version)
);

CREATE INDEX idx_document_versions_tenant ON document_versions(tenant_id);
CREATE INDEX idx_document_versions_document ON document_versions(document_id);

-- ============================================================================
-- FACTS
-- ============================================================================

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

CREATE INDEX idx_facts_tenant ON facts(tenant_id);
CREATE INDEX idx_facts_type_key ON facts(tenant_id, fact_type, fact_key);

-- ============================================================================
-- DATA SNAPSHOTS
-- ============================================================================

CREATE TABLE data_snapshots (
  snapshot_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  status snapshot_status NOT NULL,
  label text NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  sealed_at timestamptz NULL,
  created_by uuid NULL REFERENCES actors(actor_id),
  snapshot_canonical bytea NULL,
  snapshot_hash bytea NULL
);

CREATE INDEX idx_snapshots_tenant ON data_snapshots(tenant_id);
CREATE INDEX idx_snapshots_status ON data_snapshots(tenant_id, status);

CREATE TABLE snapshot_items (
  snapshot_id uuid NOT NULL REFERENCES data_snapshots(snapshot_id),
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  fact_id uuid NOT NULL REFERENCES facts(fact_id),
  PRIMARY KEY(snapshot_id, fact_id)
);

CREATE INDEX idx_snapshot_items_tenant ON snapshot_items(tenant_id);

-- ============================================================================
-- BATTERY PRODUCTS & PASSPORTS
-- ============================================================================

CREATE TABLE battery_products (
  battery_product_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  sku text NOT NULL,
  name text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(tenant_id, sku)
);

CREATE INDEX idx_battery_products_tenant ON battery_products(tenant_id);

CREATE TABLE passports (
  passport_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  battery_product_id uuid NOT NULL REFERENCES battery_products(battery_product_id),
  current_passport_version_id uuid NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(tenant_id, battery_product_id)
);

CREATE INDEX idx_passports_tenant ON passports(tenant_id);

-- ============================================================================
-- RULE PACKAGES & VERSIONS
-- ============================================================================

CREATE TABLE rule_packages (
  rule_package_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  name text NOT NULL,
  description text NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  created_by uuid NULL REFERENCES actors(actor_id),
  UNIQUE(tenant_id, name)
);

CREATE INDEX idx_rule_packages_tenant ON rule_packages(tenant_id);

CREATE TABLE rule_package_versions (
  rule_package_version_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  rule_package_id uuid NOT NULL REFERENCES rule_packages(rule_package_id),
  version int NOT NULL CHECK (version >= 1),
  status rule_pkg_status NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  published_at timestamptz NULL,
  compiler_min_build text NOT NULL,
  dsl_source bytea NOT NULL,
  dsl_sha256 bytea NOT NULL,
  tests_source bytea NOT NULL,
  tests_sha256 bytea NOT NULL,
  UNIQUE(rule_package_id, version)
);

CREATE INDEX idx_rule_package_versions_tenant ON rule_package_versions(tenant_id);
CREATE INDEX idx_rule_package_versions_package ON rule_package_versions(rule_package_id);

CREATE TABLE rule_fields (
  rule_field_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  rule_package_version_id uuid NOT NULL REFERENCES rule_package_versions(rule_package_version_id),
  field_path text NOT NULL,
  field_type text NOT NULL,
  description text NULL,
  UNIQUE(rule_package_version_id, field_path)
);

CREATE INDEX idx_rule_fields_tenant ON rule_fields(tenant_id);
CREATE INDEX idx_rule_fields_version ON rule_fields(rule_package_version_id);

CREATE TABLE rule_tests_runs (
  run_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  rule_package_version_id uuid NOT NULL REFERENCES rule_package_versions(rule_package_version_id),
  status text NOT NULL CHECK (status IN ('PENDING', 'PASSED', 'FAILED')),
  started_at timestamptz NOT NULL DEFAULT now(),
  finished_at timestamptz NULL,
  results jsonb NULL,
  error_message text NULL
);

CREATE INDEX idx_rule_tests_runs_tenant ON rule_tests_runs(tenant_id);
CREATE INDEX idx_rule_tests_runs_version ON rule_tests_runs(rule_package_version_id);

-- ============================================================================
-- PASSPORT VERSIONS
-- ============================================================================

CREATE TABLE passport_versions (
  passport_version_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  passport_id uuid NOT NULL REFERENCES passports(passport_id),
  status passport_status NOT NULL,
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

CREATE INDEX idx_passport_versions_tenant ON passport_versions(tenant_id);
CREATE INDEX idx_passport_versions_passport ON passport_versions(passport_id);
CREATE INDEX idx_passport_versions_status ON passport_versions(tenant_id, status);

-- Add foreign key for current_passport_version_id after passport_versions exists
ALTER TABLE passports
  ADD CONSTRAINT fk_passports_current_version
  FOREIGN KEY (current_passport_version_id)
  REFERENCES passport_versions(passport_version_id);

-- ============================================================================
-- JOB QUEUE
-- ============================================================================

CREATE TABLE jobs (
  job_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id),
  type job_type NOT NULL,
  status job_status NOT NULL,
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

-- Optimized index for job queue polling
CREATE INDEX idx_jobs_queue ON jobs(status, scheduled_at, priority)
  WHERE status = 'QUEUED';
CREATE INDEX idx_jobs_tenant ON jobs(tenant_id);
CREATE INDEX idx_jobs_lease ON jobs(lease_expires_at)
  WHERE status = 'RUNNING';

-- migrate:down
DROP TABLE IF EXISTS jobs CASCADE;
DROP TABLE IF EXISTS passport_versions CASCADE;
DROP TABLE IF EXISTS rule_tests_runs CASCADE;
DROP TABLE IF EXISTS rule_fields CASCADE;
DROP TABLE IF EXISTS rule_package_versions CASCADE;
DROP TABLE IF EXISTS rule_packages CASCADE;
DROP TABLE IF EXISTS passports CASCADE;
DROP TABLE IF EXISTS battery_products CASCADE;
DROP TABLE IF EXISTS snapshot_items CASCADE;
DROP TABLE IF EXISTS data_snapshots CASCADE;
DROP TABLE IF EXISTS facts CASCADE;
DROP TABLE IF EXISTS document_versions CASCADE;
DROP TABLE IF EXISTS documents CASCADE;
DROP TABLE IF EXISTS events CASCADE;
DROP TABLE IF EXISTS idempotency_keys CASCADE;
DROP TABLE IF EXISTS actor_roles CASCADE;
DROP TABLE IF EXISTS role_permissions CASCADE;
DROP TABLE IF EXISTS permissions CASCADE;
DROP TABLE IF EXISTS roles CASCADE;
DROP TABLE IF EXISTS api_keys CASCADE;
DROP TABLE IF EXISTS actors CASCADE;
DROP TABLE IF EXISTS tenants CASCADE;
DROP TYPE IF EXISTS access_decision CASCADE;
DROP TYPE IF EXISTS passport_status CASCADE;
DROP TYPE IF EXISTS rule_pkg_status CASCADE;
DROP TYPE IF EXISTS snapshot_status CASCADE;
DROP TYPE IF EXISTS document_status CASCADE;
DROP TYPE IF EXISTS document_kind CASCADE;
DROP TYPE IF EXISTS job_status CASCADE;
DROP TYPE IF EXISTS job_type CASCADE;
DROP TYPE IF EXISTS actor_type CASCADE;
