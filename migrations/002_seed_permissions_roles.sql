-- migrate:up
-- BPC Database Schema - Migration 002: Seed Permissions, Roles, and Dev Tenant
-- SSOT Reference: Section 6.3

-- ============================================================================
-- PERMISSIONS (16 total)
-- ============================================================================

INSERT INTO permissions (permission) VALUES
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
  ('tenant:admin');

-- ============================================================================
-- DEV TENANT
-- ============================================================================

INSERT INTO tenants (tenant_id, slug, name) VALUES
  ('00000000-0000-0000-0000-000000000001', 'dev', 'Development Tenant');

-- ============================================================================
-- DEFAULT ROLES
-- ============================================================================

-- Admin role (all permissions)
INSERT INTO roles (role_id, tenant_id, name) VALUES
  ('00000000-0000-0000-0000-000000000101', '00000000-0000-0000-0000-000000000001', 'Admin');

-- ComplianceOfficer role
INSERT INTO roles (role_id, tenant_id, name) VALUES
  ('00000000-0000-0000-0000-000000000102', '00000000-0000-0000-0000-000000000001', 'ComplianceOfficer');

-- Auditor role
INSERT INTO roles (role_id, tenant_id, name) VALUES
  ('00000000-0000-0000-0000-000000000103', '00000000-0000-0000-0000-000000000001', 'Auditor');

-- ============================================================================
-- ROLE-PERMISSION MAPPINGS
-- ============================================================================

-- Admin gets all 16 permissions
INSERT INTO role_permissions (role_id, permission)
SELECT '00000000-0000-0000-0000-000000000101', permission
FROM permissions;

-- ComplianceOfficer permissions
INSERT INTO role_permissions (role_id, permission) VALUES
  ('00000000-0000-0000-0000-000000000102', 'passport:read'),
  ('00000000-0000-0000-0000-000000000102', 'passport:compile'),
  ('00000000-0000-0000-0000-000000000102', 'passport:replay'),
  ('00000000-0000-0000-0000-000000000102', 'rules:read'),
  ('00000000-0000-0000-0000-000000000102', 'docs:read'),
  ('00000000-0000-0000-0000-000000000102', 'snapshot:build'),
  ('00000000-0000-0000-0000-000000000102', 'snapshot:seal');

-- Auditor permissions
INSERT INTO role_permissions (role_id, permission) VALUES
  ('00000000-0000-0000-0000-000000000103', 'passport:read'),
  ('00000000-0000-0000-0000-000000000103', 'rules:read'),
  ('00000000-0000-0000-0000-000000000103', 'audit:read');

-- migrate:down
DELETE FROM role_permissions WHERE role_id IN (
  '00000000-0000-0000-0000-000000000101',
  '00000000-0000-0000-0000-000000000102',
  '00000000-0000-0000-0000-000000000103'
);
DELETE FROM roles WHERE tenant_id = '00000000-0000-0000-0000-000000000001';
DELETE FROM tenants WHERE tenant_id = '00000000-0000-0000-0000-000000000001';
DELETE FROM permissions;
