-- Migration 003: Policies and Webhooks
-- Battery Passport Compiler - Advanced Features

-- Policies table (access control beyond RBAC)
CREATE TABLE policies (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    tenant_id UUID NOT NULL REFERENCES tenants(id),
    name TEXT NOT NULL,
    description TEXT,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    priority INTEGER NOT NULL DEFAULT 100,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT policies_tenant_name_unique UNIQUE (tenant_id, name)
);

-- Policy versions (immutable, versioned policies)
CREATE TABLE policy_versions (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    policy_id UUID NOT NULL REFERENCES policies(id),
    tenant_id UUID NOT NULL REFERENCES tenants(id),
    version INTEGER NOT NULL,
    policy_hash TEXT NOT NULL,
    rules JSONB NOT NULL,
    effect TEXT NOT NULL CHECK (effect IN ('ALLOW', 'DENY')),
    resources TEXT[] NOT NULL,
    actions TEXT[] NOT NULL,
    conditions JSONB,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT policy_versions_unique UNIQUE (policy_id, version)
);

-- Webhook endpoints table
CREATE TABLE webhook_endpoints (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    tenant_id UUID NOT NULL REFERENCES tenants(id),
    url TEXT NOT NULL,
    secret TEXT NOT NULL,
    name TEXT NOT NULL,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Webhook subscriptions table
CREATE TABLE webhook_subscriptions (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    endpoint_id UUID NOT NULL REFERENCES webhook_endpoints(id),
    tenant_id UUID NOT NULL REFERENCES tenants(id),
    event_type TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT webhook_subscriptions_unique UNIQUE (endpoint_id, event_type)
);

-- Webhook deliveries table
CREATE TABLE webhook_deliveries (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    subscription_id UUID NOT NULL REFERENCES webhook_subscriptions(id),
    tenant_id UUID NOT NULL REFERENCES tenants(id),
    event_id UUID NOT NULL,
    event_type TEXT NOT NULL,
    payload JSONB NOT NULL,
    status TEXT NOT NULL DEFAULT 'PENDING' CHECK (status IN ('PENDING', 'DELIVERED', 'FAILED')),
    attempts INTEGER NOT NULL DEFAULT 0,
    last_error TEXT,
    delivered_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Indexes for policy evaluation
CREATE INDEX idx_policies_tenant_active ON policies (tenant_id, is_active);
CREATE INDEX idx_policies_priority ON policies (tenant_id, priority);
CREATE INDEX idx_policy_versions_policy ON policy_versions (policy_id, is_active);
CREATE INDEX idx_policy_versions_tenant ON policy_versions (tenant_id, is_active);

-- Indexes for webhook lookup
CREATE INDEX idx_webhook_endpoints_tenant ON webhook_endpoints (tenant_id, is_active);
CREATE INDEX idx_webhook_subscriptions_event ON webhook_subscriptions (tenant_id, event_type);
CREATE INDEX idx_webhook_deliveries_status ON webhook_deliveries (tenant_id, status);
