{-# LANGUAGE OverloadedStrings #-}

-- | Battery Passport Compiler - Database Layer
--
-- This module re-exports all database types and operations for BPC.
-- Import this module to access:
--
-- * Connection pool management ('mkPool', 'withConn', 'closePool')
-- * Error types ('DBError', 'EventError', 'SnapshotError', etc.)
-- * Repository operations for all entities
--
-- == Architecture
--
-- The database layer follows the repository pattern with tenant isolation.
-- All repository functions take 'TenantId' as the first parameter to ensure
-- data isolation between tenants.
--
-- == Event Sourcing
--
-- All state changes are recorded in the event store with hash chain
-- verification per BPC-EVENT-1 specification. Use 'appendEvent' and
-- 'verifyChain' for audit trail management.
--
-- @since 0.1.0.0
module BPC.DB
  ( -- * Connection Pool
    -- $pool
    DBConfig(..)
  , defaultDBConfig
  , mkPool
  , withConn
  , closePool
  , buildConnString

    -- * Error Types
    -- $errors
  , DBError(..)
  , EventError(..)
  , ChainError(..)
  , SnapshotError(..)
  , SealError(..)
  , JobError(..)
  , DocumentError(..)
  , UploadError(..)
  , FactError(..)
  , RuleError(..)
  , PublishError(..)
  , PassportError(..)
  , ActivateError(..)
  , RevokeError(..)
  , AuthError(..)
  , WebhookError(..)

    -- * Event Store
    -- $events
  , module BPC.DB.Repos.Events

    -- * Document Repository
    -- $documents
  , module BPC.DB.Repos.Documents

    -- * Fact Repository
    -- $facts
  , module BPC.DB.Repos.Facts

    -- * Snapshot Repository
    -- $snapshots
  , module BPC.DB.Repos.Snapshots

    -- * Passport Repository
    -- $passports
  , module BPC.DB.Repos.Passports

    -- * Job Repository
    -- $jobs
  , module BPC.DB.Repos.Jobs

    -- * Rule Repository
    -- $rules
  , module BPC.DB.Repos.Rules

    -- * Auth Repository
    -- $auth
  , module BPC.DB.Repos.Auth

    -- * Webhook Repository
    -- $webhooks
  , module BPC.DB.Repos.Webhooks
  ) where

import BPC.DB.Error
import BPC.DB.Pool
import BPC.DB.Repos.Auth
import BPC.DB.Repos.Documents
import BPC.DB.Repos.Events
import BPC.DB.Repos.Facts
import BPC.DB.Repos.Jobs
import BPC.DB.Repos.Passports
import BPC.DB.Repos.Rules
import BPC.DB.Repos.Snapshots
import BPC.DB.Repos.Webhooks

-- $pool
-- Connection pool management using resource-pool.
--
-- @
-- main = do
--   pool <- mkPool defaultDBConfig { dbName = "bpc_prod" }
--   case pool of
--     Left err -> putStrLn $ "Failed: " <> show err
--     Right p -> do
--       withConn p $ \\conn -> do
--         -- use connection
--         pure ()
--       closePool p
-- @

-- $errors
-- All database operations return 'Either' with specific error types.
-- Pattern match on errors for proper handling.

-- $events
-- Append-only event store with BPC-EVENT-1 hash chain.
-- All state changes should be recorded as events for audit.

-- $documents
-- Document and document version storage with SHA-256 hashing.

-- $facts
-- Fact storage with canonical JSON encoding and payload hashing.

-- $snapshots
-- Snapshot management with BPC-SNAPSHOT-1 deterministic sealing.

-- $passports
-- Passport and version lifecycle management (activate/revoke).

-- $jobs
-- Job queue with idempotent enqueue and race-free leasing.

-- $rules
-- Rule package management with publish validation (500+ test cases).

-- $auth
-- Tenant, actor, API key, and permission management.

-- $webhooks
-- Webhook endpoint, subscription, and delivery tracking.
