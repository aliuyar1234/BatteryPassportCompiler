{-# LANGUAGE OverloadedStrings #-}

-- | Worker Main Module
--
-- Configuration loading and worker startup.
--
-- @since 0.1.0.0
module BPC.Worker.Main
  ( loadConfig
  , defaultConfig
  , startWorker
  ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import BPC.Worker.Types
import BPC.Worker.Runner

-- | Load worker configuration from environment variables.
--
-- @since 0.1.0.0
loadConfig :: IO WorkerConfig
loadConfig = do
  pollInterval <- fromMaybe 1000 . (>>= readMaybe) <$> lookupEnv "BPC_JOBS_POLL_INTERVAL_MS"
  leaseRenew <- fromMaybe 30 . (>>= readMaybe) <$> lookupEnv "BPC_JOBS_LEASE_RENEW_SECONDS"
  leaseTimeout <- fromMaybe 300 . (>>= readMaybe) <$> lookupEnv "BPC_JOBS_LEASE_TIMEOUT_SECONDS"
  maxAttempts <- fromMaybe 10 . (>>= readMaybe) <$> lookupEnv "BPC_JOBS_MAX_ATTEMPTS_DEFAULT"
  workerId <- maybe "worker-1" T.pack <$> lookupEnv "BPC_WORKER_ID"
  signingKey <- fmap T.pack <$> lookupEnv "BPC_SIGNING_ED25519_PRIVATE_KEY_BASE64"
  webhookTimeout <- fromMaybe 30 . (>>= readMaybe) <$> lookupEnv "BPC_WEBHOOK_TIMEOUT_SECONDS"

  pure WorkerConfig
    { wcPollIntervalMs = pollInterval
    , wcLeaseRenewSeconds = leaseRenew
    , wcLeaseTimeoutSeconds = leaseTimeout
    , wcMaxAttempts = maxAttempts
    , wcWorkerId = workerId
    , wcSigningKeyBase64 = signingKey
    , wcWebhookTimeoutSeconds = webhookTimeout
    }

-- | Default worker configuration.
--
-- @since 0.1.0.0
defaultConfig :: WorkerConfig
defaultConfig = WorkerConfig
  { wcPollIntervalMs = 1000
  , wcLeaseRenewSeconds = 30
  , wcLeaseTimeoutSeconds = 300
  , wcMaxAttempts = 10
  , wcWorkerId = "worker-default"
  , wcSigningKeyBase64 = Nothing
  , wcWebhookTimeoutSeconds = 30
  }

-- | Start the worker with signal handling.
--
-- @since 0.1.0.0
startWorker :: WorkerConfig -> Pool Connection -> IO ()
startWorker config pool = do
  shutdownMVar <- newEmptyMVar
  -- Run the worker
  runWorker config pool shutdownMVar
