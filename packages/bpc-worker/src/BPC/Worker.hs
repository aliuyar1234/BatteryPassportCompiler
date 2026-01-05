{-# LANGUAGE OverloadedStrings #-}

-- | BPC Worker
--
-- Job processing worker for the Battery Passport Compiler.
--
-- @since 0.1.0.0
module BPC.Worker
  ( -- * Worker Startup
    startWorker
  , runWorker
  , runWorkerOnce
    -- * Configuration
  , WorkerConfig(..)
  , WorkerEnv(..)
  , loadConfig
  , defaultConfig
    -- * Types
  , HandlerError(..)
  , HandlerResult(..)
  , JobType(..)
  , JobPayload(..)
    -- * Retry Logic
  , computeBackoff
  , maxAttempts
  , isRetryable
  , shouldRetry
    -- * Dispatch
  , dispatchJob
    -- * Re-exports
  , module BPC.Worker.Types
  , module BPC.Worker.Retry
  ) where

import BPC.Worker.Types
import BPC.Worker.Retry
import BPC.Worker.Runner (runWorker, runWorkerOnce)
import BPC.Worker.Main (loadConfig, defaultConfig, startWorker)
import BPC.Worker.Dispatch (dispatchJob)
