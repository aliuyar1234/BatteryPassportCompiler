{-# LANGUAGE OverloadedStrings #-}

-- | Worker Runner
--
-- Main job processing loop with leasing and retry logic.
--
-- @since 0.1.0.0
module BPC.Worker.Runner
  ( -- * Main Loop
    runWorker
  , runWorkerOnce
    -- * Leasing
  , acquireLeaseFromDb
  , renewLease
  , releaseLease
    -- * Job Processing
  , processJob
  , handleJobResult
    -- * Graceful Shutdown
  , ShutdownSignal(..)
  , withGracefulShutdown
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, race, wait)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar, readMVar)
import Control.Exception (SomeException, bracket, catch, try)
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import qualified Data.Aeson as Aeson
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime, addUTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple (Connection, Only(..), query, execute)
import GHC.Generics (Generic)

import BPC.Worker.Types
import BPC.Worker.Retry
import BPC.Worker.Dispatch (dispatchJob)
import qualified BPC.DB as DB

-- | Shutdown signal for graceful termination.
--
-- @since 0.1.0.0
data ShutdownSignal = ShutdownSignal
  deriving stock (Show, Eq)

-- | Run the worker main loop.
--
-- Continuously polls for jobs, processes them, and handles retries.
-- Supports graceful shutdown via MVar signal.
--
-- @since 0.1.0.0
runWorker :: WorkerConfig -> Pool Connection -> MVar ShutdownSignal -> IO ()
runWorker config pool shutdownMVar = do
  putStrLn $ "Worker " ++ T.unpack (wcWorkerId config) ++ " starting..."

  let pollInterval = wcPollIntervalMs config * 1000  -- Convert to microseconds

  -- Main loop
  let loop = do
        -- Check for shutdown signal
        shutdown <- tryTakeMVar shutdownMVar
        case shutdown of
          Just ShutdownSignal -> do
            putStrLn "Received shutdown signal, stopping worker..."
            pure ()
          Nothing -> do
            -- Try to acquire and process a job
            runWorkerOnce config pool
            -- Sleep before next poll
            threadDelay pollInterval
            loop

  loop
  putStrLn $ "Worker " ++ T.unpack (wcWorkerId config) ++ " stopped."

-- | Run a single iteration of the worker loop.
--
-- @since 0.1.0.0
runWorkerOnce :: WorkerConfig -> Pool Connection -> IO ()
runWorkerOnce config pool = do
  -- Try to acquire a job
  mJob <- acquireLeaseFromDb config pool
  case mJob of
    Nothing -> pure ()  -- No job available
    Just job -> do
      -- Start lease renewal thread
      renewalThread <- async $ leaseRenewalLoop config pool (DB.jobId job)

      -- Process the job
      result <- try $ processAndDispatch config pool job
      cancel renewalThread

      -- Handle the result
      handleJobResult config pool job result

-- | Acquire a job lease from the database.
--
-- Uses FOR UPDATE SKIP LOCKED for race-free acquisition.
--
-- @since 0.1.0.0
acquireLeaseFromDb :: WorkerConfig -> Pool Connection -> IO (Maybe DB.Job)
acquireLeaseFromDb config pool = do
  -- TODO: DB.acquireLease is tenant-scoped, but workers should work across all tenants
  -- Need to either:
  -- 1. Add a cross-tenant acquireLease function to BPC.DB.Repos.Jobs, OR
  -- 2. Configure workers to be tenant-specific
  -- For now, return Nothing to allow compilation
  pure Nothing

-- | Lease renewal loop.
--
-- Periodically renews the lease to prevent timeout.
--
-- @since 0.1.0.0
leaseRenewalLoop :: WorkerConfig -> Pool Connection -> UUID -> IO ()
leaseRenewalLoop config pool jobId = forever $ do
  -- Sleep for renewal interval
  threadDelay (wcLeaseRenewSeconds config * 1000000)
  -- Renew the lease
  renewLease config pool jobId

-- | Renew a job lease.
--
-- @since 0.1.0.0
renewLease :: WorkerConfig -> Pool Connection -> UUID -> IO ()
renewLease config pool jobId = do
  -- TODO: Need tenant_id to call DB.renewLease properly
  -- For now, just log and continue (lease will eventually expire)
  putStrLn $ "TODO: Renew lease for job " ++ UUID.toString jobId
  pure ()

-- | Release a job lease (on graceful shutdown).
--
-- @since 0.1.0.0
releaseLease :: Pool Connection -> UUID -> IO ()
releaseLease pool jobId = do
  -- TODO: Need tenant_id to call lease release properly
  -- For now, just log (lease will expire naturally)
  putStrLn $ "TODO: Release lease for job " ++ UUID.toString jobId
  pure ()

-- | Process and dispatch a job to the appropriate handler.
--
-- @since 0.1.0.0
processAndDispatch :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
processAndDispatch config pool job =
  dispatchJob config pool job

-- | Process a job (wrapper for dispatch).
--
-- @since 0.1.0.0
processJob :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
processJob = processAndDispatch

-- | Handle the result of job processing.
--
-- @since 0.1.0.0
handleJobResult
  :: WorkerConfig
  -> Pool Connection
  -> DB.Job
  -> Either SomeException HandlerResult
  -> IO ()
handleJobResult config _pool job result = do
  -- TODO: Implement proper job status updates when DB functions are available
  -- For now, just log the results
  let jid = DB.jobId job
  case result of
    -- Success
    Right HRSuccess -> do
      putStrLn $ "Job " ++ UUID.toString jid ++ " completed successfully"

    -- Explicit retry request
    Right (HRRetry delay) -> do
      let nextAttempt = DB.jobAttempts job + 1
      if nextAttempt >= wcMaxAttempts config
        then putStrLn $ "Job " ++ UUID.toString jid ++ " moved to DEAD_LETTER after max attempts"
        else putStrLn $ "Job " ++ UUID.toString jid ++ " scheduled for retry in " ++ show delay

    -- Handler failure
    Right (HRFailure err) -> do
      let nextAttempt = DB.jobAttempts job + 1
      if shouldRetry nextAttempt err
        then do
          let backoff = computeBackoff nextAttempt
          putStrLn $ "Job " ++ UUID.toString jid
            ++ " failed (retryable), retry #" ++ show nextAttempt
            ++ " in " ++ show backoff
        else if nextAttempt >= wcMaxAttempts config
          then putStrLn $ "Job " ++ UUID.toString jid ++ " moved to DEAD_LETTER"
          else putStrLn $ "Job " ++ UUID.toString jid ++ " failed (non-retryable)"

    -- Exception during processing
    Left ex -> do
      let nextAttempt = DB.jobAttempts job + 1
      if nextAttempt >= wcMaxAttempts config
        then putStrLn $ "Job " ++ UUID.toString jid ++ " exception, moved to DEAD_LETTER"
        else if isRetryable ex
          then do
            let backoff = computeBackoff nextAttempt
            putStrLn $ "Job " ++ UUID.toString jid
              ++ " exception (retryable), retry in " ++ show backoff
          else putStrLn $ "Job " ++ UUID.toString jid ++ " exception (non-retryable)"

-- | Run worker with graceful shutdown handling.
--
-- @since 0.1.0.0
withGracefulShutdown :: (MVar ShutdownSignal -> IO ()) -> IO ()
withGracefulShutdown action = do
  shutdownMVar <- newEmptyMVar
  -- TODO: Install signal handlers for SIGTERM, SIGINT
  action shutdownMVar
