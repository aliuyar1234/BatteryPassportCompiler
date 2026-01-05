{-# LANGUAGE OverloadedStrings #-}

-- | Job Repository
--
-- Job queue with idempotent enqueue and race-free leasing using
-- FOR UPDATE SKIP LOCKED. Implements exponential backoff for retries.
--
-- @since 0.1.0.0
module BPC.DB.Repos.Jobs
  ( -- * Types
    JobId
  , JobStatus(..)
  , JobType(..)
  , Job(..)
  , JobInput(..)
    -- * Operations
  , enqueue
  , acquireLease
  , renewLease
  , complete
  , failJob
  , cancelJob
  , getJob
  , getPendingJobs
  , getDeadLetterJobs
    -- * Utilities
  , calculateBackoff
  ) where

import Control.Exception (catch)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, NominalDiffTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection, Only(..), SqlError)
import qualified Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)

import BPC.DB.Error (JobError(..))
import BPC.DB.Repos.Events (TenantId)

-- | Job ID (UUID)
type JobId = UUID

-- | Job status enum.
data JobStatus
  = JobQueued
  | JobRunning
  | JobSucceeded
  | JobFailed
  | JobCancelled
  | JobDeadLetter
  deriving stock (Show, Eq, Generic)

statusToText :: JobStatus -> Text
statusToText = \case
  JobQueued     -> "QUEUED"
  JobRunning    -> "RUNNING"
  JobSucceeded  -> "SUCCEEDED"
  JobFailed     -> "FAILED"
  JobCancelled  -> "CANCELLED"
  JobDeadLetter -> "DEAD_LETTER"

textToStatus :: Text -> Maybe JobStatus
textToStatus = \case
  "QUEUED"      -> Just JobQueued
  "RUNNING"     -> Just JobRunning
  "SUCCEEDED"   -> Just JobSucceeded
  "FAILED"      -> Just JobFailed
  "CANCELLED"   -> Just JobCancelled
  "DEAD_LETTER" -> Just JobDeadLetter
  _             -> Nothing

-- | Job type enum.
data JobType
  = JobParse
  | JobBuild
  | JobCompile
  | JobSign
  | JobQr
  | JobWebhook
  deriving stock (Show, Eq, Generic)

jobTypeToText :: JobType -> Text
jobTypeToText = \case
  JobParse   -> "PARSE"
  JobBuild   -> "BUILD"
  JobCompile -> "COMPILE"
  JobSign    -> "SIGN"
  JobQr      -> "QR"
  JobWebhook -> "WEBHOOK"

textToJobType :: Text -> Maybe JobType
textToJobType = \case
  "PARSE"   -> Just JobParse
  "BUILD"   -> Just JobBuild
  "COMPILE" -> Just JobCompile
  "SIGN"    -> Just JobSign
  "QR"      -> Just JobQr
  "WEBHOOK" -> Just JobWebhook
  _         -> Nothing

-- | Job record.
data Job = Job
  { jobId :: JobId
  , jobTenantId :: TenantId
  , jobType :: JobType
  , jobIdempotencyKey :: Text
  , jobPayload :: Value
  , jobStatus :: JobStatus
  , jobAttempts :: Int
  , jobMaxAttempts :: Int
  , jobWorkerId :: Maybe Text
  , jobLeaseExpiresAt :: Maybe UTCTime
  , jobLastError :: Maybe Value
  , jobScheduledAt :: UTCTime
  , jobStartedAt :: Maybe UTCTime
  , jobCompletedAt :: Maybe UTCTime
  , jobCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Input for enqueuing a job.
data JobInput = JobInput
  { jiType :: JobType
  -- ^ Job type
  , jiIdempotencyKey :: Text
  -- ^ Unique key for idempotent enqueue
  , jiPayload :: Value
  -- ^ Job payload (JSON)
  , jiMaxAttempts :: Int
  -- ^ Maximum retry attempts (default: 3)
  , jiScheduledAt :: Maybe UTCTime
  -- ^ Scheduled execution time (default: now)
  }
  deriving stock (Show, Eq, Generic)

-- | Calculate exponential backoff delay.
--
-- delay = min(2^attempts, 1024) seconds
--
-- @since 0.1.0.0
calculateBackoff :: Int -> NominalDiffTime
calculateBackoff attempts =
  let delay = min (2 ^ attempts) 1024
  in fromIntegral (delay :: Int)

-- | Enqueue a new job (idempotent).
--
-- Uses ON CONFLICT DO NOTHING for idempotency on (tenant_id, idempotency_key).
-- Returns existing job ID if already enqueued.
--
-- @since 0.1.0.0
enqueue
  :: Connection
  -> TenantId
  -> JobInput
  -> IO JobId
enqueue conn tenantId input = do
  -- Try to insert; ON CONFLICT returns nothing
  result <- PG.query conn
    "INSERT INTO jobs \
    \(tenant_id, job_type, idempotency_key, payload, status, attempts, max_attempts, scheduled_at) \
    \VALUES (?, ?, ?, ?, 'QUEUED', 0, ?, COALESCE(?, NOW())) \
    \ON CONFLICT (tenant_id, idempotency_key) DO NOTHING \
    \RETURNING id"
    ( tenantId
    , jobTypeToText $ jiType input
    , jiIdempotencyKey input
    , jiPayload input
    , jiMaxAttempts input
    , jiScheduledAt input
    ) :: IO [Only UUID]

  case result of
    (Only jid : _) -> pure jid
    [] -> do
      -- Already exists, get existing ID
      [Only existingId] <- PG.query conn
        "SELECT id FROM jobs WHERE tenant_id = ? AND idempotency_key = ?"
        (tenantId, jiIdempotencyKey input)
      pure existingId

-- | Acquire lease on a job (race-free).
--
-- Uses FOR UPDATE SKIP LOCKED to prevent race conditions.
-- Returns Nothing if no jobs available.
--
-- @since 0.1.0.0
acquireLease
  :: Connection
  -> TenantId
  -> Text
  -> IO (Maybe Job)
acquireLease conn tenantId workerId = do
  -- Select and lock a QUEUED job
  rows <- PG.query conn
    "UPDATE jobs SET \
    \  status = 'RUNNING', \
    \  worker_id = ?, \
    \  attempts = attempts + 1, \
    \  started_at = NOW(), \
    \  lease_expires_at = NOW() + INTERVAL '5 minutes' \
    \WHERE id = ( \
    \  SELECT id FROM jobs \
    \  WHERE tenant_id = ? AND status = 'QUEUED' AND scheduled_at <= NOW() \
    \  ORDER BY scheduled_at ASC \
    \  FOR UPDATE SKIP LOCKED \
    \  LIMIT 1 \
    \) \
    \RETURNING id, tenant_id, job_type, idempotency_key, payload, status, \
    \          attempts, max_attempts, worker_id, lease_expires_at, last_error, \
    \          scheduled_at, started_at, completed_at, created_at"
    (workerId, tenantId)
  pure $ case rows of
    [] -> Nothing
    (row : _) -> Just $ rowToJob row

-- | Renew lease on a job.
--
-- Extends lease by 5 minutes. Returns LEASE_NOT_HELD if worker doesn't hold lease.
--
-- @since 0.1.0.0
renewLease
  :: Connection
  -> TenantId
  -> JobId
  -> Text
  -> IO (Either JobError ())
renewLease conn tenantId jobId workerId = do
  n <- PG.execute conn
    "UPDATE jobs SET lease_expires_at = NOW() + INTERVAL '5 minutes' \
    \WHERE tenant_id = ? AND id = ? AND worker_id = ? AND status = 'RUNNING'"
    (tenantId, jobId, workerId)
  if n > 0
    then pure $ Right ()
    else do
      mJob <- getJob conn tenantId jobId
      case mJob of
        Nothing -> pure $ Left $ JOB_NOT_FOUND jobId
        Just job
          | jobWorkerId job /= Just workerId -> pure $ Left $ LEASE_NOT_HELD jobId workerId
          | jobStatus job /= JobRunning -> pure $ Left $ LEASE_EXPIRED jobId
          | otherwise -> pure $ Left $ LEASE_EXPIRED jobId

-- | Mark job as completed successfully.
--
-- @since 0.1.0.0
complete
  :: Connection
  -> TenantId
  -> JobId
  -> IO (Either JobError ())
complete conn tenantId jobId = do
  n <- PG.execute conn
    "UPDATE jobs SET status = 'SUCCEEDED', completed_at = NOW() \
    \WHERE tenant_id = ? AND id = ? AND status = 'RUNNING'"
    (tenantId, jobId)
  if n > 0
    then pure $ Right ()
    else do
      mJob <- getJob conn tenantId jobId
      case mJob of
        Nothing -> pure $ Left $ JOB_NOT_FOUND jobId
        Just job
          | jobStatus job `elem` [JobSucceeded, JobFailed, JobCancelled, JobDeadLetter] ->
              pure $ Left $ JOB_ALREADY_COMPLETED jobId
          | otherwise -> pure $ Left $ JOB_NOT_FOUND jobId

-- | Mark job as failed with error.
--
-- Implements exponential backoff. Moves to DEAD_LETTER if max attempts exceeded.
--
-- @since 0.1.0.0
failJob
  :: Connection
  -> TenantId
  -> JobId
  -> Value
  -> IO (Either JobError ())
failJob conn tenantId jobId errorData = do
  mJob <- getJob conn tenantId jobId
  case mJob of
    Nothing -> pure $ Left $ JOB_NOT_FOUND jobId
    Just job
      | jobStatus job /= JobRunning -> pure $ Left $ JOB_ALREADY_COMPLETED jobId
      | jobAttempts job >= jobMaxAttempts job -> do
          -- Move to DEAD_LETTER
          _ <- PG.execute conn
            "UPDATE jobs SET status = 'DEAD_LETTER', last_error = ?, completed_at = NOW() \
            \WHERE tenant_id = ? AND id = ?"
            (errorData, tenantId, jobId)
          pure $ Right ()
      | otherwise -> do
          -- Schedule retry with backoff
          let backoff = calculateBackoff (jobAttempts job)
          _ <- PG.execute conn
            "UPDATE jobs SET \
            \  status = 'QUEUED', \
            \  worker_id = NULL, \
            \  lease_expires_at = NULL, \
            \  last_error = ?, \
            \  scheduled_at = NOW() + ? * INTERVAL '1 second' \
            \WHERE tenant_id = ? AND id = ?"
            (errorData, realToFrac backoff :: Double, tenantId, jobId)
          pure $ Right ()

-- | Cancel a job.
--
-- Only QUEUED jobs can be cancelled.
--
-- @since 0.1.0.0
cancelJob
  :: Connection
  -> TenantId
  -> JobId
  -> IO (Either JobError ())
cancelJob conn tenantId jobId = do
  n <- PG.execute conn
    "UPDATE jobs SET status = 'CANCELLED', completed_at = NOW() \
    \WHERE tenant_id = ? AND id = ? AND status = 'QUEUED'"
    (tenantId, jobId)
  if n > 0
    then pure $ Right ()
    else do
      mJob <- getJob conn tenantId jobId
      case mJob of
        Nothing -> pure $ Left $ JOB_NOT_FOUND jobId
        Just job
          | jobStatus job == JobCancelled -> pure $ Left $ JOB_CANCELLED jobId
          | otherwise -> pure $ Left $ JOB_ALREADY_COMPLETED jobId

-- | Get a job by ID.
getJob
  :: Connection
  -> TenantId
  -> JobId
  -> IO (Maybe Job)
getJob conn tenantId jobId = do
  rows <- PG.query conn
    "SELECT id, tenant_id, job_type, idempotency_key, payload, status, \
    \       attempts, max_attempts, worker_id, lease_expires_at, last_error, \
    \       scheduled_at, started_at, completed_at, created_at \
    \FROM jobs WHERE tenant_id = ? AND id = ?"
    (tenantId, jobId)
  pure $ case rows of
    [] -> Nothing
    (row : _) -> Just $ rowToJob row

-- | Get all pending (QUEUED) jobs.
getPendingJobs
  :: Connection
  -> TenantId
  -> Int
  -> IO [Job]
getPendingJobs conn tenantId limit = do
  rows <- PG.query conn
    "SELECT id, tenant_id, job_type, idempotency_key, payload, status, \
    \       attempts, max_attempts, worker_id, lease_expires_at, last_error, \
    \       scheduled_at, started_at, completed_at, created_at \
    \FROM jobs \
    \WHERE tenant_id = ? AND status = 'QUEUED' AND scheduled_at <= NOW() \
    \ORDER BY scheduled_at ASC \
    \LIMIT ?"
    (tenantId, limit)
  pure $ map rowToJob rows

-- | Get all dead letter jobs.
getDeadLetterJobs
  :: Connection
  -> TenantId
  -> Int
  -> IO [Job]
getDeadLetterJobs conn tenantId limit = do
  rows <- PG.query conn
    "SELECT id, tenant_id, job_type, idempotency_key, payload, status, \
    \       attempts, max_attempts, worker_id, lease_expires_at, last_error, \
    \       scheduled_at, started_at, completed_at, created_at \
    \FROM jobs \
    \WHERE tenant_id = ? AND status = 'DEAD_LETTER' \
    \ORDER BY completed_at DESC \
    \LIMIT ?"
    (tenantId, limit)
  pure $ map rowToJob rows

-- | Convert database row to Job.
rowToJob
  :: (UUID, UUID, Text, Text, Value, Text, Int, Int, Maybe Text, Maybe UTCTime,
      Maybe Value, UTCTime, Maybe UTCTime, Maybe UTCTime, UTCTime)
  -> Job
rowToJob (jid, tid, jtype, ikey, payload, status, attempts, maxAttempts,
          workerId, leaseExp, lastErr, schedAt, startAt, compAt, cat) = Job
  { jobId = jid
  , jobTenantId = tid
  , jobType = maybe JobParse id $ textToJobType jtype
  , jobIdempotencyKey = ikey
  , jobPayload = payload
  , jobStatus = maybe JobQueued id $ textToStatus status
  , jobAttempts = attempts
  , jobMaxAttempts = maxAttempts
  , jobWorkerId = workerId
  , jobLeaseExpiresAt = leaseExp
  , jobLastError = lastErr
  , jobScheduledAt = schedAt
  , jobStartedAt = startAt
  , jobCompletedAt = compAt
  , jobCreatedAt = cat
  }
