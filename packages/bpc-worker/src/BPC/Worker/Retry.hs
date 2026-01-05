{-# LANGUAGE OverloadedStrings #-}

-- | Retry Logic
--
-- Exponential backoff and retry classification for job processing.
--
-- @since 0.1.0.0
module BPC.Worker.Retry
  ( -- * Constants
    maxAttempts
  , maxBackoffSeconds
    -- * Backoff Computation
  , computeBackoff
    -- * Retry Classification
  , isRetryable
  , isRetryableError
    -- * Utilities
  , shouldRetry
  ) where

import Control.Exception (SomeException, fromException)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple (SqlError(..))
import GHC.Generics (Generic)
import System.IO.Error (isDoesNotExistError, isPermissionError)

import BPC.Worker.Types (HandlerError(..))

-- | Maximum number of retry attempts.
--
-- Per SSOT 4.6: BPC_JOBS_MAX_ATTEMPTS_DEFAULT = 10
--
-- @since 0.1.0.0
maxAttempts :: Int
maxAttempts = 10

-- | Maximum backoff duration in seconds.
--
-- Caps at 1024 seconds (~17 minutes).
--
-- @since 0.1.0.0
maxBackoffSeconds :: Int
maxBackoffSeconds = 1024

-- | Compute exponential backoff delay.
--
-- Formula: min(2^attempts, 1024) seconds
--
-- Examples:
-- - computeBackoff 1 = 2s
-- - computeBackoff 5 = 32s
-- - computeBackoff 10 = 1024s (capped)
-- - computeBackoff 15 = 1024s (capped)
--
-- @since 0.1.0.0
computeBackoff :: Int -> NominalDiffTime
computeBackoff attempts =
  let seconds = min (2 ^ attempts) maxBackoffSeconds
  in fromIntegral seconds

-- | Check if an exception is retryable.
--
-- Retryable errors include:
-- - Database connection errors
-- - Temporary network issues
-- - Lock contention
--
-- Non-retryable errors include:
-- - Validation errors
-- - Not found errors
-- - Permission errors
--
-- @since 0.1.0.0
isRetryable :: SomeException -> Bool
isRetryable e
  -- Database errors are often transient
  | Just (sqlErr :: SqlError) <- fromException e =
      isRetryableSqlError sqlErr
  -- IO errors - check specific types
  | Just ioErr <- fromException e =
      not (isDoesNotExistError ioErr || isPermissionError ioErr)
  -- Default to retryable for unknown errors
  | otherwise = True

-- | Check if a SQL error is retryable.
--
-- @since 0.1.0.0
isRetryableSqlError :: SqlError -> Bool
isRetryableSqlError SqlError{..} =
  case sqlState of
    -- Connection errors
    "08000" -> True  -- connection_exception
    "08003" -> True  -- connection_does_not_exist
    "08006" -> True  -- connection_failure
    -- Lock errors
    "40001" -> True  -- serialization_failure
    "40P01" -> True  -- deadlock_detected
    "55P03" -> True  -- lock_not_available
    -- Resource errors
    "53000" -> True  -- insufficient_resources
    "53100" -> True  -- disk_full
    "53200" -> True  -- out_of_memory
    "53300" -> True  -- too_many_connections
    -- Constraint violations are NOT retryable
    "23000" -> False -- integrity_constraint_violation
    "23505" -> False -- unique_violation
    "23503" -> False -- foreign_key_violation
    -- Data errors are NOT retryable
    "22000" -> False -- data_exception
    -- Default to not retryable for unknown SQL states
    _ -> False

-- | Check if a handler error is retryable.
--
-- @since 0.1.0.0
isRetryableError :: HandlerError -> Bool
isRetryableError (HERetryable _) = True
isRetryableError (HENonRetryable _) = False
isRetryableError (HEValidation _) = False
isRetryableError (HENotFound _) = False
isRetryableError (HEPrecondition _) = False
isRetryableError (HEInternal _) = True  -- Internal errors might be transient

-- | Determine if a job should be retried.
--
-- @since 0.1.0.0
shouldRetry :: Int -> HandlerError -> Bool
shouldRetry attempts err =
  attempts < maxAttempts && isRetryableError err
