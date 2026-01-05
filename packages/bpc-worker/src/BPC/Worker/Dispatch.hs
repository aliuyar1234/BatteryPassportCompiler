{-# LANGUAGE OverloadedStrings #-}

-- | Job Dispatcher
--
-- Maps job types to their handlers.
--
-- @since 0.1.0.0
module BPC.Worker.Dispatch
  ( dispatchJob
  , UnsupportedJobType(..)
  ) where

import Control.Exception (Exception, throwIO)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)

import BPC.Worker.Types
import qualified BPC.DB as DB
import qualified BPC.DB.Repos.Jobs as Jobs
import qualified BPC.Worker.Handlers.ParseFacts as ParseFacts
import qualified BPC.Worker.Handlers.BuildSnapshot as BuildSnapshot
import qualified BPC.Worker.Handlers.CompilePassport as CompilePassport
import qualified BPC.Worker.Handlers.SignPassport as SignPassport
import qualified BPC.Worker.Handlers.GenerateQR as GenerateQR
import qualified BPC.Worker.Handlers.ExportPassport as ExportPassport
import qualified BPC.Worker.Handlers.RunRuleTests as RunRuleTests
import qualified BPC.Worker.Handlers.DeliverWebhook as DeliverWebhook

-- | Exception for unsupported job types.
--
-- @since 0.1.0.0
newtype UnsupportedJobType = UnsupportedJobType Text
  deriving stock (Show, Eq, Generic)

instance Exception UnsupportedJobType

-- | Dispatch a job to the appropriate handler.
--
-- Maps DB job types to their corresponding handlers.
--
-- Note: DB.JobType has 6 variants (PARSE, BUILD, COMPILE, SIGN, QR, WEBHOOK)
-- which map to specific worker handlers.
--
-- @since 0.1.0.0
dispatchJob :: WorkerConfig -> Pool Connection -> Jobs.Job -> IO HandlerResult
dispatchJob config pool job = do
  case Jobs.jobType job of
    Jobs.JobParse ->
      ParseFacts.handle config pool job

    Jobs.JobBuild ->
      BuildSnapshot.handle config pool job

    Jobs.JobCompile ->
      CompilePassport.handle config pool job

    Jobs.JobSign ->
      SignPassport.handle config pool job

    Jobs.JobQr ->
      GenerateQR.handle config pool job

    Jobs.JobWebhook ->
      DeliverWebhook.handle config pool job
