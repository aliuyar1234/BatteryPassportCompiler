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
-- @since 0.1.0.0
dispatchJob :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
dispatchJob config pool job = do
  let jobType = DB.jobType job
  case parseJobType jobType of
    Just JTIngestDocument ->
      -- Ingest is a no-op in MVP (document already uploaded)
      pure HRSuccess

    Just JTParseFacts ->
      ParseFacts.handle config pool job

    Just JTBuildSnapshot ->
      BuildSnapshot.handle config pool job

    Just JTCompilePassport ->
      CompilePassport.handle config pool job

    Just JTSignPassport ->
      SignPassport.handle config pool job

    Just JTGenerateQR ->
      GenerateQR.handle config pool job

    Just JTExportPassport ->
      ExportPassport.handle config pool job

    Just JTRunRuleTests ->
      RunRuleTests.handle config pool job

    Just JTDeliverWebhook ->
      DeliverWebhook.handle config pool job

    Nothing ->
      throwIO $ UnsupportedJobType jobType

-- | Parse job type from text.
--
-- @since 0.1.0.0
parseJobType :: Text -> Maybe JobType
parseJobType t = case t of
  "INGEST_DOCUMENT" -> Just JTIngestDocument
  "PARSE_FACTS" -> Just JTParseFacts
  "BUILD_SNAPSHOT" -> Just JTBuildSnapshot
  "COMPILE_PASSPORT" -> Just JTCompilePassport
  "SIGN_PASSPORT" -> Just JTSignPassport
  "GENERATE_QR" -> Just JTGenerateQR
  "EXPORT_PASSPORT" -> Just JTExportPassport
  "RUN_RULE_TESTS" -> Just JTRunRuleTests
  "DELIVER_WEBHOOK" -> Just JTDeliverWebhook
  _ -> Nothing
