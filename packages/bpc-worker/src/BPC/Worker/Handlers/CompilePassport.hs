{-# LANGUAGE OverloadedStrings #-}

-- | Compile Passport Handler
--
-- Compiles passport using pure compilePassportPure function.
--
-- @since 0.1.0.0
module BPC.Worker.Handlers.CompilePassport
  ( handle
  , CompilePassportPayload(..)
  ) where

import Control.Monad (void, when)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), decode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)

import BPC.Worker.Types
import qualified BPC.DB as DB
import qualified BPC.Core.Compiler as Compiler
import qualified BPC.Core.Canonical as Canonical

-- | Payload for COMPILE_PASSPORT job.
--
-- @since 0.1.0.0
data CompilePassportPayload = CompilePassportPayload
  { cppPassportId :: UUID
  , cppTenantId :: UUID
  , cppSnapshotId :: UUID
  , cppRuleVersionId :: UUID
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CompilePassportPayload where
  parseJSON = Aeson.withObject "CompilePassportPayload" $ \o -> CompilePassportPayload
    <$> o .: "passport_id"
    <*> o .: "tenant_id"
    <*> o .: "snapshot_id"
    <*> o .: "rule_version_id"

instance ToJSON CompilePassportPayload where
  toJSON CompilePassportPayload{..} = object
    [ "passport_id" .= cppPassportId
    , "tenant_id" .= cppTenantId
    , "snapshot_id" .= cppSnapshotId
    , "rule_version_id" .= cppRuleVersionId
    ]

-- | Maximum sizes per SSOT.
maxPayloadSize :: Int
maxPayloadSize = 131072  -- 128 KB

maxProofSize :: Int
maxProofSize = 262144  -- 256 KB

maxReceiptSize :: Int
maxReceiptSize = 16384  -- 16 KB

-- | Handle COMPILE_PASSPORT job.
--
-- @since 0.1.0.0
handle :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
handle config pool job = do
  -- Decode payload
  let mPayload = decode (LBS.fromStrict $ DB.jobPayload job) :: Maybe CompilePassportPayload
  case mPayload of
    Nothing -> pure $ HRFailure $ HEValidation "Invalid COMPILE_PASSPORT payload"
    Just payload -> withResource pool $ \conn -> do
      -- Verify snapshot is SEALED
      mSnapshot <- DB.getSnapshot conn (cppTenantId payload) (cppSnapshotId payload)
      case mSnapshot of
        Nothing -> pure $ HRFailure $ HENotFound "Snapshot not found"
        Just snapshot -> case DB.snapshotStatus snapshot of
          DB.SnapshotSealed -> do
            -- Verify rules are PUBLISHED
            mRuleVer <- DB.getRuleVersion conn (cppTenantId payload) (cppRuleVersionId payload)
            case mRuleVer of
              Nothing -> pure $ HRFailure $ HENotFound "Rule version not found"
              Just ruleVer -> case DB.rvStatus ruleVer of
                DB.RuleVersionPublished -> do
                  -- Load facts from snapshot
                  facts <- DB.getSnapshotFacts conn (cppTenantId payload) (cppSnapshotId payload)

                  -- Build compilation input
                  let compileInput = Compiler.CompileInput
                        { Compiler.ciSnapshotId = cppSnapshotId payload
                        , Compiler.ciSnapshotHash = DB.snapshotHash snapshot
                        , Compiler.ciFacts = map toCoreFact facts
                        , Compiler.ciRules = DB.rvRules ruleVer
                        , Compiler.ciRuleVersionId = cppRuleVersionId payload
                        }

                  -- Call pure compilation function
                  case Compiler.compilePassportPure compileInput of
                    Left err ->
                      pure $ HRFailure $ HENonRetryable $ "Compilation failed: " <> T.pack (show err)
                    Right output -> do
                      -- Verify size limits
                      let payloadBytes = Compiler.coPayloadBytes output
                      let proofBytes = Compiler.coProofBytes output
                      let receiptBytes = Compiler.coReceiptBytes output

                      if BS.length payloadBytes > maxPayloadSize
                        then pure $ HRFailure $ HENonRetryable "Payload exceeds 128KB limit"
                        else if BS.length proofBytes > maxProofSize
                          then pure $ HRFailure $ HENonRetryable "Proof exceeds 256KB limit"
                          else if BS.length receiptBytes > maxReceiptSize
                            then pure $ HRFailure $ HENonRetryable "Receipt exceeds 16KB limit"
                            else do
                              -- Create passport version
                              versionId <- UUID.nextRandom
                              now <- getCurrentTime
                              void $ DB.createPassportVersion conn (cppTenantId payload) DB.PassportVersionInput
                                { DB.pviId = versionId
                                , DB.pviPassportId = cppPassportId payload
                                , DB.pviSnapshotId = cppSnapshotId payload
                                , DB.pviRuleVersionId = cppRuleVersionId payload
                                , DB.pviPayloadBytes = payloadBytes
                                , DB.pviPayloadHash = Compiler.coPayloadHash output
                                , DB.pviProofBytes = proofBytes
                                , DB.pviProofHash = Compiler.coProofHash output
                                , DB.pviReceiptBytes = receiptBytes
                                , DB.pviReceiptHash = Compiler.coReceiptHash output
                                }

                              -- Enqueue SIGN_PASSPORT job
                              signJobId <- UUID.nextRandom
                              void $ DB.enqueue conn (cppTenantId payload) DB.JobInput
                                { DB.jiId = signJobId
                                , DB.jiType = "SIGN_PASSPORT"
                                , DB.jiPayload = LBS.toStrict $ Aeson.encode $ object
                                    [ "passport_version_id" .= versionId
                                    , "tenant_id" .= cppTenantId payload
                                    ]
                                }

                              -- Emit audit event
                              void $ DB.appendEvent conn (cppTenantId payload) DB.AppendEventInput
                                { DB.aeiAggregateType = "PassportVersion"
                                , DB.aeiAggregateId = versionId
                                , DB.aeiEventType = "PASSPORT_COMPILED"
                                , DB.aeiEventData = Aeson.encode $ object
                                    [ "passport_id" .= cppPassportId payload
                                    , "passport_version_id" .= versionId
                                    , "payload_hash" .= Compiler.coPayloadHash output
                                    , "receipt_hash" .= Compiler.coReceiptHash output
                                    ]
                                , DB.aeiActorId = Nothing
                                }

                              pure HRSuccess

                _ -> pure $ HRFailure $ HEPrecondition "Rule version not PUBLISHED"

          _ -> pure $ HRFailure $ HEPrecondition "Snapshot not SEALED"

-- | Convert DB fact to Core fact.
toCoreFact :: DB.Fact -> Compiler.Fact
toCoreFact f = Compiler.Fact
  { Compiler.fType = DB.factType f
  , Compiler.fKey = DB.factKey f
  , Compiler.fValue = DB.factValue f
  , Compiler.fHash = DB.factHash f
  }
