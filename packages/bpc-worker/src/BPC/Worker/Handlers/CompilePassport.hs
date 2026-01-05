{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Compile Passport Handler
--
-- Compiles passport using pure compilePassportPure function.
--
-- @since 0.1.0.0
module BPC.Worker.Handlers.CompilePassport
  ( handle
  , CompilePassportPayload(..)
  ) where

import Control.Monad (void)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), decode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)

import BPC.Worker.Types
import qualified BPC.DB as DB
import qualified BPC.Core.Compile as Compile
import qualified BPC.Core.CanonicalJson as Canonical
import qualified BPC.Core.Rules.Parser as Parser
import qualified BPC.Core.Rules.Eval as Eval

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

-- | Handle COMPILE_PASSPORT job.
--
-- @since 0.1.0.0
handle :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
handle _config pool job = do
  -- Decode payload
  let mPayload = decode (LBS.fromStrict $ DB.jobPayload job) :: Maybe CompilePassportPayload
  case mPayload of
    Nothing -> pure $ HRFailure $ HEValidation "Invalid COMPILE_PASSPORT payload"
    Just payload -> withResource pool $ \conn -> do
      -- Verify snapshot is SEALED
      mSnapshot <- DB.getSnapshot conn (cppTenantId payload) (cppSnapshotId payload)
      case mSnapshot of
        Nothing -> pure $ HRFailure $ HENotFound "Snapshot not found"
        Just snapshot -> case DB.snapStatus snapshot of
          DB.SnapSealed -> do
            -- Verify rules are PUBLISHED
            mRuleVer <- DB.getRuleVersion conn (cppTenantId payload) (cppRuleVersionId payload)
            case mRuleVer of
              Nothing -> pure $ HRFailure $ HENotFound "Rule version not found"
              Just ruleVer -> case DB.rvStatus ruleVer of
                DB.RulePublished -> do
                  -- Parse the DSL source into a Module
                  case Parser.parseSource (DB.rvDslSource ruleVer) of
                    Left parseErr ->
                      pure $ HRFailure $ HENonRetryable $ "Parse error: " <> T.pack (show parseErr)
                    Right rulesModule -> do
                      -- Load facts from snapshot
                      facts <- DB.getSnapshotFacts conn (cppTenantId payload) (cppSnapshotId payload)

                      -- Generate passport version ID
                      versionId <- UUID.nextRandom

                      -- Build compilation input
                      let factsMap = Map.fromList $ map toFactEntry facts
                      let compileInput = Compile.CompileInput
                            { Compile.ciSnapshotId = cppSnapshotId payload
                            , Compile.ciSnapshotHash = fromMaybe "" (DB.snapHash snapshot)
                            , Compile.ciSnapshotStatus = "SEALED"
                            , Compile.ciRulesId = cppRuleVersionId payload
                            , Compile.ciRulesHash = DB.rvDslHash ruleVer
                            , Compile.ciRulesStatus = "PUBLISHED"
                            , Compile.ciRulesModule = rulesModule
                            , Compile.ciFacts = factsMap
                            , Compile.ciTenantId = cppTenantId payload
                            , Compile.ciPassportVersionId = versionId
                            }

                      -- Call pure compilation function
                      case Compile.compilePassportPure compileInput of
                        Left err ->
                          pure $ HRFailure $ HENonRetryable $ "Compilation failed: " <> T.pack (show err)
                        Right output -> do
                          -- Size limits already checked inside compilePassportPure
                          let payloadBytes = Compile.coPayloadCanonical output
                          let proofBytes = Compile.coProofCanonical output
                          -- Serialize receipt for storage
                          let receiptBytes = case Canonical.canonicalEncode (Aeson.toJSON $ Compile.coReceiptUnsigned output) of
                                Left _ -> BS.empty
                                Right bs -> bs

                          -- Create passport version
                          void $ DB.createPassportVersion conn (cppTenantId payload) DB.PassportVersionInput
                            { DB.pviId = versionId
                            , DB.pviPassportId = cppPassportId payload
                            , DB.pviSnapshotId = cppSnapshotId payload
                            , DB.pviRuleVersionId = cppRuleVersionId payload
                            , DB.pviPayloadBytes = payloadBytes
                            , DB.pviPayloadHash = Compile.coPayloadHash output
                            , DB.pviProofBytes = proofBytes
                            , DB.pviProofHash = Compile.coProofHash output
                            , DB.pviReceiptBytes = receiptBytes
                            , DB.pviReceiptHash = Compile.coReceiptHash output
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
                                , "payload_hash" .= Compile.coPayloadHash output
                                , "receipt_hash" .= Compile.coReceiptHash output
                                ]
                            , DB.aeiActorId = Nothing
                            }

                          pure HRSuccess

                _ -> pure $ HRFailure $ HEPrecondition "Rule version not PUBLISHED"

          _ -> pure $ HRFailure $ HEPrecondition "Snapshot not SEALED"

-- | Convert DB fact to a (key, value) entry for the facts map.
-- The key is (factType, factKey) and the value is the payload converted to eval Value.
toFactEntry :: DB.Fact -> ((Text, Text), Eval.Value)
toFactEntry f = ((DB.factType f, DB.factKey f), jsonToValue (DB.factPayload f))

-- | Convert Aeson Value to eval Value (simplified).
jsonToValue :: Aeson.Value -> Eval.Value
jsonToValue v = case v of
  Aeson.Object _ -> Eval.VRecord Map.empty  -- Placeholder for object conversion
  Aeson.Array _ -> Eval.VList []             -- Placeholder for array conversion
  Aeson.String s -> Eval.VString s
  Aeson.Number n -> Eval.VInt (truncate n)
  Aeson.Bool b -> Eval.VBool b
  Aeson.Null -> Eval.VNone
