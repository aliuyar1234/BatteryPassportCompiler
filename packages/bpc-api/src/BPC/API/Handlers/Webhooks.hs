{-# LANGUAGE OverloadedStrings #-}

-- | Webhook Handlers
module BPC.API.Handlers.Webhooks
  ( createWebhookEndpoint
  , listWebhookEndpoints
  , getWebhookEndpoint
  , deactivateWebhookEndpoint
  , subscribeWebhook
  , unsubscribeWebhook
  , listWebhookSubscriptions
  ) where

import Control.Monad.Except (throwError)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

import BPC.API.App (AppM, withPool)
import BPC.API.Error (AppError(..))
import BPC.API.Types (AuthContext(..), Permission(..), CursorPage(..), CreatedResponse(..))
import BPC.API.Middleware.Auth (requirePermission)
import qualified BPC.DB as DB

data WebhookEndpointResponse = WebhookEndpointResponse
  { werId :: UUID, werUrl :: Text, werName :: Text
  , werIsActive :: Bool, werCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON WebhookEndpointResponse where
  toJSON WebhookEndpointResponse{..} = object
    [ "id" .= werId, "url" .= werUrl, "name" .= werName
    , "is_active" .= werIsActive, "created_at" .= werCreatedAt
    ]

data WebhookSubscriptionResponse = WebhookSubscriptionResponse
  { wsrId :: UUID, wsrEndpointId :: UUID, wsrEventType :: Text, wsrCreatedAt :: UTCTime }
  deriving stock (Show, Eq, Generic)

instance ToJSON WebhookSubscriptionResponse where
  toJSON WebhookSubscriptionResponse{..} = object
    [ "id" .= wsrId, "endpoint_id" .= wsrEndpointId
    , "event_type" .= wsrEventType, "created_at" .= wsrCreatedAt
    ]

createWebhookEndpoint :: AuthContext -> Text -> Text -> Text -> AppM CreatedResponse
createWebhookEndpoint ctx url secret name = do
  requirePermission PermWebhookManage ctx
  result <- withPool $ \conn -> DB.createEndpoint conn (acTenantId ctx) $ DB.EndpointInput url secret name
  case result of
    Left err -> throwError $ InternalError "Failed to create webhook endpoint"
    Right endpointId -> pure CreatedResponse { crId = endpointId, crLocation = "/v1/webhook-endpoints/" }

listWebhookEndpoints :: AuthContext -> AppM [WebhookEndpointResponse]
listWebhookEndpoints ctx = do
  requirePermission PermWebhookManage ctx
  endpoints <- withPool $ \conn -> DB.listEndpoints conn (acTenantId ctx)
  pure $ map toResponse endpoints
  where
    toResponse e = WebhookEndpointResponse (DB.weId e) (DB.weUrl e) (DB.weName e) (DB.weIsActive e) (DB.weCreatedAt e)

getWebhookEndpoint :: AuthContext -> UUID -> AppM WebhookEndpointResponse
getWebhookEndpoint ctx endpointId = do
  requirePermission PermWebhookManage ctx
  result <- withPool $ \conn -> DB.getEndpoint conn (acTenantId ctx) endpointId
  case result of
    Nothing -> throwError $ NotFound "Webhook endpoint"
    Just e -> pure $ WebhookEndpointResponse (DB.weId e) (DB.weUrl e) (DB.weName e) (DB.weIsActive e) (DB.weCreatedAt e)

deactivateWebhookEndpoint :: AuthContext -> UUID -> AppM ()
deactivateWebhookEndpoint ctx endpointId = do
  requirePermission PermWebhookManage ctx
  success <- withPool $ \conn -> DB.deactivateEndpoint conn (acTenantId ctx) endpointId
  if success then pure () else throwError $ NotFound "Webhook endpoint"

subscribeWebhook :: AuthContext -> UUID -> Text -> AppM CreatedResponse
subscribeWebhook ctx endpointId eventType = do
  requirePermission PermWebhookManage ctx
  let mEventType = textToEventType eventType
  case mEventType of
    Nothing -> throwError $ InvalidRequest "Invalid event type"
    Just et -> do
      result <- withPool $ \conn -> DB.subscribe conn (acTenantId ctx) endpointId et
      case result of
        Left err -> throwError $ InternalError "Failed to subscribe"
        Right subId -> pure CreatedResponse { crId = subId, crLocation = "/v1/webhook-subscriptions/" }
  where
    textToEventType t = case t of
      "document.uploaded" -> Just DB.EventDocumentUploaded
      "document.parsed" -> Just DB.EventDocumentParsed
      "snapshot.sealed" -> Just DB.EventSnapshotSealed
      "passport.compiled" -> Just DB.EventPassportCompiled
      "passport.signed" -> Just DB.EventPassportSigned
      "passport.activated" -> Just DB.EventPassportActivated
      "passport.revoked" -> Just DB.EventPassportRevoked
      "job.failed" -> Just DB.EventJobFailed
      _ -> Nothing

unsubscribeWebhook :: AuthContext -> UUID -> Text -> AppM ()
unsubscribeWebhook ctx endpointId eventType = do
  requirePermission PermWebhookManage ctx
  let mEventType = textToEventType eventType
  case mEventType of
    Nothing -> throwError $ InvalidRequest "Invalid event type"
    Just et -> do
      success <- withPool $ \conn -> DB.unsubscribe conn (acTenantId ctx) endpointId et
      if success then pure () else throwError $ NotFound "Subscription"
  where
    textToEventType t = case t of
      "document.uploaded" -> Just DB.EventDocumentUploaded
      "document.parsed" -> Just DB.EventDocumentParsed
      "snapshot.sealed" -> Just DB.EventSnapshotSealed
      "passport.compiled" -> Just DB.EventPassportCompiled
      "passport.signed" -> Just DB.EventPassportSigned
      "passport.activated" -> Just DB.EventPassportActivated
      "passport.revoked" -> Just DB.EventPassportRevoked
      "job.failed" -> Just DB.EventJobFailed
      _ -> Nothing

listWebhookSubscriptions :: AuthContext -> UUID -> AppM [WebhookSubscriptionResponse]
listWebhookSubscriptions ctx endpointId = do
  requirePermission PermWebhookManage ctx
  subs <- withPool $ \conn -> DB.getSubscriptions conn (acTenantId ctx) endpointId
  pure $ map toResponse subs
  where
    toResponse s = WebhookSubscriptionResponse (DB.wsId s) (DB.wsEndpointId s)
                     (eventTypeToText $ DB.wsEventType s) (DB.wsCreatedAt s)
    eventTypeToText DB.EventDocumentUploaded = "document.uploaded"
    eventTypeToText DB.EventDocumentParsed = "document.parsed"
    eventTypeToText DB.EventSnapshotSealed = "snapshot.sealed"
    eventTypeToText DB.EventPassportCompiled = "passport.compiled"
    eventTypeToText DB.EventPassportSigned = "passport.signed"
    eventTypeToText DB.EventPassportActivated = "passport.activated"
    eventTypeToText DB.EventPassportRevoked = "passport.revoked"
    eventTypeToText DB.EventJobFailed = "job.failed"
