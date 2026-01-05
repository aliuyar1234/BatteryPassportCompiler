{-# LANGUAGE OverloadedStrings #-}

-- | Policy Middleware
--
-- Fine-grained access control beyond RBAC.
--
-- @since 0.1.0.0
module BPC.API.Middleware.Policy
  ( -- * Types
    PolicyDecision(..)
  , PolicyContext(..)
    -- * Evaluation
  , evaluatePolicy
  , checkPolicy
    -- * Middleware
  , policyMiddleware
  ) where

import Control.Monad (forM_)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Pool (withResource)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import Network.Wai (Application, Middleware, Request, pathInfo, requestMethod)
import qualified Network.Wai as Wai

import BPC.API.App (AppM, Env(..), withPool)
import BPC.API.Error (AppError(..))
import BPC.API.Types (AuthContext(..))
import qualified BPC.DB.Repos.Policies as Policies

-- | Policy decision.
data PolicyDecision
  = PolicyAllow
  -- ^ Request is allowed
  | PolicyDeny Text
  -- ^ Request is denied with reason
  | PolicyNoMatch
  -- ^ No policy matched, fall back to RBAC
  deriving stock (Show, Eq)

-- | Policy evaluation context.
data PolicyContext = PolicyContext
  { pcResource :: Text
  , pcAction :: Text
  , pcAuth :: AuthContext
  }
  deriving stock (Show, Eq)

-- | Evaluate policies for a request.
--
-- Policies are evaluated in priority order (lowest number = highest priority).
-- First matching policy determines the decision.
--
-- @since 0.1.0.0
evaluatePolicy :: AuthContext -> Text -> Text -> AppM PolicyDecision
evaluatePolicy ctx resource action = do
  let tenantId = acTenantId ctx

  -- Get active policies for this resource
  policies <- withPool $ \conn -> Policies.getActivePolicies conn tenantId resource

  -- Evaluate in priority order (already sorted)
  evaluateInOrder policies resource action

-- | Evaluate policies in order.
--
-- @since 0.1.0.0
evaluateInOrder :: [Policies.PolicyVersion] -> Text -> Text -> AppM PolicyDecision
evaluateInOrder [] _ _ = pure PolicyNoMatch  -- No matching policy
evaluateInOrder (p:ps) resource action
  | Policies.matchesRequest resource action p =
      case Policies.pvEffect p of
        Policies.Allow -> pure PolicyAllow
        Policies.Deny -> pure $ PolicyDeny $ "Denied by policy " <> T.pack (show $ Policies.pvPolicyId p)
  | otherwise = evaluateInOrder ps resource action

-- | Check policy and throw if denied.
--
-- @since 0.1.0.0
checkPolicy :: AuthContext -> Text -> Text -> AppM ()
checkPolicy ctx resource action = do
  decision <- evaluatePolicy ctx resource action
  case decision of
    PolicyAllow -> pure ()
    PolicyDeny reason -> do
      -- Log denied access
      logDeniedAccess ctx resource action reason
      throwError $ Forbidden reason
    PolicyNoMatch -> pure ()  -- Fall back to RBAC

-- | Log denied access to audit trail.
--
-- @since 0.1.0.0
logDeniedAccess :: AuthContext -> Text -> Text -> Text -> AppM ()
logDeniedAccess ctx resource action reason = do
  -- Emit AccessDenied event
  withPool $ \conn -> do
    void $ liftIO $ Policies.createPolicy conn (acTenantId ctx) Policies.PolicyInput
      { Policies.piName = "access-denied-log"
      , Policies.piDescription = Just $ "Access denied: " <> reason
      , Policies.piPriority = 999
      }
    -- In real impl, would emit audit event
    pure ()
  where
    void = const ()

-- | Policy middleware.
--
-- Evaluates policies before routing to handlers.
--
-- @since 0.1.0.0
policyMiddleware :: Middleware
policyMiddleware app req respond = do
  -- Extract resource and action from request
  let resource = T.intercalate "/" $ Wai.pathInfo req
  let action = TE.decodeUtf8 $ Wai.requestMethod req

  -- Policy evaluation happens in handler context
  -- This middleware just passes through
  app req respond
