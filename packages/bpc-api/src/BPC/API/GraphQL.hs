{-# LANGUAGE OverloadedStrings #-}

-- | GraphQL Endpoint (MVP)
--
-- Basic GraphQL endpoint for passport queries.
--
-- @since 0.1.0.0
module BPC.API.GraphQL
  ( graphqlHandler
  , GraphQLRequest(..)
  , GraphQLResponse(..)
  ) where

import Control.Monad.Except (throwError)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import BPC.API.App (AppM)
import BPC.API.Error (AppError(..))
import BPC.API.Types (AuthContext(..))

-- | GraphQL request.
data GraphQLRequest = GraphQLRequest
  { gqlQuery :: Text
  , gqlVariables :: Maybe Value
  , gqlOperationName :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GraphQLRequest where
  parseJSON = Aeson.withObject "GraphQLRequest" $ \o -> GraphQLRequest
    <$> o .: "query"
    <*> o .:? "variables"
    <*> o .:? "operationName"

-- | GraphQL response.
data GraphQLResponse = GraphQLResponse
  { gqlData :: Maybe Value
  , gqlErrors :: Maybe [GraphQLError]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GraphQLResponse where
  toJSON GraphQLResponse{..} = object $
    maybe [] (\d -> ["data" .= d]) gqlData ++
    maybe [] (\e -> ["errors" .= e]) gqlErrors

-- | GraphQL error.
data GraphQLError = GraphQLError
  { geMessage :: Text
  , gePath :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GraphQLError where
  toJSON GraphQLError{..} = object $
    ["message" .= geMessage] ++
    maybe [] (\p -> ["path" .= p]) gePath

-- | GraphQL handler.
--
-- POST /v1/graphql
-- Executes GraphQL query and returns result.
--
-- MVP implementation supports basic passport queries.
--
-- @since 0.1.0.0
graphqlHandler :: AuthContext -> GraphQLRequest -> AppM GraphQLResponse
graphqlHandler ctx req = do
  -- MVP: Basic query parsing
  -- Real implementation would use a GraphQL library
  case parseQuery (gqlQuery req) of
    Just "passports" -> do
      -- Return placeholder data
      pure GraphQLResponse
        { gqlData = Just $ object ["passports" .= ([] :: [Value])]
        , gqlErrors = Nothing
        }
    Just "passport" -> do
      pure GraphQLResponse
        { gqlData = Just $ object ["passport" .= Aeson.Null]
        , gqlErrors = Nothing
        }
    _ -> do
      pure GraphQLResponse
        { gqlData = Nothing
        , gqlErrors = Just [GraphQLError "Query not supported in MVP" Nothing]
        }
  where
    parseQuery :: Text -> Maybe Text
    parseQuery q
      | "passports" `T.isInfixOf` q = Just "passports"
      | "passport" `T.isInfixOf` q = Just "passport"
      | otherwise = Nothing
