-- SPDX-License-Identifier: MIT
-- Copyright (c) 2023 Chua Hou

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}

module Followmon.Twitter (UserID, getFollowerIDs) where

import Followmon.Log             qualified as Log

import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson                (FromJSON (..), Object, Value, withObject,
                                  (.:), (.:?))
import Data.Aeson.Types          (Parser, listParser)
import Data.Function             ((&))
import Data.Set                  (Set)
import Data.Set                  qualified as Set
import Data.String.Interpolate   (i)
import GHC.Generics              (Generic)
import Network.HTTP.Simple       (addToRequestQueryString, getResponseBody,
                                  httpJSON, setRequestBearerAuth)

-- | Type of user IDs.
type UserID = String

-- | Type of response to the followers endpoint.
data FollowersJSON = FollowersJSON
    { userIDs   :: [UserID]
    , nextToken :: Maybe String -- ^ Possible pagination token.
    } deriving Show
instance FromJSON FollowersJSON where
    parseJSON = withObject "FollowersJSON" $ \v -> FollowersJSON
        <$> ((v .: "data") >>= listParser parseUser)
        <*> parseMeta v
            where
                parseMeta :: Object -> Parser (Maybe String)
                parseMeta v = runMaybeT $ do
                        meta <- MaybeT $ v .:? "meta"
                        MaybeT $ meta .:? "next_token"
                parseUser :: Value -> Parser UserID
                parseUser = withObject "userID" (.: "id")

-- | JSON object wrapped in a "data" field.
newtype DataWrapper a = DataWrapper { content :: a } deriving Show
instance FromJSON a => FromJSON (DataWrapper a) where
    parseJSON = withObject "DataWrapper" $ \v -> DataWrapper <$> v .: "data"

-- | JSON object containing information about a user.
data UserJSON = UserJSON
    { id       :: String
    , name     :: String
    , username :: String
    } deriving (Generic, Show)
instance FromJSON UserJSON

-- | JSON object containing next_token for pagination. We don't care about any
-- other fields, and we're okay with it being missing.
newtype MetaJSON = MetaJSON { next_token :: Maybe String }
    deriving (Generic, Show)
instance FromJSON MetaJSON

-- | Twitter API v2 base URL.
twitterAPI :: String
twitterAPI = "https://api.twitter.com/2"

-- | Get user IDs of followers of a username. Throws an error in IO if the
-- request fails or a parse error occurs.
getFollowerIDs :: String -- ^ Bearer token.
               -> String -- ^ Username to lookup.
               -> IO (Set UserID)
getFollowerIDs tok username = do
    Log.info [i|Looking up username #{username}|]
    DataWrapper user <- getResponseBody <$> httpJSON
            ([i|GET #{twitterAPI}/users/by/username/#{username}|]
                & setRequestBearerAuth [i|#{tok}|])
                    :: IO (DataWrapper UserJSON)
    Log.info [i|Looked up username #{username}, got #{user}|]
    go user.id Nothing
        where
            -- Look up followers of given user ID, with a possible pagination
            -- token.
            go :: UserID -> Maybe String -> IO (Set UserID)
            go uid mNextTok = do
                let req = [i|GET #{twitterAPI}/users/#{uid}/followers|]
                        & setRequestBearerAuth [i|#{tok}|]
                        & addToRequestQueryString [("max_results", Just "1000")]
                let req' = maybe req (\nextTok ->
                            req & addToRequestQueryString
                                [("pagination_token", Just [i|#{nextTok}|])])
                            mNextTok -- Add pagination token if necessary.
                Log.info [i|Requesting followers of #{uid}|]
                response <- getResponseBody <$> httpJSON req'
                            :: IO FollowersJSON
                let followers = Set.fromList response.userIDs
                Log.info [i|Received #{Set.size followers} followers|]
                case response.nextToken of
                  Just nextTok -> Set.union followers <$> go uid (Just nextTok)
                  Nothing      -> pure followers
