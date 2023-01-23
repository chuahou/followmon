-- SPDX-License-Identifier: MIT
-- Copyright (c) 2023 Chua Hou

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}

module Followmon.Twitter (UserID, getFollowerIDs) where

import Followmon.Log           qualified as Log

import Data.Aeson              (FromJSON (..), withObject, (.:))
import Data.Function           ((&))
import Data.Set                (Set)
import Data.Set                qualified as Set
import Data.String.Interpolate (i)
import GHC.Generics            (Generic)
import Network.HTTP.Simple     (addToRequestQueryString, getResponseBody,
                                httpJSON, setRequestBearerAuth)

-- | Type of user IDs.
type UserID = Integer

-- | Type of response to the followers endpoint.
data FollowersJSON = FollowersJSON
    { ids             :: [UserID]
    , next_cursor_str :: Maybe String -- ^ Possible pagination token.
    } deriving (Generic, Show)
instance FromJSON FollowersJSON

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

-- | Twitter API base URL.
twitterAPI :: String
twitterAPI = "https://api.twitter.com"

-- | Get user IDs of followers of a username. Throws an error in IO if the
-- request fails or a parse error occurs.
getFollowerIDs :: String -- ^ Bearer token.
               -> String -- ^ Username to lookup.
               -> IO (Set UserID)
getFollowerIDs tok username = go Nothing
    where
        go :: Maybe String -- ^ Possible next cursor.
           -> IO (Set UserID)
        go mCursor = do
            let count = 5000 -- How many to request each time.
            let req = [i|GET #{twitterAPI}/1.1/followers/ids.json|]
                    & setRequestBearerAuth [i|#{tok}|]
                    & addToRequestQueryString
                        [ ("screen_name", Just [i|#{username}|])
                        , ("count", Just [i|#{count}|])
                        ]
            let req' = maybe req (\cursor ->
                        req & addToRequestQueryString
                            [("cursor", Just [i|#{cursor}|])])
                        mCursor -- Add pagination token if necessary.
            Log.info [i|Requesting followers of #{username}|]
            response <- getResponseBody <$> httpJSON req'
                        :: IO FollowersJSON
            let followers = Set.fromList response.ids
            let nReceived = Set.size followers
            Log.info [i|Received #{nReceived} followers|]
            -- If we have a next cursor, and we got less than expected, go to
            -- the next page.
            case (response.next_cursor_str, nReceived == count) of
              (Just cursor, True) -> Set.union followers <$> go (Just cursor)
              _                   -> pure followers
