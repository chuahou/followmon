-- SPDX-License-Identifier: MIT
-- Copyright (c) 2023 Chua Hou

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Followmon.Twitter
    ( UserID
    , UserJSON (..)
    , FollowerType (..)
    , getFollowerIDs
    , lookupUsersByID
    ) where

import Followmon.Log           qualified as Log

import Control.Concurrent      (threadDelay)
import Control.Exception       (handle, throw)
import Control.Monad           (when)
import Data.Aeson              (FromJSON (..), withObject, (.:))
import Data.ByteString.UTF8    qualified as BSU
import Data.Function           ((&))
import Data.List               (intercalate)
import Data.Set                (Set)
import Data.Set                qualified as Set
import Data.String.Interpolate (i)
import Data.Time               (UTCTime, defaultTimeLocale, diffUTCTime,
                                parseTimeM)
import Data.Time.Clock         (getCurrentTime)
import GHC.Generics            (Generic)
import Network.HTTP.Simple     (JSONException (..), addToRequestQueryString,
                                getResponseBody, getResponseHeader,
                                getResponseStatusCode, httpJSON,
                                setRequestBearerAuth)

-- | Type of user IDs.
type UserID = String

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

-- | Type of list to request. We use this as an argument to request functions
-- since they have similar API interfaces.
data FollowerType = Incoming -- ^ Followers.
              | Outgoing -- ^ Followees/friends.
instance Show FollowerType where
    show Incoming = "followers"
    show Outgoing = "followees"

-- | Get user IDs of followers or followees of a username. Throws an error in IO
-- if the request fails or a parse error occurs.
getFollowerIDs :: String -- ^ Bearer token.
               -> String -- ^ Username to lookup.
               -> FollowerType -- ^ Which list to get.
               -> IO (Set UserID)
getFollowerIDs tok username typ = go Nothing
    where
        go :: Maybe String -- ^ Possible next cursor.
           -> IO (Set UserID)
        go mCursor = do
            let count = 5_000 -- How many to request each time.
            let endpoint = case typ of
                             Incoming -> "followers"
                             Outgoing -> "friends" :: String
            let req = [i|GET #{twitterAPI}/1.1/#{endpoint}/ids.json|]
                    & setRequestBearerAuth [i|#{tok}|]
                    & addToRequestQueryString
                        [ ("screen_name", Just [i|#{username}|])
                        , ("count", Just [i|#{count}|])
                        , ("stringify_ids", Just "true")
                        ]
            let req' = maybe req (\cursor ->
                        req & addToRequestQueryString
                            [("cursor", Just [i|#{cursor}|])])
                        mCursor -- Add pagination token if necessary.
            Log.info [i|Requesting #{typ} of #{username}|]
            -- Make request, waiting for rate limit if necessary.
            response <- waitForRateLimit $ getResponseBody <$> httpJSON req'
                        :: IO FollowersJSON
            let followers = Set.fromList response.ids
            let nReceived = Set.size followers
            Log.info [i|Received #{nReceived} #{typ}|]
            -- If we have a next cursor, and we got less than expected, go to
            -- the next page.
            case (response.next_cursor_str, nReceived == count) of
              (Just cursor, True) -> Set.union followers <$> go (Just cursor)
              _                   -> pure followers

-- | Wait for rate limit reset if we get HTTP 429 with a @x-rate-limit-reset@
-- response header while performing an IO action. This should present itself as
-- a 'JSONConversionException' since we expect to use this with a 'httpJSON'
-- call.
waitForRateLimit :: forall a. IO a -> IO a
waitForRateLimit action = handle handler action
    where
        handler :: JSONException -> IO a
        handler e@(JSONConversionException _ response _) =
            case ( getResponseStatusCode response
                 , getResponseHeader "x-rate-limit-reset" response
                 ) of
                (429, [resetTime]) -> do
                    Log.info [i|Rate limit timeout till #{resetTime}|]
                    parseTimeM True defaultTimeLocale "%s"
                        (BSU.toString resetTime) >>= delayUntil
                    action -- Try again.
                        where
                            -- Delay until given time.
                            delayUntil :: UTCTime -> IO ()
                            delayUntil t = do
                                currentTime <- getCurrentTime
                                let delay = diffUTCTime t currentTime
                                when (delay > 0) $ do
                                    Log.info [i|Waiting #{delay} seconds|]
                                    threadDelay (ceiling delay * 1_000_000)
                                    delayUntil t -- Make sure done.
                _ -> throw e
        -- Forward all other exceptions.
        handler e = throw e

-- | Lookup a list of users by their user IDs.
lookupUsersByID :: String -- ^ Bearer token.
                -> [UserID] -- ^ List of user IDs.
                -> IO [UserJSON]
lookupUsersByID tok ids = do
    Log.info [i|Looking up #{length ids} users|]
    users <- fmap concat . mapM lookupGroup . groupIds $ ids
    Log.info [i|Looked up #{length users} users|]
    pure users
    where
        -- Split IDs into groups of 100.
        groupIds :: [UserID] -> [[UserID]]
        groupIds [] = []
        groupIds xs = let (ys, xs') = splitAt 100 xs
                       in ys : groupIds xs'
        lookupGroup :: [UserID] -> IO [UserJSON]
        lookupGroup ids' = do
            Log.info [i|Requesting #{length ids'} users|]
            let usersParam = intercalate "," ids'
            let req = [i|GET #{twitterAPI}/2/users|]
                    & setRequestBearerAuth [i|#{tok}|]
                    & addToRequestQueryString [("ids", Just [i|#{usersParam}|])]
            DataWrapper users <- waitForRateLimit $
                                    getResponseBody <$> httpJSON req
            pure users
