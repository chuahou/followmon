-- SPDX-License-Identifier: MIT
-- Copyright (c) 2023 Chua Hou

{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Followmon.Config
import Followmon.Log           qualified as Log
import Followmon.Telegram
import Followmon.Twitter

import Control.Applicative     ((<|>))
import Control.Concurrent      (forkIO, threadDelay)
import Control.Exception       (SomeException, handle, throw)
import Control.Monad           (void, when)
import Data.Binary             (Binary (..), decodeOrFail, encodeFile)
import Data.ByteString.Lazy    qualified as BL
import Data.List               (intercalate)
import Data.Map.Strict         (Map)
import Data.Map.Strict         qualified as Map
import Data.Maybe              (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Set                (Set)
import Data.Set                qualified as Set
import Data.String.Interpolate (i)
import Data.Time               (UTCTime (..), defaultTimeLocale, diffUTCTime,
                                formatTime, fromGregorian, getCurrentTime,
                                parseTimeM, secondsToDiffTime)
import Dhall                   (auto, inputFile)
import Network.HTTP.Simple     (httpNoBody)
import System.Environment      (getArgs)
import System.Exit             (exitFailure)

-- | Cache of user information is a map from 'UserID's to the time we last
-- updated it and its corresponding 'UserJSON'. We store the time last updated
-- so that we can refresh if necessary.
type UserCache = Map UserID (CacheTime, UserJSON)

-- | Type of time at which something was cached. Wrapped in a newtype so that we
-- can serialize it without an orphan instance. When parsing and failing, we
-- return epoch time 1970-01-01T00:00:00Z instead. We only store up to seconds
-- resolution, so actually @decode . encode != id@.
newtype CacheTime = CacheTime { time :: UTCTime } deriving Show
instance Binary CacheTime where
    put (CacheTime t) = put $ formatTime defaultTimeLocale "%s" t
    get = do str <- get
             case parseTimeM False defaultTimeLocale "%s" str of
               Just t -> pure $ CacheTime t
               Nothing -> pure $ CacheTime (UTCTime (fromGregorian 1970 1 1)
                                               (secondsToDiffTime 0))

-- | Type of lists saved to disk.
type FileSaved = (Set UserID, Set UserID, UserCache)

-- | Try to decode a 'FileSaved' from the given filename, returning 'Nothing' if
-- impossible. Goes through a list of past formats, updating them to the latest
-- format if applicable.
decodeFile :: FilePath -> IO (Maybe FileSaved)
decodeFile fname =
        (BL.readFile fname >>= go decoders)
    <|> (Log.err "Failed to read file" >> pure Nothing)
    where

        go :: [BL.ByteString -> Maybe FileSaved]
           -> BL.ByteString -> IO (Maybe FileSaved)
        go [] _       = pure Nothing
        go (d:ds) lbs = case d lbs of
                          Just y  -> pure $ Just y
                          Nothing -> Log.err "Falling back to next decoder"
                                    >> go ds lbs

        decoders :: [BL.ByteString -> Maybe FileSaved]
        decoders = [ \lbs -> case decodeOrFail lbs of
                               Left _          -> Nothing
                               Right (_, _, y) -> Just y
                   , \lbs -> case decodeOrFail lbs of
                               Left _ -> Nothing
                               Right (_, _, (ins, outs)) ->
                                   Just (ins, outs, Map.empty)
                   ]

main :: IO ()
main = getArgs >>= \case
        [x] -> inputFile auto [i|#{x}|] >>= \cfg -> handle (handler cfg) (go cfg)
        _   -> usage
    where
        -- Catch all exceptions and print them both to log and to Telegram
        -- before forwarding them.
        handler :: Config -> SomeException -> IO ()
        handler cfg e = do
            Log.err "Unexpected exception occurred"
            sendMessage cfg.telegramBotToken cfg.telegramChatID $ concat
                [ "*UNEXPECTED ERROR*\n"
                , "Attempting to send error message\\.\n"
                , "May fail if special characters are encountered\\."
                ]
            sendMessage cfg.telegramBotToken cfg.telegramChatID
                [i|```\n#{show e}\n```|]
            throw e

        go :: Config -> IO ()
        go cfg = do
            Log.info [i|Getting initial lists|]
            Log.info $ "Reading lists from file " ++ cfg.filename
            (ins, outs, userCache) <- decodeFile cfg.filename >>= \case
              Just decoded -> pure decoded
              Nothing -> do
                  Log.err "Failed to decode file"
                  Log.info "Getting initial lists from API"
                  ins  <- getFollowerIDs cfg.twitterBearerToken
                            cfg.username Incoming
                  outs <- getFollowerIDs cfg.twitterBearerToken
                            cfg.username Outgoing
                  pure (ins, outs, Map.empty)
            sendMessage cfg.telegramBotToken cfg.telegramChatID
                [i|Initialized with #{Set.size ins} #{Incoming} and #{Set.size outs} #{Outgoing}\\.|]
            loop cfg ins outs userCache -- Enter main loop.

        loop :: Config -> Set UserID -> Set UserID -> UserCache -> IO ()
        loop cfg ins outs userCache = do
            -- Send healthcheck if specified.
            case cfg.healthcheckUrl of
              Nothing  -> pure ()
              Just url -> do
                  Log.info [i|Pinging healthcheck #{url}|]
                  void . forkIO . void $ httpNoBody [i|GET #{url}|]
            -- Loop delay.
            let interval = cfg.intervalSeconds
            Log.info [i|Waiting for #{interval} seconds|]
            threadDelay $ fromIntegral interval * 1_000_000
            -- Update in and out lists.
            (ins',  insAdded,  insRemoved)  <- updateList cfg Incoming ins
            (outs', outsAdded, outsRemoved) <- updateList cfg Outgoing outs
            -- Generate reports of changes. We use the old userCache, because of
            -- the following scenario:
            -- * User obtained in previous cache.
            -- * User was deleted, can no longer be looked up, and is removed
            --   from the follower list.
            -- * userCache contains the user, but userCache' does not.
            -- * report needs userCache and not userCache'.
            insReport <- report cfg userCache Incoming (Set.size ins)
                            insAdded insRemoved
            outsReport <- report cfg userCache Outgoing (Set.size outs)
                            outsAdded outsRemoved
            let mReport = insReport <> outsReport -- Nothing if nothing to report.
            -- Update user cache.
            mNewCache <- updateCache cfg userCache (Set.union ins' outs')
            let userCache' = fromMaybe userCache mNewCache
            -- Save new values to disk if there are updates.
            when (isJust mReport || isJust mNewCache) $ do
                Log.info "Updating file on disk"
                encodeFile cfg.filename (ins', outs', userCache')
            -- Report updates to Telegram if necessary.
            maybe (pure ()) (\msg -> do
                Log.info "Sending update message"
                sendMessage cfg.telegramBotToken cfg.telegramChatID msg
                loop cfg ins' outs' userCache') mReport
            -- Next iteration.
            loop cfg ins' outs' userCache'

-- | Get updated list of the specified type. Returns the new set for that type,
-- tupled with the sets of added and removed user IDs.
updateList :: Config -- ^ Application configuration.
           -> FollowerType -- ^ Type to check.
           -> Set UserID -- ^ Previous set to compare to.
           -> IO (Set UserID, Set UserID, Set UserID)
updateList cfg typ old = do
    Log.info [i|Getting updated list of #{typ}|]
    new <- getFollowerIDs cfg.twitterBearerToken cfg.username typ
    pure (new, new Set.\\ old, old Set.\\ new)

-- | Reports changes in the specified type.
report :: Config -- ^ Application configuration.
       -> UserCache -- ^ User information cache.
       -> FollowerType -- ^ Type to report.
       -> Int -- ^ Previous size of the type's list.
       -> Set UserID -- ^ Added user IDs.
       -> Set UserID -- ^ Removed user IDs.
       -> IO (Maybe String)
report cfg userCache typ oldN added removed = do
    let addedN   = Set.size added
        removedN = Set.size removed
    -- Lookup users we have to report about.
    userMap <- lookupUsersByID cfg.twitterBearerToken . Set.toList $
                Set.union added removed
    -- Supplement userMap with userCache in case some of the removed users were
    -- deleted, meaning we could no longer look them up with the API, making
    -- them missing from userMap.
    let userMap' = Map.union (Map.map snd userCache) userMap
    -- Helper function to look up a user in userMap', returning a generic user
    -- named with its user ID if both lookups fail.
        lookupUser uid = case userMap' Map.!? uid of
                           Just u  -> u
                           Nothing -> UserJSON uid "unknown" [i|#{uid}|]
        addedList   = map lookupUser . Set.toList $ added
        removedList = map lookupUser . Set.toList $ removed
    pure $ if addedN + removedN > 0
              then Just $ concat
                [ printSummary addedN removedN
                , printUsers "+" addedList
                , printUsers "-" removedList
                ]
              else Nothing
    where
        printSummary :: Int -> Int -> String
        printSummary addedN removedN =
            [i|*Changes \\(#{typ}\\)*: #{format "\\+" addedN} #{format "\\-" removedN} #{changeStr}\n#{oldN} → #{oldN + change}\n|]
            where
                format :: String -> Int -> String
                format pre 0 = [i|#{pre}0|]
                format pre n = [i|*#{pre}#{n}*|]
                change = addedN - removedN
                changeStr
                  | change > 0 = [i|*↑#{change}*|]
                  | change < 0 = [i|*↓#{0 - change}*|]
                  | otherwise  = "*\\~0*" :: String
        printUsers :: String -> [UserJSON] -> String
        printUsers _ [] = ""
        printUsers bullet users =
              (<> "\n") . intercalate "\n"
            . map printUser $ users
                where
                    printUser :: UserJSON -> String
                    printUser user =
                        let username = user.username
                            padding = replicate (15 - length username) ' '
                            name = user.name
                         in [i|\\[`#{bullet}`\\]  `@#{escapeMessagePre username}#{padding}` [#{escapeMessage name}](https://twitter.com/#{username})|]

-- | Update user information cache. Goes through the set of users to keep in the
-- cache, taking a value from the old cache if available and not expired, and
-- looking it up in the API otherwise. Returns @Just newCache@ if the cache is
-- updated, and @Nothing@ if there are no updates.
updateCache :: Config -- ^ Application config.
            -> UserCache -- ^ Previous copy of cache.
            -> Set UserID -- ^ Set of users to keep in the cache.
            -> IO (Maybe UserCache)
updateCache cfg userCache uids = do
    let oldSize = Map.size userCache
    Log.info [i|Updating cache, current size #{oldSize}|]
    t <- getCurrentTime
    let uids' = Set.toList uids
        maxDiffTime = fromIntegral cfg.userCacheSeconds
        -- Check each user ID, mapping it to a tuple of itself with @Nothing@ if
        -- we have to look it up, and @Just v@ if we are using the cached value
        -- @v@.
        ys = map (\uid -> do
                case userCache Map.!? uid of
                  Just (ct, u) ->
                      if diffUTCTime t ct.time > maxDiffTime
                         then (uid, Nothing)       -- Expired.
                         else (uid, Just (ct, u))  -- Use cached value.
                  Nothing -> (uid, Nothing)) uids' -- Not cached.
        -- List of user IDs to refresh.
        toRefresh = map fst . filter (isNothing . snd) $ ys
        -- Values to take from the old cache.
        cache = Map.fromList . mapMaybe (\(uid, mu) -> (uid,) <$> mu) $ ys
    case toRefresh of
      [] -> if oldSize == Map.size cache
               then pure Nothing -- Nothing to refresh, cache size did not
                                 -- shrink, meaning nothing was updated.
               else pure $ Just cache
      _ -> do -- We have things to refresh.
        Log.info [i|Updating #{length toRefresh} users|]
        refreshed <- lookupUsersByID cfg.twitterBearerToken toRefresh
        ct <- CacheTime <$> getCurrentTime
        let userCache' = Map.union cache (Map.map (ct,) refreshed)
        Log.info [i|Updated cache, new size #{Map.size userCache'}|]
        pure $ Just userCache'

usage :: IO ()
usage = do
    Log.err "Usage: provide config file as single argument"
    exitFailure
