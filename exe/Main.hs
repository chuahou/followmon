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
import Control.Monad           (void)
import Data.Binary             (Binary (..), decodeOrFail, encodeFile)
import Data.ByteString.Lazy    qualified as BL
import Data.List               (intercalate)
import Data.Map.Strict         (Map)
import Data.Map.Strict         qualified as Map
import Data.Maybe              (isNothing, mapMaybe)
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
            -- We used to store (ins, outs) only, but now we update to
            -- (ins, outs, userCache). If we fail to decode as
            -- (ins, outs, userCache), we will try to read the old format and
            -- overwrite it in the new format.
            lbs <- BL.readFile cfg.filename
                    <|> (Log.err "Failed to read file" >> pure "")
            (ins, outs, userCache) <- case decodeOrFail lbs of
                Left _ -> do
                    Log.err "Failed to decode file"
                    Log.info "Attempting to decode legacy file"
                    case decodeOrFail lbs of
                      Left _ -> do
                          Log.err "Failed to decode legacy file"
                          Log.info "Getting initial lists from API"
                          ins  <- getFollowerIDs cfg.twitterBearerToken
                                      cfg.username Incoming
                          outs <- getFollowerIDs cfg.twitterBearerToken
                                      cfg.username Outgoing
                          Log.info "Writing initial lists to disk"
                          encodeFile cfg.filename (ins, outs)
                          pure (ins, outs, Map.empty)
                      Right (_, _, (ins, outs)) -> do
                          Log.info "Decoded initial lists from legacy file"
                          Log.info "Updating file to new format"
                          let newFormat = (ins, outs, Map.empty :: UserCache)
                          encodeFile cfg.filename newFormat
                          pure newFormat
                Right (_, _, y) -> do
                    Log.info "Decoded initial lists from file"
                    pure y
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
            (ins',  insMsg)  <- update Incoming ins
            (outs', outsMsg) <- update Outgoing outs
            -- Update user cache.
            userCache' <- updateCache (Set.union ins' outs')
            -- Save new values to disk.
            Log.info "Updating file on disk"
            encodeFile cfg.filename (ins', outs', userCache')
            -- Check if we have any updates. If so, report the updates to
            -- Telegram.
            let msg = insMsg <> outsMsg
            case msg of
                  Nothing -> loop cfg ins' outs' userCache' -- No updates.
                  Just msg' -> do
                      Log.info "Sending update message"
                      sendMessage cfg.telegramBotToken cfg.telegramChatID msg'
                      loop cfg ins' outs' userCache'
                where
                    -- Get updated list of the specified type, and report
                    -- changes. Returns the new set and Just msg if there's an
                    -- update message msg, and Nothing if there are no changes.
                    update :: FollowerType -- Type to check.
                           -> Set UserID -- Old set to compare to.
                           -> IO (Set UserID, Maybe String)
                    update typ old = do
                        Log.info [i|Getting updated list of #{typ}|]
                        new <- getFollowerIDs cfg.twitterBearerToken
                                    cfg.username typ
                        let added = new Set.\\ old
                            addedN = Set.size added
                            removed = old Set.\\ new
                            removedN = Set.size removed
                        userMap <- lookupUsersByID cfg.twitterBearerToken
                                 . Set.toList $ Set.union added removed
                        -- Looked-up users supplemented with user cache, in case
                        -- some of those users were deleted since we cached them
                        -- and hence looking them up failed.
                        let userMap' = Map.union
                                        (Map.map snd userCache) userMap
                        let lookupUser m uid = case m Map.!? uid of
                                                 Just u -> u
                                                 Nothing -> UserJSON uid
                                                                "unknown"
                                                                [i|#{uid}|]
                            added' = map (lookupUser userMap') $ Set.toList added
                            removed' = map (lookupUser userMap') $ Set.toList removed
                        pure $ if addedN + removedN > 0
                            then (new, Just $ concat
                                [ printSummary typ addedN removedN (Set.size old)
                                , printUsers "+" added'
                                , printUsers "-" removed'
                                , "\n"
                                ])
                            else (new, Nothing)

                    -- Updated user cache. Given the new set of users, it checks
                    -- the old cache and uses those values if not expired, and
                    -- looks them up otherwise.
                    updateCache :: Set UserID -> IO UserCache
                    updateCache set = do
                        Log.info [i|Updating cache, current size #{Map.size userCache}|]
                        t <- getCurrentTime
                        let uids = Set.toList set
                            maxDiffTime = fromIntegral cfg.userCacheSeconds
                            ys = map (\uid -> do
                                    case userCache Map.!? uid of
                                      Just (ct, u) ->
                                          if diffUTCTime t ct.time > maxDiffTime
                                             then (uid, Nothing)
                                             else (uid, Just (ct, u))
                                      Nothing -> (uid, Nothing)) uids
                            toRefresh = map fst . filter (isNothing . snd) $ ys
                            cache = Map.fromList
                                  . mapMaybe (\(uid, mu) -> (uid,) <$> mu)
                                  $ ys
                        Log.info [i|Updating #{length toRefresh} users|]
                        userMap <- lookupUsersByID cfg.twitterBearerToken
                                    toRefresh
                        ct <- CacheTime <$> getCurrentTime
                        let userCache' = Map.union cache (Map.map (ct,) userMap)
                        Log.info [i|Updated cache, new size #{Map.size userCache'}|]
                        pure userCache'

                    -- Formats summary line given type, number added, number
                    -- removed, and size of set before change.
                    printSummary :: FollowerType -> Int -> Int -> Int -> String
                    printSummary typ addedN removedN oldN =
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

                    -- Formats a user nicely given a bullet point to use and a
                    -- user JSON object. We pad usernames to 15 characters.
                    printUser :: String -> UserJSON -> String
                    printUser bullet user =
                        let username = user.username
                            padding = replicate (15 - length username) ' '
                            name = user.name
                         in [i|\\[`#{bullet}`\\]  `@#{escapeMessagePre username}#{padding}` [#{escapeMessage name}](https://twitter.com/#{username})|]

                    -- Formats list of users with bullet points, returning empty
                    -- string if list is empty.
                    printUsers :: String -> [UserJSON] -> String
                    printUsers _ [] = ""
                    printUsers bullet users =
                        (<> "\n") . intercalate "\n"
                      . map (printUser bullet) $ users

usage :: IO ()
usage = do
    Log.err "Usage: provide config file as single argument"
    exitFailure
