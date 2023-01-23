-- SPDX-License-Identifier: MIT
-- Copyright (c) 2023 Chua Hou

module Main (main) where

import Followmon.Config
import Followmon.Log           qualified as Log
import Followmon.Telegram
import Followmon.Twitter

import Control.Applicative     ((<|>))
import Control.Concurrent      (forkIO, threadDelay)
import Control.Exception       (SomeException, handle, throw)
import Control.Monad           (void)
import Data.Binary             (decodeOrFail, encodeFile)
import Data.ByteString.Lazy    qualified as BL
import Data.List               (intercalate)
import Data.Set                (Set)
import Data.Set                qualified as Set
import Data.String.Interpolate (i)
import Dhall                   (auto, inputFile)
import Network.HTTP.Simple     (httpNoBody)
import System.Environment      (getArgs)
import System.Exit             (exitFailure)

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
            Log.info [i|Getting initial lists of #{Incoming} and #{Outgoing}|]
            Log.info $ "Reading lists from file " ++ cfg.filename
            lbs <- BL.readFile cfg.filename
                    <|> (Log.err "Failed to read file" >> pure "")
            (ins, outs) <- case decodeOrFail lbs of
                Left _ -> do
                    Log.err "Failed to decode file"
                    Log.info "Getting initial lists from API"
                    ins  <- getFollowerIDs cfg.twitterBearerToken
                                cfg.username Incoming
                    outs <- getFollowerIDs cfg.twitterBearerToken
                                cfg.username Outgoing
                    Log.info "Writing initial lists to disk"
                    encodeFile cfg.filename (ins, outs)
                    pure (ins, outs)
                Right (_, _, y) -> do
                    Log.info "Decoded initial lists from file"
                    pure y
            sendMessage cfg.telegramBotToken cfg.telegramChatID
                [i|Initialized with #{Set.size ins} #{Incoming} and #{Set.size outs} #{Outgoing}\\.|]
            loop cfg ins outs -- Enter main loop.
        loop :: Config -> Set UserID -> Set UserID -> IO ()
        loop cfg ins outs = do
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
            -- Check if we have any updates. If so, save the new values to disk,
            -- and report the updates to Telegram.
            let msg = insMsg <> outsMsg
            case msg of
                  Nothing -> loop cfg ins' outs' -- No updates.
                  Just msg' -> do
                      Log.info "Updating file on disk"
                      encodeFile cfg.filename (ins', outs')
                      Log.info "Sending update message"
                      sendMessage cfg.telegramBotToken cfg.telegramChatID msg'
                      loop cfg ins' outs'
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
                        added' <- lookupUsersByID cfg.twitterBearerToken
                                . Set.toList $ added
                        removed' <- lookupUsersByID cfg.twitterBearerToken
                                  . Set.toList $ removed
                        pure $ if addedN + removedN > 0
                            then (new, Just $ concat
                                [ printSummary typ addedN removedN (Set.size old)
                                , printUsers "+" added'
                                , printUsers "-" removed'
                                ])
                            else (new, Nothing)

                    -- Formats summary line given type, number added, number
                    -- removed, and size of set before change.
                    printSummary :: FollowerType -> Int -> Int -> Int -> String
                    printSummary typ addedN removedN oldN =
                        [i|*Changes \\(#{typ}\\)*: #{format "\\+" addedN} #{format "\\-" removedN} #{changeStr}\n#{oldN} → #{oldN + change}|]
                        where
                            format :: String -> Int -> String
                            format pre 0 = [i|#{pre}0|]
                            format pre n = [i|*#{pre}#{n}*|]
                            change = addedN - removedN
                            changeStr
                              | change > 0 = [i|*↑#{change}*|]
                              | change < 0 = [i|*↓#{0 - change}*|]
                              | otherwise  = "*~0*" :: String

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
                        ('\n' :) . (<> "\n\n") . intercalate "\n"
                      . map (printUser bullet) $ users

usage :: IO ()
usage = do
    Log.err "Usage: provide config file as single argument"
    exitFailure
