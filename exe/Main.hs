-- SPDX-License-Identifier: MIT
-- Copyright (c) 2023 Chua Hou

module Main (main) where

import Followmon.Config
import Followmon.Log           qualified as Log
import Followmon.Twitter

import Control.Applicative     ((<|>))
import Control.Concurrent      (threadDelay)
import Control.Exception       (SomeException, handle, throw)
import Control.Monad           (when, (>=>))
import Data.Binary             (decodeOrFail, encodeFile)
import Data.ByteString.Lazy    qualified as BL
import Data.Set                (Set)
import Data.Set                qualified as Set
import Data.String.Interpolate (i)
import Dhall                   (auto, inputFile)
import System.Environment      (getArgs)
import System.Exit             (exitFailure)

main :: IO ()
main = getArgs >>= \case
        [x] -> inputFile auto [i|#{x}|] >>= handle handler . go
        _   -> usage
    where
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
                Right (_, _, y) -> pure y
            putStrLn [i|Initialized with #{Set.size ins} #{Incoming} and #{Set.size outs} #{Outgoing}|]
            loop cfg ins outs -- Enter main loop.
        loop :: Config -> Set UserID -> Set UserID -> IO ()
        loop cfg ins outs = do
            let interval = cfg.intervalSeconds
            Log.info [i|Waiting for #{interval} seconds|]
            threadDelay $ fromIntegral interval * 1_000_000
            (ins',  insChanged)  <- update Incoming ins
            (outs', outsChanged) <- update Outgoing outs
            when (insChanged || outsChanged) $ do
                Log.info "Updating file on disk"
                encodeFile cfg.filename (ins', outs')
            loop cfg ins' outs'
                where
                    update :: FollowerType -> Set UserID
                           -> IO (Set UserID, Bool)
                    update typ old = do
                        Log.info [i|Getting updated list of #{typ}|]
                        new <- getFollowerIDs cfg.twitterBearerToken
                                    cfg.username typ
                        let added = new Set.\\ old
                            addedN = Set.size added
                            removed = old Set.\\ new
                            removedN = Set.size removed
                            change = addedN - removedN -- Net change.
                            arrow -- Arrow to display net change.
                              | change > 0 = "↑" :: String
                              | change < 0 = "↓"
                              | otherwise = "~"
                            changed = addedN + removedN > 0
                        when changed $ do
                            Log.info "Reporting changes in #{typ}"
                            putStrLn
                                [i|Summary (#{typ}): +#{addedN} -#{removedN} (#{arrow}#{abs change})|]
                            when (addedN > 0) $ do
                                putStrLn $ case typ of
                                  Incoming -> "Gained followers:"
                                  Outgoing -> "Newly followed:"
                                printUsers added
                            when (removedN > 0) $ do
                                putStrLn $ case typ of
                                  Incoming -> "Lost followers:"
                                  Outgoing -> "Stopped following:"
                                printUsers removed
                        pure (new, changed)
                    printUsers :: Set UserID -> IO ()
                    printUsers =
                        lookupUsersByID cfg.twitterBearerToken . Set.toList
                        >=> mapM_ (\user -> putStrLn $
                            let username = user.username
                                name = user.name
                             in [i|- @#{username} #{name}|])

        -- Catch all exceptions and print them before forwarding them.
        handler :: SomeException -> IO ()
        handler e = Log.err "Unexpected exception occurred" >> throw e

usage :: IO ()
usage = do
    Log.err "Usage: provide config file as single argument"
    exitFailure
