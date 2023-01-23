-- SPDX-License-Identifier: MIT
-- Copyright (c) 2023 Chua Hou

module Main (main) where

import Followmon.Config
import Followmon.Log           qualified as Log
import Followmon.Twitter

import Control.Concurrent      (threadDelay)
import Control.Exception       (SomeException, handle, throw)
import Control.Monad           (when, (>=>))
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
            Log.info "Getting initial list of #{Incoming} and #{Outgoing}"
            ins  <- getFollowerIDs cfg.twitterBearerToken cfg.username Incoming
            outs <- getFollowerIDs cfg.twitterBearerToken cfg.username Outgoing
            putStrLn [i|Initialized with #{Set.size ins} #{Incoming} and #{Set.size outs} #{Outgoing}|]
            loop cfg ins outs -- Enter main loop.
        loop :: Config -> Set UserID -> Set UserID -> IO ()
        loop cfg ins outs = do
            let interval = cfg.intervalSeconds
            Log.info [i|Waiting for #{interval} seconds|]
            threadDelay $ fromIntegral interval * 1_000_000
            ins'  <- update Incoming ins
            outs' <- update Outgoing outs
            loop cfg ins' outs'
                where
                    update :: FollowerType -> Set UserID -> IO (Set UserID)
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
                        when (addedN + removedN > 0) $ do
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
                        pure new
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
