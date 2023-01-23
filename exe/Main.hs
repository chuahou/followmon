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
            Log.info "Getting initial list of followers"
            fs <- getFollowerIDs cfg.twitterBearerToken cfg.username
            putStrLn [i|Initialized with #{Set.size fs} followers|]
            loop cfg fs -- Enter main loop.
        loop :: Config -> Set UserID -> IO ()
        loop cfg fs = do
            let interval = cfg.intervalSeconds
            Log.info [i|Waiting for #{interval} seconds|]
            threadDelay $ fromIntegral interval * 1_000_000
            Log.info "Getting updated list of followers"
            fs' <- getFollowerIDs cfg.twitterBearerToken cfg.username
            let added = fs' Set.\\ fs
                addedN = Set.size added
                removed = fs Set.\\ fs'
                removedN = Set.size removed
                change = addedN - removedN
                arrow | change > 0 = "↑" :: String
                      | change < 0 = "↓"
                      | otherwise = "~"
            when (addedN + removedN > 0) $ do
                putStrLn
                    [i|Summary: +#{addedN} -#{removedN} (#{arrow}#{abs change})|]
                when (addedN > 0) $ do
                    putStrLn "Gained followers:"
                    printUsers added
                when (removedN > 0) $ do
                    putStrLn "Lost followers:"
                    printUsers removed
            loop cfg fs'
                where
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
