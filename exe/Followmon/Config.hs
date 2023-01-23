-- SPDX-License-Identifier: MIT
-- Copyright (c) 2023 Chua Hou

{-# LANGUAGE DeriveGeneric #-}

module Followmon.Config (Config (..)) where
import Dhall (FromDhall, Generic, Natural)

data Config = Config
    { twitterBearerToken :: String -- ^ Bearer token for Twitter.
    , intervalSeconds    :: Natural -- ^ Interval in seconds to check.
    , username           :: String -- ^ Username to monitor.
    , filename           :: FilePath -- ^ Filepath for serialized sets.
    } deriving (Generic, Show)
instance FromDhall Config
