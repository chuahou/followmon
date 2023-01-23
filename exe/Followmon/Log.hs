-- SPDX-License-Identifier: MIT
-- Copyright (c) 2023 Chua Hou

module Followmon.Log (info, err) where

import System.IO (hPutStrLn, stderr)

-- | Log a message to stderr.
logMsg :: String -- ^ Message to log.
       -> IO ()
logMsg = hPutStrLn stderr

-- | Log a message with INFO severity.
info :: String -- ^ Message to log.
     -> IO ()
info = logMsg . ("[INFO] " <>)

-- | Log a message with ERROR severity.
err :: String -- ^ Message to log.
    -> IO ()
err = logMsg . ("[ERR!] " <>)
