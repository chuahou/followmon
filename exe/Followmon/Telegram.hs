-- SPDX-License-Identifier: MIT
-- Copyright (c) 2023 Chua Hou

module Followmon.Telegram (sendMessage) where

import Followmon.Log           qualified as Log

import Data.Aeson              (FromJSON)
import Data.Function           ((&))
import Data.String.Interpolate (i)
import GHC.Generics            (Generic)
import Network.HTTP.Simple     (addToRequestQueryString, getResponseBody,
                                httpJSON)

-- | Type of response to the sendMessage endpoint.
data ResponseJSON = ResponseJSON
    { ok          :: Bool
    , description :: Maybe String -- ^ Optional error description.
    } deriving (Generic, Show)
instance FromJSON ResponseJSON

-- | Send a message to a chat, logging an error silently if it fails. Uses
-- MarkdownV2 formatting.
sendMessage :: String -- ^ Bot token.
            -> String -- ^ Chat ID.
            -> String -- ^ Message text.
            -> IO ()
sendMessage tok chatID msg = do
    let req = [i|GET https://api.telegram.org/bot#{tok}/sendMessage|]
                & addToRequestQueryString
                    [ ("chat_id", Just [i|#{chatID}|])
                    , ("text", Just [i|#{msg}|])
                    , ("parse_mode", Just "MarkdownV2")
                    ]
    Log.info [i|Sending message #{msg} to chat ID #{chatID}|]
    response <- getResponseBody <$> httpJSON req :: IO ResponseJSON
    if response.ok
       then Log.info "Successfully sent message"
       else Log.err $ case response.description of
        Just d  -> [i|Failed to send message with error description #{d}|]
        Nothing -> "Failed to send message, no error description received"
