-- SPDX-License-Identifier: MIT
-- Copyright (c) 2023 Chua Hou

module Followmon.Network (makeJSONRequest) where

import Control.Exception    (Exception (..), SomeException)
import Data.Aeson           (FromJSON)
import Data.Bifunctor       (first)
import Data.ByteString.UTF8 qualified as BSU
import Network.HTTP.Simple  (QueryItem, getResponseBody, httpJSONEither,
                             parseRequest, setRequestQueryString)

-- | Make a request given a request string and query parameters, and parse the
-- results as JSON. When an error occurs, whether in the parsing of the request
-- string, making the request, or parsing the response, it is returned as a
-- 'SomeException' in 'Left'.
makeJSONRequest :: FromJSON a
            => String -- ^ Request as string, for input to parseRequest.
            -> [(String, String)] -- ^ Query parameters to add.
            -> IO (Either SomeException a)
                -- ^ Either response parsed from JSON, or an error message.
makeJSONRequest requestString queryParams =
    case parseRequest requestString of
        Left err -> pure $ Left err -- Parse error.
        Right req -> do
            let req' = setRequestQueryString (map toQueryItem queryParams) req
            first toException . getResponseBody <$> httpJSONEither req'
                -- We need to convert JSONException to SomeException.
    where
        toQueryItem :: (String, String) -> QueryItem
        toQueryItem (k, v) = (BSU.fromString k, pure . BSU.fromString $ v)
