{-# LANGUAGE OverloadedStrings #-}

module Net.Monad.Shitpost.Core
    ( urlEncodeParams
    , makeRequest
    , from
    , responseOpen
    , responseClose
    , authHeader
    , getRequest
    , postRequest
    )
where

import Prelude hiding (map, null, filter, break)
import qualified Prelude as P

import Net.OAuth.OAuth10a
import Net.Monad.Shitpost.Types

import Tubes

import Data.ByteString (ByteString, null, append, empty)
import Data.ByteString.Char8 (break, unpack, pack)
import qualified Data.ByteString.Builder as BB
import Network.HTTP.Client hiding (responseOpen, responseClose)
import qualified Network.HTTP.Client as H
import Network.HTTP.Types
import Control.Monad.IO.Class
import Control.Monad (forever)
import Control.Monad.Trans (lift)
import Data.Monoid ((<>))

urlEncodeParams :: [Param] -> Request -> Request
urlEncodeParams params req = urlEncodedBody params' req where
    params' = fmap (\(Param k v) -> (k,v)) params

-- | Sends a managed HTTP request and receives a 'Source' via callback
makeRequest
    :: Request
    -> (ResponseStream -> Shitpost a)
    -> Shitpost a
makeRequest r k = do
    response <- responseOpen r
    result <- k response
    responseClose response
    return result

from :: IO ByteString -> Source Shitpost ByteString
from io = Source loop where
    loop = do
        bs <- liftIO io
        if null bs
           then halt
            else (yield bs) >> loop

responseOpen
    :: Request
    -> Shitpost ResponseStream
responseOpen req = do
    man <- getManager
    res <- liftIO $ H.responseOpen req man
    return $ fmap (from . brRead) res

responseClose
    :: Response a
    -> Shitpost ()
responseClose = liftIO . H.responseClose

authHeader
    :: ByteString -- ^ method
    -> ByteString -- ^ url
    -> [Param]    -- ^ any extra parameters
    -> Shitpost ByteString
authHeader method url extras = do
    credentials <- getCredentials
    auth_header credentials method url extras

-- | Construct a GET request with the appropriate headers
getRequest
    :: String -- ^ URL
    -> [Param] -- ^ Request parameters
    -> Shitpost Request
getRequest url params = do
    let queryString = "?"++(unpack $ param_string params)
    initialRequest <- liftIO $ parseRequest $ "GET " ++ url ++ queryString
    ah <- authHeader "GET" (pack url) params
    return $ initialRequest {
        requestHeaders =
                [("Authorization", ah)
                ,("Content-Type", "multipart/form-data")
                ,("Accept","*/*")
                ,("User-Agent","shitpost")]
        }

-- | Construct a POST request with the appropriate headers
postRequest
    :: String -- ^ URL
    -> [Param] -- ^ Request parameters
    -> Shitpost Request
postRequest url params = do
    initialRequest <- liftIO $ parseRequest $ "POST " ++ url
    ah <- authHeader "POST" (pack url) params
    return $ urlEncodeParams params $ initialRequest {
        requestHeaders =
                [("Authorization", ah)
                ,("Content-Type", "multipart/form-data")
                ,("Accept","*/*")
                ,("User-Agent","shitpost")]
        }
