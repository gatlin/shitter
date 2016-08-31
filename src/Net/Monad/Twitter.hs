{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Net.Monad.Twitter
Description : Twitter API utilities
Copyright   : 2016
License     : GPLv3

Maintainer  : Gatlin Johnson <gatlin@niltag.net>
Stability   : experimental
Portability : non-portable

This module implements functions and commands for the 'Twitter' monad so that
Twitter bots may be programmed with ease.
-}

module Net.Monad.Twitter
    ( -- * Twitter monad
      Twitter(..)
    , runTwitter
    , tweet
    , tweet'
    , getHomeTimeline
    , getHomeTimeline'
    , getUserTimeline
    , getUserTimeline'
    , userStream
    , userStream'
    , publicStream
      -- * Miscellaneous
    , Credentials(..)
    , Param(..)
    -- * Re-exports
    , lift
    , liftIO
    , Network.HTTP.Types.Status
    , statusCode
    )
where

import Prelude hiding (map, null, filter, break)
import qualified Prelude as P

import Net.Monad.Twitter.OAuth
import Net.Monad.Twitter.Types

import Tubes

import Data.ByteString (ByteString, null, append, empty)
import Data.ByteString.Char8 (break, unpack, pack)
import qualified Data.ByteString.Builder as BB
import Network.HTTP.Client hiding (responseOpen, responseClose)
import qualified Network.HTTP.Client as H
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types
import System.IO (stdout)
import Control.Monad.IO.Class
import Control.Monad (forever)
import Control.Monad.Trans (lift)
import Data.Monoid ((<>))
import Data.List (intercalate)

urlRESTBase :: String
urlRESTBase = "https://api.twitter.com/1.1/"

urlEncodeParams :: [Param] -> Request -> Request
urlEncodeParams params req = urlEncodedBody params' req where
    params' = fmap (\(Param k v) -> (k,v)) params

-- | Sends a managed HTTP request and receives a 'Source' via callback.
makeRequest
    :: Request
    -> (ResponseStream -> Twitter a)
    -> Twitter a
makeRequest r k = do
    response <- responseOpen r
    result <- k response
    responseClose response
    return result

from :: IO ByteString -> Source Twitter ByteString
from io = Source loop where
    loop = do
        bs <- liftIO io
        if null bs
            then halt
            else (yield bs) >> loop

responseOpen
    :: Request
    -> Twitter ResponseStream
responseOpen req = do
    man <- getManager
    res <- liftIO $ H.responseOpen req man
    return $ fmap (from . brRead) res

responseClose
    :: Response a
    -> Twitter ()
responseClose = liftIO . H.responseClose

-- | Construct a GET request with the appropriate headers
getRequest
    :: String -- ^ URL
    -> [Param] -- ^ Request parameters
    -> Twitter Request
getRequest url params = do
    -- convert the parameters into a query string
    let queryString = "?"++(unpack $ param_string params)
    initialRequest <- liftIO $ parseRequest $ "GET "++ url ++ queryString
    ah <- auth_header "GET" (pack url) params
    return $ initialRequest {
            requestHeaders =
                    [("Authorization", ah)
                    ,("Content-type", "multipart/form-data")
                    ,("Accept", "*/*")
                    ,("User-Agent","undershare")]
            }

-- | Construct a POST request with the appropriate headers
postRequest
    :: String -- ^ URL
    -> [Param] -- ^ Request parameters
    -> Twitter Request
postRequest url params = do
    initialRequest <- liftIO $ parseRequest $ "POST " ++ url
    ah <- auth_header "POST" (pack url) params
    return $ urlEncodeParams params $ initialRequest {
        requestHeaders =
                [("Authorization", ah)
                ,("Content-Type", "multipart/form-data")
                ,("Accept", "*/*")
                ,("User-Agent", "undershare")]
        }

-- | Produces a 'Source' of tweets in 'ByteString' form
getHomeTimeline'
    :: [Param]
    -> (Status -> Source Twitter ByteString -> Twitter a)
    -> Twitter a
getHomeTimeline' params k = do
    request <- getRequest (urlRESTBase ++ "statuses/home_timeline.json") params
    makeRequest request $ \r -> k (responseStatus r) (responseBody r)

getHomeTimeline
    :: (Status -> Source Twitter ByteString -> Twitter a)
    -> Twitter a
getHomeTimeline k = getHomeTimeline' [] k

getUserTimeline'
    :: [Param]
    -> (Status -> Source Twitter ByteString -> Twitter a)
    -> Twitter a
getUserTimeline' params k = do
    request <- getRequest (urlRESTBase ++ "statuses/user_timeline.json") params
    makeRequest request $ \r -> k (responseStatus r) (responseBody r)

getUserTimeline
    :: (Status -> Source Twitter ByteString -> Twitter a)
    -> Twitter a
getUserTimeline k = getUserTimeline' [] k

userStream'
    :: [Param]
    -> (Status -> Source Twitter ByteString -> Twitter a)
    -> Twitter a
userStream' params k = do
    request <- getRequest "https://userstream.twitter.com/1.1/user.json" params
    makeRequest request $ \r -> k (responseStatus r) (responseBody r)

userStream
    :: (Status -> Source Twitter ByteString -> Twitter a)
    -> Twitter a
userStream k = userStream' [] k

publicStream
    :: [String] -- ^ Search terms
    -> (Status -> Source Twitter ByteString -> Twitter a)
    -> Twitter a
publicStream terms k = do
    let params = [Param "track" (pack $ intercalate "," terms)]
    request <- postRequest "https://stream.twitter.com/1.1/statuses/filter.json"
               params
    makeRequest request $ \r -> k (responseStatus r) (responseBody r)

tweet'
    :: String -- ^ Tweet
    -> [Param]
    -> Twitter Status
tweet' txt params = do
    let url = urlRESTBase ++ "statuses/update.json"
    let params' = (Param "status" (pack txt) : params)
    request <- postRequest url params'
    makeRequest request $ \res -> do
        liftIO . putStrLn $ "Status: " ++
            show (statusCode $ responseStatus res)
        return $ responseStatus res

tweet :: String -> Twitter Status
tweet txt = tweet' txt []
