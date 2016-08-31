{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Lib.hs
Description : Twitter API utilities
Copyright   : 2016
License     : GPLv3

Maintainer  : Gatlin Johnson <gatlin@niltag.net>
Stability   : experimental
Portability : non-portable

This module implements functions and commands for the 'Twitter' monad so that
Twitter bots may be programmed with ease.
-}

module Lib
    ( Twitter(..)
    , Credentials(..)
    , Param(..)
    , withCredentials
    , tweet
    , tweet'
    , getHomeTimeline
    , getHomeTimeline'
    , test
    )
where

import Prelude hiding (map, null)
import qualified Prelude as P

import Lib.OAuth
import Lib.Types

import Tubes

import Data.ByteString (ByteString, null, hPut, append)
import Data.ByteString.Char8 (unpack, pack)
import Network.HTTP.Client hiding (responseOpen, responseClose)
import qualified Network.HTTP.Client as H
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.IO (stdout)
import Control.Monad.IO.Class
import Control.Monad (forever)
import Control.Monad.Trans (lift)

urlBase :: String
urlBase = "https://api.twitter.com/1.1/"

urlEncodeParams :: [Param] -> Request -> Request
urlEncodeParams params req = urlEncodedBody params' req where
    params' = fmap (\(Param k v) -> (k,v)) params

closeOpenResponse :: Twitter ()
closeOpenResponse = getOpenResponse >>= maybe (return ()) (\r -> do
    responseClose r
    setOpenResponse Nothing)

-- | Sends a managed HTTP request and receives a 'Source' via callback.
makeRequest
    :: Request
    -> Manager
    -> (ResponseStream -> Twitter a)
    -> Twitter a
makeRequest r m k = do
    response <- responseOpen r m
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
    -> Manager
    -> Twitter ResponseStream
responseOpen req man = do
    res <- liftIO $ H.responseOpen req man
    let res' = fmap (from . brRead) res
    setOpenResponse (Just res')
    return res'

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
getHomeTimeline' :: [Param] -> Twitter (Source Twitter ByteString)
getHomeTimeline' params = do
    c <- getCredentials
    request <- getRequest (urlBase ++ "statuses/home_timeline.json") params
    manager <- liftIO $ newManager tlsManagerSettings
    res <- (responseOpen request manager)
    return $ responseBody res

getHomeTimeline :: Twitter (Source Twitter ByteString)
getHomeTimeline = getHomeTimeline' []

tweet'
    :: String -- ^ Tweet
    -> [Param]
    -> Twitter ()
tweet' status params = do
    let url = urlBase ++ "statuses/update.json"
    let params' = (Param "status" (pack status) : params)
    request <- postRequest url params'
    manager <- liftIO $ newManager tlsManagerSettings
    makeRequest request manager $ \res -> do
        liftIO . putStrLn $ "Status: " ++
            show (statusCode $ responseStatus res)

tweet :: String -> Twitter ()
tweet status = tweet' status []

test :: Twitter ()
test = tweet "golly i sure do like dogs"
