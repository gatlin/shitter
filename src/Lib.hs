{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( authHeader
    , Config(..)
    , withHTTP
    , test
    , Twitter
    , withConfig
    )
where

import Prelude hiding (map, null)
import qualified Prelude as P

import Lib.OAuth
import Lib.Types
import Lib.Twitter

import Tubes

import Data.ByteString (ByteString, null, hPut, append)
import Data.ByteString.Char8 (unpack, pack)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.IO (stdout)
import Control.Monad.IO.Class
import Control.Monad (forever)

urlBase :: String
urlBase = "https://api.twitter.com/1.1/"

urlEncodeParams :: [Param] -> Request -> Request
urlEncodeParams params req = urlEncodedBody params' req where
    params' = fmap (\(Param k v) -> (k,v)) params

-- | Sends a managed HTTP request and receives a 'Source' via callback.
withHTTP
    :: Request
    -> Manager
    -> (Response (Source Twitter ByteString) -> IO a)
    -> Twitter a
withHTTP r m k = liftIO $ withResponse r m k' where
    k' resp = do
        p <- liftIO $ (from . brRead . responseBody) resp
        k (resp { responseBody = p })

from :: IO ByteString -> IO (Source Twitter ByteString)
from io = return $ Source loop where
    loop = do
        bs <- liftIO io
        if null bs
            then halt
            else (yield bs) >> loop

-- | Construct a GET request with the appropriate headers
getRequest
    :: String -- ^ URL
    -> [Param] -- ^ Request parameters
    -> Twitter Request
getRequest url params = do
    (Config c s t ts) <- getConfig
    -- convert the parameters into a query string
    let queryString = "?"++(unpack $ param_string params)
    initialRequest <- liftIO $ parseRequest $ "GET "++ url ++ queryString
    ah <- authHeader (pack c, pack s)
          (fmap pack t) (fmap pack ts)
          "GET" (pack url)
          params
    return $ initialRequest {
            requestHeaders =
                    [("Authorization", ah)
                    ,("Content-type", "multipart/form-data")
                    ,("Accept", "*/*")
                    ,("User-Agent","undershare")]
            }

getTimeline :: Sink Twitter ByteString -> Twitter ()
getTimeline snk = do
    c <- getConfig
    req <- getRequest (urlBase ++ "statuses/home_timeline.json")
           []
    manager <- liftIO $ newManager tlsManagerSettings
    withHTTP req manager $ \response -> withConfig c $
        runTube $ sample (responseBody response) >< pour snk

testSink :: Sink Twitter ByteString
testSink = Sink $ forever $ do
    piece <- await
    liftIO $ putStrLn . unpack $ piece

test :: Twitter ()
test = do
    --postTweet "Oh, hell, I'll test one more" [] c
    getTimeline testSink

tweet
    :: String -- ^ Tweet
    -> Twitter ()
tweet status = do
    (Config c s t ts) <- getConfig
    let url = urlBase ++ "statuses/update.json"
    initialReq <- liftIO $ parseRequest $ "POST " ++ url
    let statusParam = Param "status" (pack status)
    let params' = [statusParam]
    ah <- authHeader (pack c, pack s)
          (fmap pack t) (fmap pack ts)
          "POST" (pack url)
          params'
    liftIO $ putStrLn . unpack $ ah
    let request = urlEncodeParams params' $ initialReq {
            requestHeaders =
                    [("Authorization", ah)
                    ,("Content-Type","multipart/form-data")
                    ,("Accept", "*/*")
                    ,("User-Agent","undershare")]
            }
    liftIO $ putStrLn $ "Request: " ++ show request
    manager <- liftIO $ newManager tlsManagerSettings
    withHTTP request manager $ \response -> do
        putStrLn $ "Status code: " ++
            show (statusCode $ responseStatus response)
