{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( authHeader
    , Config(..)
    , withHTTP
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
    -> (Response (Source IO ByteString) -> IO a)
    -> IO a
withHTTP r m k = withResponse r m k' where
    k' resp = do
        p <- (from . brRead . responseBody) resp
        k (resp { responseBody = p })

from :: IO ByteString -> IO (Source IO ByteString)
from io = return $ Source loop where
    loop = do
        bs <- liftIO io
        if null bs
            then halt
            else (yield bs) >> loop

-- | Construct a GET request with the appropriate headers
getRequest
    :: MonadIO m
    => String -- ^ URL
    -> [Param] -- ^ Request parameters
    -> Config
    -> m Request
getRequest url params (Config c s t ts) = do
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

getTimeline :: Sink IO ByteString -> Config -> IO ()
getTimeline snk c = do
    req <- getRequest (urlBase ++ "statuses/home_timeline.json")
           [] c
    manager <- newManager tlsManagerSettings
    withHTTP req manager $ \response ->
        runTube $ sample (responseBody response) >< pour snk

testSink :: Sink IO ByteString
testSink = Sink $ forever $ do
    piece <- await
    liftIO $ putStrLn . unpack $ piece

test :: Config -> IO ()
test c = do
    --postTweet "Oh, hell, I'll test one more" [] c
    getTimeline testSink c

postTweet
    :: String -- ^ Tweet
    -> [Param]
    -> Config
    -> IO ()
postTweet tweet params (Config c s t ts) = do
    let url = urlBase ++ "statuses/update.json"
    initialReq <- parseRequest $ "POST " ++ url
    let statusParam = Param "status" (pack tweet)
    let params' = statusParam : params
    ah <- authHeader (pack c, pack s)
          (fmap pack t) (fmap pack ts)
          "POST" (pack url)
          params'
    putStrLn . unpack $ ah
    let request = urlEncodeParams params' $ initialReq {
            requestHeaders =
                    [("Authorization", ah)
                    ,("Content-Type","multipart/form-data")
                    ,("Accept", "*/*")
                    ,("User-Agent","undershare")]
            }
    putStrLn $ "Request: " ++ show request
    manager <- newManager tlsManagerSettings
    withHTTP request manager $ \response -> do
        putStrLn $ "Status code: " ++
            show (statusCode $ responseStatus response)
