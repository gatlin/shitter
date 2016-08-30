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

test :: Config -> IO ()
test (Config c s t ts) = do
    let method = "GET"
    let url = "https://api.twitter.com/1.1/statuses/home_timeline.json"
    initialRequest <- parseRequest $ method ++ " " ++ url
    manager <- newManager tlsManagerSettings
    ah <- authHeader' (pack c, pack s)
          (fmap pack t) (fmap pack ts)
          (pack method) (pack url)
          "cf70e9e51253b4526b80b85c0c3bcb81"
          "1472570666"
          []
    let request = initialRequest {
            requestHeaders =
                    [("Authorization", ah)
                    ,("Content-type", "multipart/form-data")
                    ,("Accept","*/*")
                    ,("Connection","close")
                    ,("User-Agent", "undershare")
                    ]
            }
    putStrLn $ "Authorization: " ++ unpack ah
{-    withHTTP request manager $ \response -> do
        putStrLn $ "Status: " ++ show (statusCode $ responseStatus response)
        runTube $ sample (responseBody response)
               >< map unpack
               >< pour display-}
{-
test = do
    let url = "https://api.twitter.com/1/statuses/update.json"
    let method = "POST"
    let key ="xvz1evFS4wEEPTGEFPHBog"
    let secret = "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw"
    let token = Just "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb"
    let token_secret = Just "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE"
    let ts = "1318622958"
    let nonce = "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg"
    let params = [ Param "oauth_consumer_key" key
                 , Param "oauth_nonce" nonce
                 , Param "oauth_timestamp" ts
                 , Param "oauth_token" (maybe "" id token)
                 , Param "oauth_signature_method" "HMAC-SHA1"
                 , Param "oauth_version" "1.0"
                 ]
    --
    let sk = signing_key secret token_secret
    putStrLn $ "Signing key: " ++ unpack sk
    let params' = param_string $
            [Param "status" "Hello Ladies + Gentlemen, a signed OAuth request!"
            , Param "include_entities" "true"]++params
    let base_string = sig_base_string params' method url
    putStrLn $ "Base string: " ++ unpack base_string
    let signature = sign sk base_string
    putStrLn $ "Signature: " ++ unpack signature
-}
