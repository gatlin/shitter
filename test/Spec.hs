{-# LANGUAGE OverloadedStrings #-}

import Net.Monad.Shitpost
import Net.Monad.Shitpost.Core
import Net.Monad.Shitpost.Twitter
import Data.ByteString (ByteString, append, empty)
import Data.ByteString.Char8 (pack, unpack)
import Tubes
import Data.Aeson
import Net.OAuth.OAuth10a
import Control.Monad (forM_, forM, guard)
import Data.List (intersperse)
import System.Environment (getArgs)
import Network.HTTP.Client

import Prelude hiding (map, take)
import qualified Prelude as P

findGatlinTweets :: Shitpost ()
findGatlinTweets = searchKeyword "gatlin" $ \status tweets -> do
    liftIO . putStrLn . show $ statusCode status
    runTube $ sample tweets >< map show >< pour display

twitterTest :: Credentials -> IO ()
twitterTest creds = runShitpost creds findGatlinTweets

tumblrTest :: Credentials -> IO ()
tumblrTest creds =
    runShitpost creds $ do
        pr <- postRequest "https://www.tumblr.com/oauth/request_token" []
        makeRequest pr $ \r -> do
            liftIO . putStrLn . show $ responseStatus r
            runTube $ sample (responseBody r)
                   >< map show
                   >< pour display

main :: IO ()
main = do
    args <- getArgs
    case (args !! 0) of
        "twitter" -> do
            twitterTest $ Credentials {
                consumerKey = pack (args !! 1),
                consumerSecret = pack (args !! 2),
                token = Just (pack (args !! 3)),
                tokenSecret = Just (pack (args !! 4))
                }
        "tumblr" -> do
            tumblrTest $ Credentials {
                consumerKey = pack (args !! 1),
                consumerSecret = pack (args !! 2),
                token = Nothing,
                tokenSecret = Nothing
                }
        _ -> do
            putStrLn "Error: invalid test arguments"
