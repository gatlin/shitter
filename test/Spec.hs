{-# LANGUAGE OverloadedStrings #-}

import Net.Monad.Shitpost
import Net.Monad.Shitpost.Core
import Net.Monad.Shitpost.Twitter
import Data.ByteString (ByteString, append, empty)
import Data.ByteString.Char8 (pack, unpack)
import Tubes
import Data.Aeson
import Net.OAuth.OAuth10a
import Control.Monad (forM_, forM)
import Data.List (intersperse)

import Prelude hiding (map, take)
import qualified Prelude as P

creds = Credentials
    "zT8gJhaxAba2Qo4bs2E5qEKqO"
    "5zgpRNgG1Bc9Vg5gl40FmpZbMCJdg6YpgTyrcjQZDnW1KY9Pay"
    (Just "12442062-0tXIsvJmCcEfhuDrgGNdl4QnYrV4QRnmRdT2wxiOU")
    (Just "Kf1O3i2cPzMnWSFbDb3ZfaksdFvOE3Sya2Enxri2d1TZX")

findGatlinTweets :: Shitpost ()
findGatlinTweets = searchKeyword "gatlin" $ \status tweets -> do
    liftIO . putStrLn . show $ statusCode status
    runTube $ sample tweets >< map show >< pour display

main :: IO ()
main = runShitpost creds findGatlinTweets

twitterUrl :: String
twitterUrl = "https://api.twitter.com/1.1/search/tweets.json"
