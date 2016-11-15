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

import Prelude hiding (map, take)
import qualified Prelude as P

findGatlinTweets :: Shitpost ()
findGatlinTweets = searchKeyword "gatlin" $ \status tweets -> do
    liftIO . putStrLn . show $ statusCode status
    runTube $ sample tweets >< map show >< pour display

main :: IO ()
main = do
    args <- getArgs
    let creds = Credentials {
            consumerKey = pack (args !! 0),
            consumerSecret = pack (args !! 1),
            token = Just (pack (args !! 2)),
            tokenSecret = Just (pack (args !! 3))
            }
    runShitpost creds findGatlinTweets
