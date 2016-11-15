{-# LANGUAGE OverloadedStrings #-}

module Net.Monad.Shitpost.Twitter
    ( search
    , searchKeyword
    , getHomeTimeline
    , getHomeTimeline'
    , getUserTimeline
    , getUserTimeline'
    , userStream
    , userStream'
    , publicStream
    , tweet
    , tweet'
    )
where

import Prelude hiding (map, null, filter)
import qualified Prelude as P

import Tubes

import Net.Monad.Shitpost.Types
import Net.Monad.Shitpost.Core
import Network.HTTP.Client
import Data.List (intercalate)

import Network.HTTP.Types
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class

twitterBase :: String
twitterBase = "https://api.twitter.com/1.1/"

-- | Non-streaming search. Must at least specify the "q" parameter
search
    :: [Param]
    -> (Status -> Source Shitpost ByteString -> Shitpost a)
    -> Shitpost a
search params k = do
    request <- getRequest (twitterBase ++ "search/tweets.json") params
    makeRequest request $ \r -> k (responseStatus r) (responseBody r)

-- | Simplified search. Specify a @String@ of comma-separated search terms.
searchKeyword
    :: String -- ^ Search term
    -> (Status -> Source Shitpost ByteString -> Shitpost a)
    -> Shitpost a
searchKeyword q k = search [Param "q" (pack q)] k

-- | Get the home timeline using the REST API with request parameters
getHomeTimeline'
    :: [Param]
    -> (Status -> Source Shitpost ByteString -> Shitpost a)
    -> Shitpost a
getHomeTimeline' params k = do
    request <- getRequest (twitterBase ++ "statuses/home_timeline.json") params
    makeRequest request $ \r -> k (responseStatus r) (responseBody r)

-- | Get the home timeline using the REST API
getHomeTimeline
    :: (Status -> Source Shitpost ByteString -> Shitpost a)
    -> Shitpost a
getHomeTimeline k = getHomeTimeline' [] k

-- | Get the user timeline using the REST API with request parameters
getUserTimeline'
    :: [Param]
    -> (Status -> Source Shitpost ByteString -> Shitpost a)
    -> Shitpost a
getUserTimeline' params k = do
    request <- getRequest (twitterBase ++ "statuses/user_timeline.json") params
    makeRequest request $ \r -> k (responseStatus r) (responseBody r)

-- | Get the user timeline using the REST API
getUserTimeline
    :: (Status -> Source Shitpost ByteString -> Shitpost a)
    -> Shitpost a
getUserTimeline k = getUserTimeline' [] k

-- | Stream raw 'ByteString' data from user stream with request parameters
userStream'
    :: [Param]
    -> (Status -> Source Shitpost ByteString -> Shitpost a)
    -> Shitpost a
userStream' params k = do
    request <- getRequest "https://userstream.twitter.com/1.1/user.json" params
    makeRequest request $ \r -> k (responseStatus r) (responseBody r)

-- | Stream raw 'ByteString' data from the user stream
userStream
    :: (Status -> Source Shitpost ByteString -> Shitpost a)
    -> Shitpost a
userStream k = userStream' [] k

-- | Stream raw 'ByteString' data from a public stream with given search terms
publicStream
    :: [String] -- ^ Search terms
    -> (Status -> Source Shitpost ByteString -> Shitpost a)
    -> Shitpost a
publicStream terms k = do
    let params = [Param "track" (pack $ intercalate "," terms)]
    request <- postRequest "https://stream.twitter.com/1.1/statuses/filter.json"
               params
    makeRequest request $ \r -> k (responseStatus r) (responseBody r)

-- | Publish a tweet using any desired request parameters
tweet'
    :: String -- ^ Tweet
    -> [Param]
    -> Shitpost Status
tweet' txt params = do
    let url = twitterBase ++ "statuses/update.json"
    let params' = (Param "status" (pack txt) : params)
    request <- postRequest url params'
    makeRequest request $ return . responseStatus

-- | Publish a tweet
tweet :: String -> Shitpost Status
tweet txt = tweet' txt []
