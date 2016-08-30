{-# LANGUAGE OverloadedStrings #-}

module Lib.Types where

import Data.ByteString (ByteString)

-- | Bot configuration values
data Config = Config
    { consumerKey :: String
    , consumerSecret :: String
    , token :: Maybe String
    , tokenSecret :: Maybe String
    } deriving (Show)

type Credentials = (ByteString, ByteString)

-- | HTTP request parameters
data Param = Param
    { paramKey   :: ByteString
    , paramValue :: ByteString
    } deriving (Show, Eq, Ord)
