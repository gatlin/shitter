{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Types (
    Credentials(..),
    Param(..),
    Twitter(..),
    withCredentials,
    getCredentials
    )
where

import Data.ByteString (ByteString)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.IO.Class

-- | Bot credentials
data Credentials = Credentials
    { consumerKey :: ByteString
    , consumerSecret :: ByteString
    , token :: Maybe ByteString
    , tokenSecret :: Maybe ByteString
    } deriving (Show)

-- | HTTP request parameters
data Param = Param
    { paramKey   :: ByteString
    , paramValue :: ByteString
    } deriving (Show, Eq, Ord)

-- | A wrapper around 'IO' with access to read-only 'Credentials'
newtype Twitter a = Twitter {
    runTwitter :: ReaderT Credentials IO a
} deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Credentials
           , MonadIO )

-- | Evaluate a 'Client' computation with a given 'Config'
withCredentials :: Credentials -> Twitter a -> IO a
withCredentials crd (Twitter c) = runReaderT c crd

-- | Retrieve the read-only 'Config' value in a 'Twitter' computation
getCredentials :: Twitter Credentials
getCredentials = ask
