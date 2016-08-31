{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Lib/Types.hs
Description : Common type definitions used elsewhere
Copyright   : 2016
License     : GPLv3

Maintainer  : Gatlin Johnson <gatlin@niltag.net>
Stability   : experimental
Portability : non-portable

-}

module Lib.Types
    (
      Credentials(..)
    , Param(..)
    , Twitter(..)
    , ResponseStream
    , getCredentials
    , getOpenResponse
    , setOpenResponse
    , withCredentials
    )
where

import Data.ByteString (ByteString)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class
import Network.HTTP.Client (Response(..))
import Tubes

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

type ResponseStream = Response (Source Twitter ByteString)

-- | A wrapper around 'IO' with access to read-only 'Credentials'
newtype Twitter a = Twitter {
    runTwitter :: ReaderT Credentials (StateT (Maybe ResponseStream) IO) a
} deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Credentials
           , MonadState (Maybe ResponseStream)
           , MonadIO )

-- | Evaluate a 'Client' computation with given 'Credentials'
withCredentials :: Credentials -> Twitter a -> IO a
withCredentials crd (Twitter c) = do
    (x, _) <- runStateT (runReaderT c crd) Nothing
    return x

-- | Retrieve the read-only 'Config' value in a 'Twitter' computation
getCredentials :: Twitter Credentials
getCredentials = ask

getOpenResponse :: Twitter (Maybe ResponseStream)
getOpenResponse = get

setOpenResponse :: Maybe ResponseStream -> Twitter ()
setOpenResponse = put
