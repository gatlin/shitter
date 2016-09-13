{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Net.Monad.Twitter.Types
Description : Common type definitions used elsewhere
Copyright   : 2016
License     : GPLv3

Maintainer  : Gatlin Johnson <gatlin@niltag.net>
Stability   : experimental
Portability : non-portable

-}

module Net.Monad.Twitter.Types
    (
      Credentials(..)
    , Twitter(..)
    , runTwitter
    , runTwitterWithManager
    , ResponseStream
    , getManager
    , getCredentials
    )
where

import Data.ByteString (ByteString)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.IO.Class
import Network.HTTP.Client (Response(..), Manager(..), newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Tubes
import Net.OAuth.OAuth10a (Credentials(..))

type ResponseStream = Response (Source Twitter ByteString)

-- | Compound type containing read-only 'Twitter' state
data TwitterStateRO = TwitterStateRO
    { credentials :: Credentials
    , manager     :: Manager
    }

-- | A wrapper around 'IO' with access to read-only 'Credentials' and a
-- connection 'Manager'
newtype Twitter a = Twitter (ReaderT TwitterStateRO IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader TwitterStateRO
             , MonadIO )

-- | Evaluate a 'Twitter' computation with given 'Credentials'
runTwitter :: Credentials -> Twitter a -> IO a
runTwitter crd twt = do
    manager <- newManager tlsManagerSettings
    runTwitterWithManager manager crd twt

-- | Similar to 'runTwitter' but with pre-existing connection 'Manager'
runTwitterWithManager :: Manager -> Credentials -> Twitter a -> IO a
runTwitterWithManager m crd (Twitter c) = runReaderT c $ TwitterStateRO crd m

-- | Retrieve the read-only 'Config' value in a 'Twitter' computation
getCredentials :: Twitter Credentials
getCredentials = ask >>= return . credentials

-- | Retrieves the connection manager
getManager :: Twitter Manager
getManager = ask >>= return . manager
