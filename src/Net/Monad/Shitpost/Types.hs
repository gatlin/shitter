{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Net.Monad.Shitpost.Types
Description : Common type definitions used elsewhere
Copyright   : 2016
License     : GPLv3

Maintainer  : Gatlin Johnson <gatlin@niltag.net>
Stability   : experimental
Portability : non-portable

-}

module Net.Monad.Shitpost.Types
    (
      Credentials(..)
    , Shitpost(..)
    , runShitpost
    , runShitpostWithManager
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

type ResponseStream = Response (Source Shitpost ByteString)

-- | Compound type containing read-only 'Twitter' state
data ShitpostStateRO = ShitpostStateRO
    { credentials :: Credentials
    , manager     :: Manager
    }

-- | A wrapper around 'IO' with access to read-only 'Credentials' and a
-- connection 'Manager'
newtype Shitpost a = Shitpost (ReaderT ShitpostStateRO IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader ShitpostStateRO
             , MonadIO )

-- | Evaluate a 'Shitpost' computation with 'Credentials'
runShitpost :: Credentials -> Shitpost a -> IO a
runShitpost crd twt = do
    manager <- newManager tlsManagerSettings
    runShitpostWithManager manager crd twt

-- | Similar to 'runTwitter' but with pre-existing connection 'Manager'
runShitpostWithManager :: Manager -> Credentials -> Shitpost a -> IO a
runShitpostWithManager m crd (Shitpost c) = runReaderT c $ ShitpostStateRO crd m

-- | Retrieve the read-only 'Config' value in a 'Twitter' computation
getCredentials :: Shitpost Credentials
getCredentials = ask >>= return . credentials

-- | Retrieves the connection manager
getManager :: Shitpost Manager
getManager = ask >>= return . manager
