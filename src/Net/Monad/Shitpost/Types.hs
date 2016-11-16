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
    , setCredentials
    )
where

import Data.ByteString (ByteString)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.IO.Class
import Network.HTTP.Client (Response(..), Manager(..), newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Tubes
import Net.OAuth.OAuth10a (Credentials(..))

type ResponseStream = Response (Source Shitpost ByteString)

-- | Compound type containing 'Twitter' state
data ShitpostState = ShitpostState
    { credentials :: Credentials
    , manager     :: Manager
    }

-- | A wrapper around 'IO' with mutable access to the 'Credentials' and a
-- connection 'Manager'
newtype Shitpost a = Shitpost (StateT ShitpostState IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState ShitpostState
             , MonadIO )

-- | Evaluate a 'Shitpost' computation with 'Credentials'
runShitpost :: Credentials -> Shitpost a -> IO a
runShitpost crd twt = do
    manager <- newManager tlsManagerSettings
    runShitpostWithManager manager crd twt

-- | Similar to 'runTwitter' but with pre-existing connection 'Manager'
runShitpostWithManager :: Manager -> Credentials -> Shitpost a -> IO a
runShitpostWithManager m crd (Shitpost c) = evalStateT c $ ShitpostState crd m

-- | Retrieve the 'Config' value in a 'Shitpost' computation
getCredentials :: Shitpost Credentials
getCredentials = get >>= return . credentials

-- | Set the 'Config' value in a 'Shitpost' computation
setCredentials :: Credentials -> Shitpost ()
setCredentials crds = modify $ \st -> st { credentials = crds }

-- | Retrieves the connection manager
getManager :: Shitpost Manager
getManager = get >>= return . manager
