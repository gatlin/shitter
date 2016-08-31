{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Twitter
where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.IO.Class

import Lib.Types

-- | A wrapper around 'IO' with access to a read-only 'Config' value
newtype Twitter a = Twitter {
    runClient :: ReaderT Config IO a
} deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Config
           , MonadIO)

-- | Evaluate a 'Client' computation with a given 'Config'
withConfig :: Config -> Twitter a -> IO a
withConfig cfg (Twitter c) = runReaderT c cfg

-- | Retrieve the read-only 'Config' value in a 'Twitter' computation
getConfig :: Twitter Config
getConfig = ask
