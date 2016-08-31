{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Client
where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.IO.Class

import Lib.Types

-- | A wrapper around 'IO' with access to a read-only 'Config' value
newtype Client a = Client {
    runClient :: ReaderT Config IO a
} deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Config
           , MonadIO)

-- | Evaluate a 'Client' computation with a given 'Config'
withConfig :: Config -> Client a -> IO a
withConfig cfg (Client c) = runReaderT c cfg
