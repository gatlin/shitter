{-# LANGUAGE OverloadedStrings #-}

module Lib.Client
where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader

import Lib.Types

-- | A 'Client' is a monad which has access to a 'Config' value
type Client = ReaderT Config IO

-- | Evaluate a 'Client' computation with a given 'Config'
withConfig :: Config -> Client a -> IO a
withConfig cfg r = runReaderT r cfg
