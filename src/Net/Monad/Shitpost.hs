{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Net.Monad.Shitpost
Description : Twitter API utilities
Copyright   : 2016
License     : GPLv3

Maintainer  : Gatlin Johnson <gatlin@niltag.net>
Stability   : experimental
Portability : non-portable

This module implements functions and commands for the 'Shitpost' monad so that
Shitpost bots may be programmed with ease.
-}

module Net.Monad.Shitpost
    ( -- * Shitpost monad
      Shitpost(..)
    , runShitpost
    , runShitpostWithManager
    , tweet
    , tweet'
    , getHomeTimeline
    , getHomeTimeline'
    , getUserTimeline
    , getUserTimeline'
    , userStream
    , userStream'
    , publicStream
      -- * Miscellaneous
    , Credentials(..)
    , Param(..)
    -- * Re-exports
    , lift
    , liftIO
    , Network.HTTP.Types.Status
    , statusCode
    )
where

import Prelude hiding (map, null, filter, break)
import qualified Prelude as P

import Net.OAuth.OAuth10a
import Net.Monad.Shitpost.Core
import Net.Monad.Shitpost.Types
import Net.Monad.Shitpost.Twitter

import Network.HTTP.Types (Status(..), statusCode)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
