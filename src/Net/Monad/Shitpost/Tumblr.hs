{-# LANGUAGE OverloadedStrings #-}

{- |
This module is UNFINISHED. 
-}

module Net.Monad.Shitpost.Tumblr
    ( tumblrPostText
    , tumblrPostText'
    )
where

import Prelude hiding (map, null, filter)
import qualified Prelude as P

import Tubes

import Net.Monad.Shitpost.Types
import Net.Monad.Shitpost.Core
import Network.HTTP.Client
import Data.List (intercalate)

import Network.HTTP.Types
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class

urlBlogBase :: String -> String
urlBlogBase blogId = "https://api.tumblr.com/v2/blog/" ++ blogId

urlUserBase :: String
urlUserBase = "https://api.tumblr.com/v2/user/"

{-
userStream' params k = do
    request <- getRequest "https://userstream.twitter.com/1.1/user.json" params
    makeRequest request $ \r -> k (responseStatus r) (responseBody r)
-}
tumblrPostText
    :: String -- ^ Blog Identifier
    -> String -- ^ Text
    -> Shitpost Status
tumblrPostText blogId txt = tumblrPostText' blogId txt []

tumblrPostText'
    :: String -- ^ Blog Identifier
    -> String -- ^ Text
    -> [Param] -- ^ Extra request parameters
    -> Shitpost Status
tumblrPostText' blogId txt extraParams = do
    let url = (urlBlogBase blogId) ++ "/post"
    let params = [(Param "type" "text")
                 ,(Param "body" (pack txt))]
                 ++ extraParams
    request <- postRequest url params
    makeRequest request $ return . responseStatus
