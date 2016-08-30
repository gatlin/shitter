{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (map)
import qualified Prelude as P

import Lib
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack, pack)
import Options.Applicative

import Tubes

optParser :: Parser Config
optParser = Config
    <$> strOption (long "key" <> help "Your API consumer key")
    <*> strOption (long "secret" <> help "Your API consumer secret")
    <*> optional (
            strOption (long "token" <> help "Client access token"))
    <*> optional (
            strOption (long "token-secret" <> help "Client access secret"))

start :: Config -> IO ()
start c@(Config key secret token ts) = do
    putStrLn "Config: "
    putStrLn . show $ c
    withConfig c test

main :: IO ()
main = execParser options >>= start where
    options = info (helper <*> optParser)
              ( fullDesc
              <> progDesc "Post a tweet! FROM THE CLI"
              <> header "shitter" )
