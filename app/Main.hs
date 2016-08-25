{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.ByteString (ByteString)
import Options.Applicative

data Config = Config
    { consumerKey :: String
    , consumerSecret :: String
    , token :: Maybe String
    , tokenSecret :: Maybe String
    } deriving (Show)

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
    putStrLn . show $ c

main :: IO ()
main = execParser options >>= start where
    options = info (helper <*> optParser)
              ( fullDesc
              <> progDesc "Post a tweet! FROM THE CLI"
              <> header "shitter" )
