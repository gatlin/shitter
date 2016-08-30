{-# LANGUAGE OverloadedStrings #-}

module Lib.OAuth
where

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (object, (.=), encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import Control.Monad.IO.Class
import Control.Monad (forM, mapM)
import System.Entropy (getEntropy)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (sort, intersperse)
import Data.Monoid ((<>))
import Crypto.MAC.HMAC (hmac)
import Crypto.Hash.SHA1 (hash)

import Lib.Types

encode_param :: Param -> Param
encode_param (Param k v) = Param (percent_encode k) (percent_encode v)

-- | Filter all non-alphanumeric characters from a ByteString
filterNonAlphanumeric :: ByteString -> ByteString
filterNonAlphanumeric = BS.pack . trunc . filter f . BS.unpack
    where f ch | ch >= 97 && ch <= 122 = True
               | ch >= 48 && ch <= 57 = True
               | otherwise = False
          trunc it | length it > 32 = take 32 it
                   | otherwise = it

-- | Generates a nonce for OAuth requests.
gen_nonce :: MonadIO m => m ByteString
gen_nonce = do
    random_bytes <- liftIO $ getEntropy 32
    return $ filterNonAlphanumeric $ B64.encode random_bytes

-- | Returns the nearest integer number of seconds since the UNIX epoch
timestamp :: MonadIO m => m Integer
timestamp = liftIO $ round <$> getPOSIXTime

bs = BB.byteString
build = BL.toStrict . BB.toLazyByteString

-- | Percent encoded bytestring. Lovingly modified from the package
--   'uri-bytestring'
percent_encode :: ByteString -> ByteString
percent_encode  = build . mconcat . map encodeChar . BS.unpack
    where
      encodeChar ch | unreserved' ch = BB.word8 ch
                    | otherwise     = h2 ch

      unreserved' ch | ch >= 65 && ch <= 90  = True -- A-Z
                     | ch >= 97 && ch <= 122 = True -- a-z
                     | ch >= 48 && ch <= 57  = True -- 0-9
                     | ch == 95 = True -- _
                     | ch == 46 = True -- .
                     | ch == 126 = True -- ~
                     | ch == 45 = True -- -
                     | otherwise = False

      h2 v = let (a, b) = v `divMod` 16 in bs $ BS.pack [37, h a, h b]
      h i | i < 10    = 48 + i -- zero (0)
          | otherwise = 65 + i - 10 -- 65: A

-- | Generate a parameter string from a list of 'Param'
param_string :: [Param] -> ByteString
param_string = build .
    foldl (<>) mempty . intersperse (bs "&") .
    map (\(Param k v) -> (bs k) <> (bs "=") <> (bs v)) .
    sort . map encode_param

-- | Create the base string which will be signed
sig_base_string :: ByteString -> ByteString -> ByteString -> ByteString
sig_base_string ps method url = build $ (bs method) <> amp <> url' <> amp <> ps'
    where
        amp = bs "&"
        url' = bs $ percent_encode url
        ps' = bs $ percent_encode ps

-- | Create the signing key for the OAuth signature
signing_key :: ByteString -> (Maybe ByteString) -> ByteString
signing_key secret token = build $ (bs secret) <> (bs "&") <> token' where
    token' = bs $ maybe "" id token

sign :: ByteString -- ^ Signing key
     -> ByteString -- ^ Message to sign
     -> ByteString -- ^ Resulting base64-encoded signature
sign key msg = B64.encode $ hmac hash 64 key msg

-- | Generate the 'Authorization' header given a list of 'Param's
create_header_string :: [Param] -> ByteString
create_header_string params = build $ (bs "OAuth ") <> str where
    q = bs $ pack ['"']
    encoded = sort $ map encode_param params
    stringified = map (\(Param k v) -> (bs k) <> (bs "=") <> q <> (bs v) <> q)
                  encoded
    comma'd = intersperse (bs ", ") stringified
    str = foldl (<>) mempty comma'd

-- | Generate an authorization header for a request
authHeader :: MonadIO m
           => Credentials -- ^ Key and secret
           -> Maybe ByteString -- ^ token
           -> Maybe ByteString -- ^ secret
           -> ByteString -- ^ method
           -> ByteString -- ^ url
           -> [Param] -- ^ Any extra parameters to pass
           -> m ByteString
authHeader (key, secret) token token_secret method url extras  = do
    nonce <- gen_nonce
    liftIO . putStrLn $ "Nonce: " ++ unpack nonce
    ts    <- timestamp >>= return . pack . show
    let params = [ Param "oauth_consumer_key" key
                 , Param "oauth_nonce" nonce
                 , Param "oauth_timestamp" ts
                 , Param "oauth_token" (maybe "" id token)
                 , Param "oauth_signature_method" "HMAC-SHA1"
                 , Param "oauth_version" "1.0"
                 ]
    let sk      = signing_key secret token_secret
    let params' = param_string $ extras ++ params
    let base_string = sig_base_string params' method url
    let signature = sign sk base_string
    let with_signature = (Param "oauth_signature" signature) : params
    return $ create_header_string with_signature

authHeader' :: MonadIO m
           => Credentials -- ^ Key and secret
           -> Maybe ByteString -- ^ token
           -> Maybe ByteString -- ^ secret
           -> ByteString -- ^ method
           -> ByteString -- ^ url
           -> ByteString -- ^ fake nonce
           -> ByteString -- ^ fake timestamp
           -> [Param] -- ^ Any extra parameters to pass
           -> m ByteString
authHeader' (key, secret) token token_secret method url nonce ts extras  = do
    let params = [ Param "oauth_consumer_key" key
                 , Param "oauth_nonce" nonce
                 , Param "oauth_timestamp" ts
                 , Param "oauth_token" (maybe "" id token)
                 , Param "oauth_signature_method" "HMAC-SHA1"
                 , Param "oauth_version" "1.0"
                 ]
    let sk      = signing_key secret token_secret
    let params' = param_string $ extras ++ params
    let base_string = sig_base_string params' method url
    let signature = sign sk base_string
    let with_signature = (Param "oauth_signature" signature) : params
    return $ create_header_string with_signature
