{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Janitor (readTweets,verifyCredentials) where

import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import OAuth1 as O

baseV2 = "https://api.twitter.com/2"

readTweets :: String -> String -> IO LB.ByteString 
readTweets token userID = do
    manager <- newManager tlsManagerSettings

    initialRequest <- parseRequest (concat [baseV2,"/users/",userID,"/tweets"])
    let request = initialRequest {
        requestHeaders = [
            ("User-Agent", "haskell"),
            ("Authorization", B.concat ["Bearer ",B.pack token])
        ]
    }
    response <- httpLbs request manager

    pure $ responseBody response


baseV1 = "https://api.twitter.com/1.1"

verifyCredentials :: String -> String -> String -> String  -> IO LB.ByteString 
verifyCredentials oauthToken' oauthSecret' consumerKey' consumerSecret' = do
    manager <- newManager tlsManagerSettings

    let url' = concat [baseV1,"/account/verify_credentials.json"]
    initialRequest <- parseRequest url'
    
    let params = OAuth1HeaderParams {
        parameters = [],
        method = "GET",
        url = B.pack url',
        oauthToken = B.pack oauthToken',
        oauthSecret = B.pack oauthSecret',
        consumerKey = B.pack consumerKey',
        consumerSecret = B.pack consumerSecret'
    }
    authHeader <- oauth1Header params
    print authHeader

    let request = initialRequest {
        requestHeaders = [
            ("User-Agent", "haskell"),
            ("Authorization", authHeader)
        ]
    }
    response <- httpLbs request manager

    pure $ responseBody response
