{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Janitor (deleteTweet,readTweets,verifyCredentials,AccessTokens(..)) where

import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Char8 as B
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Prelude as P
import OAuth1 as O

baseV1 = "https://api.twitter.com/1.1"
baseV2 = "https://api.twitter.com/2"

data AccessTokens = AccessTokens {
        oauthToken :: ByteString,
        oauthSecret :: ByteString,
        consumerKey :: ByteString,
        consumerSecret :: ByteString
    }

data APIV1Params = APIV1Params {
        parameters :: [(ByteString,ByteString)],
        method :: ByteString,
        path :: ByteString,
        accessTokens :: AccessTokens
    }


apiV1 :: ByteString -> APIV1Params -> IO LB.ByteString
apiV1 base (APIV1Params {
    parameters,
    method,
    path,
    accessTokens = AccessTokens {
        oauthToken,
        oauthSecret,
        consumerKey,
        consumerSecret
    }
}) = do
    manager <- newManager tlsManagerSettings

    let url = B.append base path
    let url' = B.unpack $ url 
    initialRequest <- parseRequest url'
    
    let oauthParams = OAuth1HeaderParams {
        parameters = parameters,
        method = method,
        url = url,
        oauthToken = oauthToken,
        oauthSecret = oauthSecret,
        consumerKey = consumerKey,
        consumerSecret = consumerSecret
    }
    authHeader <- oauth1Header oauthParams

    let request = initialRequest {
        method = method,
        requestHeaders = [
            ("User-Agent", "haskell"),
            ("Authorization", authHeader)
        ]
    }
    response <- httpLbs request manager

    pure $ responseBody response


readTweets :: String -> String -> IO LB.ByteString 
readTweets token userID = do
    manager <- newManager tlsManagerSettings

    initialRequest <- parseRequest (P.concat [baseV2,"/users/",userID,"/tweets"])
    let request = initialRequest {
        requestHeaders = [
            ("User-Agent", "haskell"),
            ("Authorization", B.concat ["Bearer ",B.pack token])
        ]
    }
    response <- httpLbs request manager

    pure $ responseBody response

verifyCredentials :: AccessTokens -> IO LB.ByteString 
verifyCredentials accessTokens = do
    let params = APIV1Params {
        parameters = [],
        method = "GET",
        path = "/account/verify_credentials.json",
        accessTokens = accessTokens
    }

    apiV1 baseV1 params
    
deleteTweet :: AccessTokens -> String -> IO LB.ByteString 
deleteTweet accessTokens id' = do
    let path = B.concat ["/statuses/destroy/", B.pack id',".json"]

    let params = APIV1Params {
        parameters = [],
        method = "POST",
        path = path,
        accessTokens = accessTokens
    }
    apiV1 baseV1 params
