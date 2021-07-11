{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Janitor (
        deleteTweet,
        readTweets,
        verifyCredentials,
        AccessTokens(..)
    ) where

import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Char8 as B
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Prelude as P
import OAuth1 as O

baseV1 = "https://api.twitter.com/1.1"

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

type ApiResponse = IO (Either LB.ByteString LB.ByteString)

apiV1 :: ByteString -> APIV1Params -> ApiResponse
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

    let status = responseStatus response
    let response' = responseBody response

    pure $ case status == status200 of
            True -> Right response'
            False -> Left response'

verifyCredentials :: AccessTokens -> ApiResponse 
verifyCredentials accessTokens = do
    let params = APIV1Params {
        parameters = [],
        method = "GET",
        path = "/account/verify_credentials.json",
        accessTokens = accessTokens
    }

    apiV1 baseV1 params
    
readTweets :: AccessTokens -> ApiResponse 
readTweets accessTokens = do
    let params = APIV1Params {
        parameters = [],
        method = "GET",
        path = "/statuses/user_timeline.json",
        accessTokens = accessTokens
    }

    apiV1 baseV1 params

deleteTweet :: AccessTokens -> String -> ApiResponse
deleteTweet accessTokens id' = do
    let path = B.concat ["/statuses/destroy/", B.pack id',".json"]

    let params = APIV1Params {
        parameters = [],
        method = "POST",
        path = path,
        accessTokens = accessTokens
    }

    apiV1 baseV1 params
