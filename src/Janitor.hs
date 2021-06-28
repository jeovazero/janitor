{-# LANGUAGE OverloadedStrings #-}
module Janitor (readTweets) where

import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Client
import Network.HTTP.Client.TLS

baseV2 = "https://api.twitter.com/2"


readTweets :: String -> String -> IO () 
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

    LB.putStrLn $ responseBody response
