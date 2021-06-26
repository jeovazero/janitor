{-# LANGUAGE OverloadedStrings #-}
module Janitor (req) where

import qualified Data.ByteString.Lazy.Char8 as LB
import Network.HTTP.Client
import Network.HTTP.Client.TLS

req :: IO () 
req = do
    manager <- newManager tlsManagerSettings

    initialRequest <- parseRequest "https://api.github.com/users/octocat"
    let request = initialRequest { requestHeaders = [("User-Agent", "haskell")] }
    response <- httpLbs request manager

    LB.putStrLn $ responseBody response
