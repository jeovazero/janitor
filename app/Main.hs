{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Janitor
import Data.Tweets

main = do
    token <- lookupEnv "TOKEN"
    twitterID <- lookupEnv "TWITTER_ID"

    case (token, twitterID) of
        (Just tk, Just id) -> do
            result <- readTweets tk id
            putStrLn $ show $ decodeTweets result
        _ -> putStrLn "Missing TOKEN or TWITTER_ID env var"
