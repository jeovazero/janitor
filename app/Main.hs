{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Janitor

main = do
    token <- lookupEnv "TOKEN"
    twitterID <- lookupEnv "TWITTER_ID"

    case (token, twitterID) of
        (Just tk, Just id) -> readTweets tk id
        _ -> putStrLn "Missing TOKEN or TWITTER_ID env var"
