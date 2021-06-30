{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Janitor
import Data.Tweets
import Data.List
import Data.Text as T
import qualified Data.Text.IO as TIO

prettyTweet tweet = result
    where
        tweetId = t_id tweet
        tweetText = text tweet
        result = T.concat [
                "----\nid: ",
                tweetId,
                "\ntext:\n----\n\n",
                tweetText,
                "\n\n----"
            ]

prettyTweets tweets = do
    let tweetsData = t_data tweets
    TIO.putStrLn (T.unlines $ fmap prettyTweet tweetsData)

main = do
    token <- lookupEnv "TOKEN"
    twitterID <- lookupEnv "TWITTER_ID"

    case (token, twitterID) of
        (Just tk, Just id) -> do
            result <- readTweets tk id

            case decodeTweets result of
                Right tweets -> prettyTweets tweets
                Left err -> putStrLn err

        _ -> putStrLn "Missing TOKEN or TWITTER_ID env var"
