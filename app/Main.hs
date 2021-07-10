{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs,lookupEnv)
import Data.Tweets
import Data.List
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.ByteString.Char8 as B8
import Janitor as J
import qualified Data.Text.IO as TIO
import CLI

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

getAccessTokens = do
    oauthToken <- lookupEnv "OAUTH_TOKEN"
    oauthSecret <- lookupEnv "OAUTH_SECRET"
    consumerKey <- lookupEnv "CONSUMER_KEY"
    consumerSecret <- lookupEnv "CONSUMER_SECRET"
    
    case (oauthToken,oauthSecret,consumerKey,consumerSecret) of
        (Just otk,Just osec,Just ckey, Just csec) -> do
            pure $ Just AccessTokens {
                oauthToken = B8.pack otk,
                oauthSecret = B8.pack osec,
                consumerKey = B8.pack ckey,
                consumerSecret = B8.pack csec
            }
        _ -> pure Nothing


readTweets = do
    token <- lookupEnv "TOKEN"
    putStrLn "Twitter ID:\n> "
    twitterID <- getLine

    case token of
        Just tk -> do
            result <- J.readTweets tk twitterID

            case decodeTweets result of
                Right tweets -> prettyTweets tweets
                Left err -> putStrLn err

        _ -> putStrLn "Missing TOKEN or TWITTER_ID env var"

deleteTweet accessTokens = do
    putStrLn "Twitter ID:\n> "
    tweetID <- getLine

    case accessTokens of
        Just tokens -> do
            result <- J.deleteTweet tokens tweetID

            print $ LB8.unpack result
        _ -> putStrLn "Missing env vars"

verify accessTokens = do
    result <- J.verifyCredentials accessTokens
    print $ LB8.unpack result

main = do
    args <- getArgs
    accessTokens <- getAccessTokens

    case accessTokens of
        Just tokens -> do
            case parseCLICmd args of
                CLIHelp               -> putStr helpCLI
                CLIRead               -> putStrLn "Not implemented" 
                CLIVerify             -> verify tokens
                CLIDeleteAll          -> putStrLn "Not implemented"
                CLINotCmdError arg    -> putStr $ helpCLINotCmdError arg
                CLICmdNoArgsError arg -> putStr $ helpCLICmdNoArgsError arg 
        _ -> putStrLn "Missing env vars"

    
