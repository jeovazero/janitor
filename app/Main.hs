{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import System.Environment (getArgs,lookupEnv)
import Data.Credentials
import Data.TweetsV1
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.ByteString.Char8 as B8
import Janitor as J
import qualified Data.Text.IO as TIO
import CLI
import PrettyTweet

prettyTweetV1 TweetV1{ t_id, text }
    = prettyTweet 36 t_id text

prettyTweets tweets = do
    TIO.putStrLn (T.unlines $ fmap prettyTweetV1 tweets)

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

    case decodeCredentials result of
        Right credentials -> print credentials
        Left err -> putStrLn err

readTweetsV1 accessTokens = do 
    result <- J.readTweets accessTokens

    case decodeTweetsV1 result of
        Right tweets -> prettyTweets tweets
        Left err -> putStrLn err

main = do
    args <- getArgs
    accessTokens <- getAccessTokens

    case accessTokens of
        Just tokens -> do
            case parseCLICmd args of
                CLIHelp               -> putStr helpCLI
                CLIRead               -> readTweetsV1 tokens 
                CLIVerify             -> verify tokens
                CLIDeleteAll          -> putStrLn "Not implemented"
                CLINotCmdError arg    -> putStr $ helpCLINotCmdError arg
                CLICmdNoArgsError arg -> putStr $ helpCLICmdNoArgsError arg 
        _ -> putStrLn "Missing env vars"

    
