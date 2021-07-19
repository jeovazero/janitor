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
import PrettyTerm

prettyTweetV1 TweetV1{ t_id, text }
    = prettyTweet 36 t_id text

prettyTweets :: [TweetV1] -> IO ()
prettyTweets [] = printTerm [Cyan "Congratulations! There are no tweets!\n"]
prettyTweets tweets  = TIO.putStrLn . T.unlines . fmap prettyTweetV1 $ tweets

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

processRequest req decoder printer = do
    result <- req

    case result of
        Right result' ->
            case decoder result' of
                Right result'' -> printer result''
                Left err -> putStrLn err
        Left result' -> print result'

deleteTweet' accessTokens tid = do
    result <- J.deleteTweet accessTokens (T.unpack tid)
    pure $ case result of
               Right _ -> True
               Left _ -> False

prettyDelete' accessTokens (TweetV1{ t_id }) = do
    prettyDelete t_id (deleteTweet' accessTokens t_id)

deleteTweets accessTokens = do
    response <- J.readTweets accessTokens

    case response of
        Right response' ->
            case decodeTweetsV1 response' of
                Right tweets -> do
                    sequence_ $ fmap (prettyDelete' accessTokens) tweets
                    putStrLn "All done!"
                Left err -> putStrLn err
        Left err -> print err

verify accessTokens =
    processRequest (J.verifyCredentials accessTokens) decodeCredentials print

readTweetsV1 accessTokens = 
    processRequest (J.readTweets accessTokens) decodeTweetsV1 prettyTweets

main = do
    args <- getArgs
    accessTokens <- getAccessTokens

    case accessTokens of
        Just tokens -> do
            case parseCLICmd args of
                CLIHelp               -> putStr helpCLI
                CLIRead               -> readTweetsV1 tokens 
                CLIVerify             -> verify tokens
                CLIDeleteAll          -> deleteTweets tokens
                CLINotCmdError arg    -> putStr $ helpCLINotCmdError arg
                CLICmdNoArgsError arg -> putStr $ helpCLICmdNoArgsError arg 
        _ -> do
            printTerm [Red "> Missing environment variables (tokens)\n\n"]
            putStr helpCLI

