{-# LANGUAGE OverloadedStrings #-}
module Data.TweetsV1 (decodeTweetsV1, TweetV1(..)) where

import Data.Aeson as A
import Data.Text
import Data.ByteString.Lazy as LB

data TweetV1 = TweetV1 {
        t_id :: Text,
        text :: Text,
        created_at :: Text,
        in_reply_to_screen_name :: Maybe Text,
        retweet_count :: Int,
        favourite_count :: Int,
        retweeted :: Bool,
        is_quote_status :: Bool
    } deriving (Show)

instance FromJSON TweetV1 where
    parseJSON = withObject "TweetV1" $ \v -> TweetV1
        <$> v .: "id_str"
        <*> v .: "text"
        <*> v .: "created_at"
        <*> v .: "in_reply_to_screen_name"
        <*> v .: "retweet_count"
        <*> v .: "favorite_count"
        <*> v .: "retweeted"
        <*> v .: "is_quote_status"

decodeTweetsV1 :: LB.ByteString -> Either String [TweetV1]
decodeTweetsV1 = A.eitherDecode
