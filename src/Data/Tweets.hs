{-# LANGUAGE OverloadedStrings #-}
module Data.Tweets (decodeTweets) where

import Data.Aeson as A
import Data.Text
import Data.ByteString.Lazy as LB

data Tweet = Tweet {
        t_id :: Text,
        text :: Text
    } deriving (Show)

instance FromJSON Tweet where
    parseJSON = withObject "Tweet" $ \v -> Tweet
        <$> v .: "id"
        <*> v .: "text"

data TweetsData = TweetsData {
    t_data :: [Tweet],
    meta :: Meta
} deriving (Show)

instance FromJSON TweetsData where
    parseJSON = withObject "TweetsData" $ \v -> TweetsData
        <$> v .: "data"
        <*> v .: "meta"

data Meta = Meta {
    oldest_id :: Text,
    newest_id :: Text,
    result_count :: Int
} deriving (Show)

instance FromJSON Meta where
    parseJSON = withObject "Meta" $ \v -> Meta
        <$> v .: "oldest_id"
        <*> v .: "newest_id"
        <*> v .: "result_count"

decodeTweets :: LB.ByteString -> Either String TweetsData
decodeTweets = A.eitherDecode
