{-# LANGUAGE OverloadedStrings #-}
module Data.Credentials (decodeCredentials,Credentials(..)) where

import Data.Aeson as A
import Data.Text
import Data.ByteString.Lazy as LB

data Credentials = Credentials {
        t_id :: Text,
        screen_name :: Text,
        name :: Text,
        description :: Text,
        followers_count :: Int,
        friends_count :: Int,
        favourites_count :: Int,
        statuses_count :: Int
    } deriving (Show)

instance FromJSON Credentials where
    parseJSON = withObject "Credentials" $ \v -> Credentials
        <$> v .: "id_str"
        <*> v .: "screen_name"
        <*> v .: "name"
        <*> v .: "description"
        <*> v .: "followers_count"
        <*> v .: "friends_count"
        <*> v .: "favourites_count"
        <*> v .: "statuses_count"

decodeCredentials :: LB.ByteString -> Either String Credentials
decodeCredentials = A.eitherDecode
