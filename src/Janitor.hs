{-# LANGUAGE OverloadedStrings #-}
module Janitor (req) where

import qualified Data.ByteString.Char8 as B

req :: IO () 
req = do
    B.putStrLn "hello friend" 
