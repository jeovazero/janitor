{-# LANGUAGE OverloadedStrings #-}
module PrettyTweet (prettyTweet) where

import PrettyTerm
import Data.ByteString.Char8 as B8
import Data.Text.Encoding as Enc

chunckBottom size box = box ++ (b:a:[])
    where
        a = applyTerm (Cyan (B8.append " " $ B8.replicate (size + 2) '-'))
        b = paddingSingleLine Cyan $ B8.replicate size ' '

chunckTop size box = a:b:box
    where
        a = applyTerm (Cyan (B8.append " " $ B8.replicate (size + 2) '-'))
        b = paddingSingleLine Cyan $ B8.replicate size ' '

chunckId id' box = top:h:box
    where
        top = applyTerm (Cyan (B8.append " " $ B8.replicate (B8.length id' + 6) '-'))
        h = paddingSingleLine Cyan $ B8.append "id: " (applyTerm (Cyan id'))

prettyTweet :: Int -> ByteString -> ByteString -> ByteString
prettyTweet size id = B8.unlines
    . chunckId id
    . chunckTop size
    . chunckBottom size
    . packTextBox Cyan
    . textBox size

printPrettyTweet :: Int -> ByteString -> ByteString -> IO ()
printPrettyTweet size id = B8.putStr . prettyTweet size id