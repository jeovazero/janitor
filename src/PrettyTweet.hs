{-# LANGUAGE OverloadedStrings #-}
module PrettyTweet (prettyTweet) where

import PrettyTerm
import Data.Text as T
import Data.Text.IO as TIO

chunckBottom size box = box ++ (b:a:[])
    where
        a = applyTerm (Cyan (T.append " " $ T.replicate (size + 2) "-"))
        b = paddingSingleLine Cyan $ T.replicate size " "

chunckTop size box = a:b:box
    where
        a = applyTerm (Cyan (T.append " " $ T.replicate (size + 2) "-"))
        b = paddingSingleLine Cyan $ T.replicate size " "

chunckId id' box = top:h:box
    where
        top = applyTerm (Cyan (T.append " " $ T.replicate (T.length id' + 6) "-"))
        h = paddingSingleLine Cyan $ T.append "id: " (applyTerm (Cyan id'))

prettyTweet :: Int -> Text -> Text -> Text 
prettyTweet size id = T.unlines
    . chunckId id
    . chunckTop size
    . chunckBottom size
    . packTextBox Cyan
    . textBox size 

printPrettyTweet :: Int -> Text -> Text -> IO ()
printPrettyTweet size id = TIO.putStr . prettyTweet size id
