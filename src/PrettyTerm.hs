{-# LANGUAGE OverloadedStrings #-}
module PrettyTerm (
    Term(..),
    applyTerm,
    textBox,
    paddingSingleLine,
    packTextBox
) where

import Data.ByteString.Char8 as B8
import Data.List as L

green = "\x1b[32m"
cyan = "\x1b[36m"
red = "\x1b[31m"
reset = "\x1b[0m"

data Term a = Red a | Cyan a | Green a | Normal a

applyTerm :: Term ByteString -> ByteString
applyTerm term =
    case term of
        Red s    -> B8.concat [red,s,reset]
        Cyan s   -> B8.concat [cyan,s,reset]
        Green s  -> B8.concat [green,s,reset]
        Normal s -> s

printTerm :: [Term ByteString] -> IO ()
printTerm = B8.putStr . B8.concat . fmap applyTerm

indexMaybe :: ByteString -> Int -> Maybe Char
indexMaybe bs n
    | n < 0             = Nothing
    | n >= B8.length bs = Nothing
    | otherwise         = Just (B8.index bs n)

fillRight :: Char -> Int -> ByteString -> ByteString
fillRight char size bs
    | size > len = B8.append bs (B8.replicate (size - len) char)
    | otherwise  = bs
    where len = B8.length bs

textBox :: Int -> ByteString -> [ByteString]
textBox size text = L.reverse $ (textBox' [] size text)

textBox' acc size text =
    case indexMaybe text size of
        Just _ ->
            let
                index = case loop (size - 1) of
                          Just n -> n + 1
                          Nothing -> size + 1
                (chunck,text') = B8.splitAt index text
            in
                textBox' ((fillRight ' ' size chunck):acc) size text' 
          
        Nothing -> (fillRight ' ' size text):acc
    where
        loop n
            | n < 0 = Nothing
            | otherwise = case B8.index text n of
                              ' ' -> Just n
                              _   -> loop (n - 1)

packTextBox :: (ByteString -> Term ByteString) -> [ByteString] -> [ByteString]
packTextBox term = paddingLeftTextBox (applyTerm (term "| "))
    . paddingRightTextBox (applyTerm (term " |"))

paddingLeftTextBox chunck = fmap (B8.append chunck)

paddingRightTextBox chunck = fmap (\bs -> B8.append bs chunck)

padding cleft cright = (\bs -> B8.append bs cright) . B8.append cleft
paddingLeft chunck = B8.append chunck
paddingRight chunck bs = B8.append bs chunck

paddingSingleLine term = padding (applyTerm (term "| ")) (applyTerm (term " |"))
