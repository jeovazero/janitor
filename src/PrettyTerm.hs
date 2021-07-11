{-# LANGUAGE OverloadedStrings #-}
module PrettyTerm (
    Term(..),
    applyTerm,
    textBox,
    paddingSingleLine,
    packTextBox
) where

import Data.Text as T
import Data.Text.IO as TIO
import Data.List as L
import Debug.Trace

green = "\x1b[32m"
cyan = "\x1b[36m"
red = "\x1b[31m"
reset = "\x1b[0m"

data Term a = Red a | Cyan a | Green a | Normal a

applyTerm :: Term Text -> Text 
applyTerm term =
    case term of
        Red s    -> T.concat [red,s,reset]
        Cyan s   -> T.concat [cyan,s,reset]
        Green s  -> T.concat [green,s,reset]
        Normal s -> s

printTerm :: [Term Text] -> IO ()
printTerm = TIO.putStr . T.concat . fmap applyTerm

textBox :: Int -> Text -> [Text]
textBox size = 
    fmap (T.unwords .  L.reverse)        
        . L.concat
        . fmap (L.reverse . loop [] [] 0 . T.words)
        . T.lines
    where
        loop [] ans _ [] = ans
        loop acc ans sz []
            | size - sz > 0 = ((T.replicate (size - sz - 1) " "):acc):ans
            | otherwise = acc:ans
        loop acc ans sz (x:xs) =
            let
                sz' = T.length x + sz + (if acc == [] then 0 else 1)
                ans' = ((T.replicate (size - sz - 1) " "):acc):ans
                ans'' = if size - sz > 0 then ans' else (acc:ans)
            in
                if sz' <= size
                then loop (x:acc) ans sz' xs
                else loop [x] ans'' (T.length x) xs

packTextBox :: (Text -> Term Text) -> [Text] -> [Text]
packTextBox term = paddingLeftTextBox (applyTerm (term "  "))

paddingLeftTextBox chunck = fmap (T.append chunck)

padding cleft cright = (\bs -> T.append bs cright) . T.append cleft
paddingLeft chunck = T.append chunck
paddingRight chunck bs = T.append bs chunck

paddingSingleLine term = padding (applyTerm (term "| ")) (applyTerm (term " |"))

