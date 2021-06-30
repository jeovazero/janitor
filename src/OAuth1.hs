import System.Random
import Data.List as L
import Data.Word
import Data.Char
import Data.ByteString as B
import Data.ByteString.Base64

rndRange = randomR (0,255)
gen = rndRange . snd

randomBytes :: Word8 -> IO [Word8]
randomBytes size = do
    seed <- newStdGen
    let first = rndRange seed
    let isize = fromIntegral size
    pure $ L.take isize $ fmap fst $ iterate' gen first

nonce = do
    rb <- randomBytes 32
    pure $ B.filter (isAlphaNum . chr . fromIntegral) $ encode (pack rb)
