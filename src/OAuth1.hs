{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module OAuth1 (oauth1Header,OAuth1HeaderParams(..)) where
import System.Random
import Data.List as L
import Data.Word
import Data.Char
import Data.ByteString as B
import Data.ByteString.Char8 as B8
import Data.ByteString.Base64
import Data.Time.Clock.POSIX
import Data.Map.Strict as Map
import Crypto.Hash.SHA1 (hmac)

data OAuth1HeaderParams = OAuth1HeaderParams {
        parameters :: [(ByteString,ByteString)],
        method :: ByteString,
        url :: ByteString,
        oauthToken :: ByteString,
        oauthSecret :: ByteString,
        consumerKey :: ByteString,
        consumerSecret :: ByteString
    }

rndRange = randomR (0,255)
gen = rndRange . snd

randomBytes :: Word8 -> IO [Word8]
randomBytes size = do
    seed <- newStdGen
    let first = rndRange seed
    let intSize = fromIntegral size
    pure $ L.take intSize $ fmap fst $ iterate' gen first

nonce = do
    rb <- randomBytes 32
    pure $ B.filter (isAlphaNum . chr . fromIntegral) $ encode (B.pack rb)

-- in millis
timestamp = fmap truncate getPOSIXTime

percentEncodingMap :: Map.Map Word8 ByteString
percentEncodingMap =
    Map.fromList $ fmap (\(a,b) -> (fromIntegral $ ord a, b))
        [('!', "%21")
        ,('#', "%23")
        ,('$', "%24")
        ,('%', "%25")
        ,('&', "%26")
        ,('\'', "%27")
        ,('(', "%28")
        ,(')', "%29")
        ,('*', "%2A")
        ,('+', "%2B")
        ,(',', "%2C")
        ,('/', "%2F")
        ,(':', "%3A")
        ,(';', "%3B")
        ,('=', "%3D")
        ,('?', "%3F")
        ,('@', "%40")
        ,('[', "%5B")
        ,(']', "%5D")
        ]

percentEncoding bs
  | B.null bs = bs
  | otherwise = B.concat $ percentEncoding' bs 1 (B.length bs) []

percentEncoding' bs current end chuncks =
    case (isEnd, Map.lookup (B.index bs (current - 1)) percentEncodingMap) of
        (True, Just value) -> L.reverse (addChunck value)
        (True, _) -> L.reverse (bs:chuncks)
        (False, Just value) ->
            percentEncoding' bs'' 1 end' (addChunck value)
        _ -> percentEncoding' bs (current + 1) end chuncks
    
    where
        isEnd = current == end
        end' = end - current
        (as,bs') = B.splitAt (current - 1) bs
        bs'' = B.drop 1 bs'
        addChunck c = c:as:chuncks

parameterString separator =
    B.intercalate separator
    . L.sort
    . fmap (\(a,b) -> B.concat [percentEncoding a,"=", percentEncoding b])

toUpperW8 = fromIntegral . ord . toUpper . chr . fromIntegral
numberToBytestring = B8.pack . show

signatureBaseString method url parameters =
    B.intercalate "&" [B.map toUpperW8 method,percentEncoding url,percentEncoding $ parameterString "&" parameters]

oauth1Header (OAuth1HeaderParams {
    oauthToken,
    oauthSecret,
    consumerKey,
    consumerSecret,
    url,
    method,
    parameters 
}) = do
    n <- nonce
    t <- timestamp
    let oauthParameters =
            [("oauth_nonce",n)
            ,("oauth_timestamp", numberToBytestring t)
            ,("oauth_consumer_key",consumerKey)
            ,("oauth_signature_method","HMAC-SHA1")
            ,("oauth_token",oauthToken)
            ,("oauth_version","1.0")
            ]
    let parameters' = oauthParameters ++ parameters
    let signatureBase = signatureBaseString method url parameters'
    let signingKey = B.intercalate "&" $ fmap percentEncoding [consumerSecret,oauthSecret]
    let oauthSignature = encode $ hmac signingKey signatureBase
    let allParameters =
            fmap
                (\(key,value) -> (key,B.concat ["\"",value,"\""]))
                $ ("oauth_signature",oauthSignature):parameters'

    pure $ B.concat ["OAuth ",parameterString ", " allParameters]
