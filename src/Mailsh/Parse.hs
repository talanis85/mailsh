module Mailsh.Parse
  ( parseFileAuto
  , parseByteStringAuto
  , parseByteString
  , parseStringMaybe
  , Attoparsec
  ) where

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Utils
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BChar8
import Data.Maybe
import Data.Monoid

type Attoparsec = Parser

detectCrlf :: B.ByteString -> Bool
detectCrlf bs =
  let detect s = do
        (c, s') <- B.uncons s
        if c == 13
           then do
             (c', s'') <- B.uncons s'
             if c' == 10
                then return True
                else return False
           else detect s'
  in fromMaybe False (detect bs)

parseFileAuto :: FilePath -> Parser a -> IO (Either String a)
parseFileAuto fp p = do
  bs <- B.readFile fp
  return $ if detectCrlf bs then parseByteStringCrlf bs p else parseByteStringLf bs p

parseByteStringAuto :: B.ByteString -> Parser a -> Either String a
parseByteStringAuto bs p = if detectCrlf bs then parseByteStringCrlf bs p else parseByteStringLf bs p

parseByteStringLf :: B.ByteString -> Parser a -> Either String a
parseByteStringLf bs p = parseOnlyPretty p $ B.toStrict $ fixCrlfL bs

parseByteStringCrlf :: B.ByteString -> Parser a -> Either String a
parseByteStringCrlf bs p = parseOnlyPretty p (B.toStrict bs)

parseByteString :: B.ByteString -> Parser a -> Either String a
parseByteString = parseByteStringCrlf

parseString :: Parser a -> String -> Either String a
parseString p s = parseOnlyPretty p (BChar8.pack s)

parseStringMaybe :: Parser a -> String -> Maybe a
parseStringMaybe p s = either (const Nothing) Just (parseString p s)

fixCrlfL :: B.ByteString -> B.ByteString
fixCrlfL = BB.toLazyByteString . B.foldr fixCrlf' mempty
  where
    fixCrlf' 10 b = BB.word8 13 <> BB.word8 10 <> b
    fixCrlf' x  b = BB.word8 x <> b

fixCrlfS :: BS.ByteString -> BS.ByteString
fixCrlfS = BS.concatMap fixCrlf'
  where
    fixCrlf' 10 = BS.pack [13, 10]
    fixCrlf' x  = BS.pack [x]
