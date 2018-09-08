module Mailsh.Parse
  ( parseCrlfFile
  , parseCrlfByteString
  , parseFile
  , parseByteString
  , parseString
  , parseStringMaybe
  , detectCrlf
  , Attoparsec
  ) where

import Control.Monad.Except
import Control.Monad.Trans
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Utils
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BChar8
import Data.Maybe
import Data.Monoid
import Data.Word

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

parseCrlfFile :: FilePath -> Parser a -> IO (Either String a)
parseCrlfFile fp p = flip parseCrlfByteString p <$> B.readFile fp

parseCrlfByteString :: B.ByteString -> Parser a -> Either String a
parseCrlfByteString bs p = parseOnlyPretty p $ B.toStrict $ fixCrlfL bs

parseFile :: FilePath -> Parser a -> IO (Either String a)
parseFile fp p = parseOnlyPretty p <$> BS.readFile fp

parseByteString :: B.ByteString -> Parser a -> Either String a
parseByteString bs p = parseOnlyPretty p (B.toStrict bs)

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
