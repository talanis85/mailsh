module Mailsh.Parse
  ( parseCrlfFile
  , parseFile
  , parseString
  , Attoparsec
  ) where

import Control.Monad.Except
import Control.Monad.Trans
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BChar8
import Data.Monoid
import Data.Word

type Attoparsec = Parser

parseCrlfFile :: FilePath -> Parser a -> IO (Either String a)
parseCrlfFile fp p = parseOnly p <$> B.toStrict <$> fixCrlfL <$> B.readFile fp

parseFile :: FilePath -> Parser a -> IO (Either String a)
parseFile fp p = parseOnly p <$> BS.readFile fp

parseString :: Parser a -> String -> Maybe a
parseString p s = case parseOnly p (BChar8.pack s) of
                    Left err -> Nothing
                    Right v -> Just v

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
