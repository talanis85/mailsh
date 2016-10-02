module Mailsh.Parse
  ( parseFile
  , fixCrlfL
  , fixCrlfS
  ) where

import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.Word

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fp p = maybeResult <$> parse p <$> fixCrlfL <$> B.readFile fp

fixCrlfL :: B.ByteString -> B.ByteString
fixCrlfL = B.concatMap fixCrlf'
  where
    fixCrlf' 10 = B.pack [13, 10]
    fixCrlf' x  = B.pack [x]

fixCrlfS :: BS.ByteString -> BS.ByteString
fixCrlfS = BS.concatMap fixCrlf'
  where
    fixCrlf' 10 = BS.pack [13, 10]
    fixCrlf' x  = BS.pack [x]
