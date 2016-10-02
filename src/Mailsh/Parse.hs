module Mailsh.Parse
  ( parseFile
  ) where

import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as B
import Data.Word

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fp p = maybeResult <$> parse p <$> B.concatMap fixCrlfL <$> B.readFile fp

fixCrlfL :: Word8 -> B.ByteString
fixCrlfL 10 = B.pack [13, 10]
fixCrlfL x  = B.pack [x]
