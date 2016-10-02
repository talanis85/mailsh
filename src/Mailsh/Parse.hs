module Mailsh.Parse
  ( parseFile
  ) where

import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as B

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fp p = maybeResult <$> parse p <$> B.readFile fp
