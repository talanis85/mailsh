module Mailsh.Parse
  ( parseCrlfFile
  , parseFile
  , parseMaildirFile
  , parseString
  , fixCrlfL
  , fixCrlfS
  ) where

import Control.Monad.Except
import Control.Monad.Trans
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BChar8
import Data.Word

import Mailsh.Maildir

parseCrlfFile :: FilePath -> Parser a -> IO (Either String a)
parseCrlfFile fp p = eitherResult <$> parse p <$> fixCrlfL <$> B.readFile fp

parseFile :: FilePath -> Parser a -> IO (Either String a)
parseFile fp p = eitherResult <$> parse p <$> B.readFile fp

parseMaildirFile :: MID -> Parser a -> MaildirM a
parseMaildirFile mid p = do
  fp <- absoluteMaildirFile mid
  r <- liftIO $ parseCrlfFile fp p
  case r of
    Left err -> throwError ("Cannot parse message " ++ err)
    Right v  -> return v

parseString :: Parser a -> String -> Maybe a
parseString p s = maybeResult (parse p (BChar8.pack s))

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
