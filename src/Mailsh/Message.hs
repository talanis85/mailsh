module Mailsh.Message
  ( parseHeaders
  , filterHeaders
  , getMessageFileHeaders
  ) where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Lazy as APL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

type Header = (String, String)

parseHeaders :: Parser [Header]
parseHeaders = many parseHeader

parseHeader :: Parser Header
parseHeader = do
  key <- takeWhile1 (notInClass "\n:")
  skipWhile (inClass ": ")
  value <- takeWhile (notInClass "\n")
  satisfy (inClass "\n")
  cont key value <|> final key value
    where
      cont key value = do
        takeWhile1 (inClass " \t")
        value' <- takeWhile (notInClass "\n")
        satisfy (inClass "\n")
        cont key (value <> B.pack [32] <> value') <|> final key (value <> B.pack [32] <> value')
      final key value =
        return (T.unpack (TE.decodeUtf8 key), T.unpack (TE.decodeUtf8 value))

filterHeaders :: [String] -> [Header] -> [Header]
filterHeaders flt = filter f
  where
    f x = fst x `elem` flt

getMessageFileHeaders :: FilePath -> IO (Maybe [Header])
getMessageFileHeaders fp = do
  f <- BL.readFile fp
  return $ APL.maybeResult (APL.parse parseHeaders f)
