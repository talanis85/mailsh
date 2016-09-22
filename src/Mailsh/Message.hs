module Mailsh.Message
  ( parseHeaders
  ) where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

parseHeaders :: Parser [(T.Text, T.Text)]
parseHeaders = many parseHeader

parseHeader :: Parser (T.Text, T.Text)
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
        return (TE.decodeUtf8 key, TE.decodeUtf8 value)

