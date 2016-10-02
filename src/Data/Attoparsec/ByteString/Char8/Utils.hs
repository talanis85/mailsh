module Data.Attoparsec.ByteString.Char8.Utils where

import Data.Attoparsec.ByteString.Char8

oneOf :: String -> Parser Char
oneOf = satisfy . inClass

noneOf :: String -> Parser Char
noneOf = satisfy . notInClass

between :: Parser a -> Parser b -> Parser c -> Parser c
between a b c = do
  a
  r <- c
  b
  return r
