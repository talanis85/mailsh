{-# LANGUAGE FlexibleContexts #-}
module Mailsh.Message.Rfc2047
  ( rfc2047
  , parseRfc2047
  ) where

import qualified Codec.Binary.Base64.String as Base64

import Data.Char
import Data.Encoding
import Data.Encoding.ASCII
import Data.Encoding.ISO88591
import Data.Encoding.UTF8

import Text.Parsec

data EncodingType = Base64 | QuotedPrintable
  deriving (Show)

parseRfc2047 :: String -> String
parseRfc2047 s = case parse rfc2047 "" s of
                   Left err -> show err
                   Right v -> v

rfc2047 = many whitespaceP >> wordP

whitespaceP = oneOf "\n\r \t"

wordP = choice
  [ try $ do
      w <- encodedWordP
      choice
        [ try $ do
            w' <- secondWordP
            return (w ++ w')
        , do
            w' <- wordP
            return (w ++ w')
        ]
  , do
      c <- anyChar
      w' <- wordP
      return (c : w')
  , eof >> return []
  ]

secondWordP = many whitespaceP >> wordP

encodedWordP = do
  string "=?"
  charset <- charsetP
  string "?"
  encoding <- encodingP
  string "?"
  text <- encodedTextP charset encoding
  string "?="
  return text

charsetP = do
  charsetName <- many1 (noneOf "?")
  case encodingFromStringExplicit charsetName of
    Nothing -> fail $ "Unknown charset: " ++ charsetName
    Just charset -> return charset

encodingP = (oneOf "Qq" >> return QuotedPrintable)
        <|> (oneOf "Bb" >> return Base64)

encodedTextP charset encoding = do
  source <- many1 (noneOf "?")
  case decodeWith encoding source of
    Nothing -> fail $ "Invalid encoding: " ++ show encoding ++ " :: " ++ source
    Just decoded -> case decodeStringExplicit charset decoded of
                      Left err -> fail $ "decodeStringExplicit: " ++ show err
                      Right v  -> return v

decodeWith :: EncodingType -> String -> Maybe String
decodeWith Base64 s =
  case Base64.decode s of
    "" -> Nothing
    s' -> Just s'
decodeWith QuotedPrintable s =
  case parse quotedPrintableP "" s of
    Left err -> fail $ show err
    Right v  -> Just v

quotedPrintableP = many1 characterP
  where
    characterP = escapedCharacterP <|> underscoreP <|> noneOf "?"
    escapedCharacterP = do
      char '='
      d1 <- hexDigit
      d2 <- hexDigit
      return (chr (digitToInt d1 * 16 + digitToInt d2))
    underscoreP = do
      char '_'
      return ' '
