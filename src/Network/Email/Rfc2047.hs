module Network.Email.Rfc2047 where

import qualified Codec.MIME.Base64 as Base64
import qualified Codec.MIME.QuotedPrintable as QuotedPrintable
import Control.Applicative
import Control.Monad.State
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Char8.Utils
import Data.Encoding

import Network.Email.Charset

{-# ANN module "HLint: ignore Use camelCase" #-}

data EncodingType = Base64 | QuotedPrintable
  deriving (Show)
 
encoded_word :: Parser String
encoded_word = do
  char '=' >> char '?'
  charset <- charset
  char '?'
  encoding <- encoding
  char '?'
  text <- encoded_text charset encoding
  char '?' >> char '='
  return text

encoding :: Parser EncodingType
encoding = (oneOf "Qq" >> return QuotedPrintable)
       <|> (oneOf "Bb" >> return Base64)

encoded_text :: DynEncoding -> EncodingType -> Parser String
encoded_text charset encoding = do
  enctext <- many1 (noneOf "?")
  let dectext = case encoding of
        QuotedPrintable -> replaceUnderscores (QuotedPrintable.decode enctext)
        Base64          -> Base64.decodeToString enctext
  case decodeStringExplicit charset dectext of
    Left err   -> fail $ "Decoding error: " ++ show err
    Right text -> return text

replaceUnderscores :: String -> String
replaceUnderscores = map f
  where
    f '_' = ' '
    f x = x
