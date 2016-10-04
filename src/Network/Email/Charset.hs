module Network.Email.Charset
  ( charset
  , module Data.Encoding
  ) where

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Char8.Utils
import Data.Encoding

charset :: Parser DynEncoding
charset = do
  charsetName <- many1 (noneOf "?")
  case encodingFromStringExplicit charsetName of
    Nothing -> fail $ "Unknown charset: " ++ charsetName
    Just charset -> return charset
