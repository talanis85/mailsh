module Network.Email.Message
  ( encoded_message
  ) where

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Char8.Utils
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Network.Email.Rfc2822
import Network.Email.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

encoded_message :: [Field] -> Parser T.Text
encoded_message headers = do
  s <- takeByteString
  return (TE.decodeUtf8 s)
