module Mailsh.Message.Charsets
  ( defaultCharsets
  ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.MIME (CharsetLookup)
import qualified Data.MIME
import qualified Data.Text as T
import qualified Data.Text.ICU.Convert as ICU

import System.IO.Unsafe (unsafePerformIO)

type Charset = BS.ByteString -> T.Text

defaultCharsets :: CharsetLookup
defaultCharsets k = Data.MIME.defaultCharsets k <|> icuCharsets k

icuCharsets :: CharsetLookup
icuCharsets k = lookup k icuCharsetList

icuCharsetList :: [(CI.CI BS.ByteString, Charset)]
icuCharsetList =
  [ ("windows-1257", windows_1257)
  , ("windows-1252", windows_1252)
  , ("iso-8859-15", iso_8859_15)
  ]

windows_1252 :: Charset
{-# NOINLINE windows_1252 #-}
windows_1252 = ICU.toUnicode (unsafePerformIO (ICU.open "windows-1252" Nothing))

windows_1257 :: Charset
{-# NOINLINE windows_1257 #-}
windows_1257 = ICU.toUnicode (unsafePerformIO (ICU.open "windows-1257" Nothing))

iso_8859_15 :: Charset
{-# NOINLINE iso_8859_15 #-}
iso_8859_15 = ICU.toUnicode (unsafePerformIO (ICU.open "iso-8859-15" Nothing))
