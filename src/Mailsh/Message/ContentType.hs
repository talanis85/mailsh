{-# LANGUAGE GADTs #-}

module Mailsh.Message.ContentType
  ( contentTypeParser
  , contentDispositionParser
  ) where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as P8
import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Data.MIME hiding (renderContentType, renderContentDisposition)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Reparser

-- purebred-email's renderContentType and renderContentDisposition unfortunately
-- don't completely do what we want. Let's reimplement them, shall we?

contentTypeParser :: Reparser String T.Text ContentType
contentTypeParser = reparser a b
  where
    a = first (("Error parsing content-type: " ++) . show) . parseOnly parseContentType . T.encodeUtf8
    b = T.decodeUtf8 . renderContentType

contentDispositionParser :: Reparser String T.Text ContentDisposition
contentDispositionParser = reparser a b
  where
    a = first (("Error parsing content-disposition: " ++) . show) . parseOnly parseContentDisposition . T.encodeUtf8
    b = T.decodeUtf8 . renderContentDisposition

renderContentType :: ContentType -> B.ByteString
renderContentType (ContentType typ sub params) =
  CI.original typ <> "/" <> CI.original sub <> printParameters params

renderContentDisposition :: ContentDisposition -> B.ByteString
renderContentDisposition (ContentDisposition typ params) =
  typStr <> printParameters params
  where
    typStr = case typ of Inline -> "inline" ; Attachment -> "attachment"

printParameters :: Parameters -> B.ByteString
printParameters (Parameters xs) =
  foldMap (\(k,v) -> "; " <> CI.original k <> "=\"" <> v <> "\"") xs

-- TODO: Should be exported by purebred-email

parseContentDisposition :: Parser ContentDisposition
parseContentDisposition = ContentDisposition
  <$> (mapDispType <$> CI.mk <$> token)
  <*> (Parameters <$> parseParameters)
  where
    mapDispType s
      | s == "inline" = Inline
      | otherwise = Attachment

parseParameters :: Parser [(CI.CI B.ByteString, B.ByteString)]
parseParameters = many (P8.char8 ';' *> skipWhile (== 32 {-SP-}) *> param)
  where
    param = (,) <$> (CI.mk <$> token) <* P8.char8 '=' <*> val
    val = token <|> quotedString

-- | header token parser
token :: Parser B.ByteString
token =
  takeWhile1 (\c -> c >= 33 && c <= 126 && notInClass "()<>@,;:\\\"/[]?=" c)
