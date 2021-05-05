{-# LANGUAGE GADTs #-}

module Mailsh.Message.ContentType
  ( contentTypeParser
  , contentDispositionParser
  , contentType
  , contentDisposition
  ) where

-- This whole module is just an out of tree bugfix. Hopefulle we
-- can remove it soon

import Control.Applicative (many, (<|>))
import Control.Lens
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as P8
import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Data.Maybe (fromMaybe)
import Data.MIME hiding (parseContentType, contentType, contentDisposition)
import Data.MIME.TransferEncoding (TransferEncodingName, transferEncodings)
import qualified Data.IMF
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Reparser

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

-- TODO: This should really be fixed in purebred-email!
-- We modified the parser such that a quoted-string will cause the quotes to
-- stay in the value

parseContentType :: Parser ContentType
parseContentType = do
  typ <- CI.mk <$> token
  _ <- P8.char8 '/'
  subtype <- CI.mk <$> token
  params <- parseParameters
  if typ == "multipart" && "boundary" `notElem` fmap fst params
    then
      -- https://tools.ietf.org/html/rfc2046#section-5.1.1
      fail "\"boundary\" parameter is required for multipart content type"
    else pure $ ContentType typ subtype (Parameters params)

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
    val = token <|> (fmap (\x -> "\"" <> x <> "\"") quotedString) -- <- Here is the change

-- | header token parser
token :: Parser B.ByteString
token =
  takeWhile1 (\c -> c >= 33 && c <= 126 && notInClass "()<>@,;:\\\"/[]?=" c)

-- Now we need to replace the accessor lenses with copies that use
-- the new parser

contentType :: HasHeaders a => Lens' a ContentType
contentType = headers . lens sa sbt where
  sa s = case view cte s of
    Nothing -> contentTypeApplicationOctetStream
    Just _ ->
      fromMaybe defaultContentType $ preview (ct . parsed parseContentType) s

  sbt s b = set (at "Content-Type") (Just (renderContentType b)) s

  ct = header "content-type"
  cte = contentTransferEncoding . to (`lookup` transferEncodings)

contentDisposition :: HasHeaders a => Lens' a (Maybe ContentDisposition)
contentDisposition = headers . at "Content-Disposition" . dimap
  (>>= either (const Nothing) Just . Data.IMF.parse parseContentDisposition)
  (fmap . fmap $ renderContentDisposition)

contentTransferEncoding
  :: (Profunctor p, Contravariant f) => Optic' p f Headers TransferEncodingName
contentTransferEncoding = to $
  fromMaybe "7bit"
  . preview (header "content-transfer-encoding" . caseInsensitive)

caseInsensitive :: CI.FoldCase s => Iso' s (CI.CI s)
caseInsensitive = iso CI.mk CI.original
{-# INLINE caseInsensitive #-}
