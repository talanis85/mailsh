module Network.Email.Message
  ( encoded_message
  ) where

import Prelude hiding (takeWhile)

import qualified Codec.MIME.Base64 as Base64
import qualified Codec.MIME.QuotedPrintable as QuotedPrintable
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Builder
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Char8.Utils
import qualified Data.Attoparsec.ByteString.Lazy as APL
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Encoding as Enc

import Network.Email.Rfc2822
import Network.Email.Rfc2047
import Network.Email.Rfc2234
import Network.Email.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

type Charset = Maybe Enc.DynEncoding

encoded_message :: MimeType -> EncodingType -> Parser Body
encoded_message t e = do
  case mimeType t of
    "multipart" -> do
      let Just boundary = Map.lookup "boundary" (mimeParams t)
      BodyTree <$> pure (multipartType (mimeSubtype t)) <*> multipartP e boundary
    _ -> do
      let charset = Map.lookup "charset" (mimeParams t) >>= Enc.encodingFromStringExplicit
      BodyLeaf <$> pure t <*> singlepartP e charset

multipartType :: String -> MultipartType
multipartType "alternative" = MultipartAlternative
multipartType _             = MultipartMixed

singlepartP :: EncodingType -> Charset -> Parser T.Text
singlepartP e charset = T.filter (/= '\r') <$> decodeCharset charset <$> decodeEncoding e <$> takeLazyByteString

multipartP :: EncodingType -> String -> Parser [Body]
multipartP e boundary = do
  bs <- decodeEncoding e <$> takeLazyByteString
  let parser = do
        multipartPartP (B.pack boundary)
        many1 (multipartPartP (B.pack boundary))
  case APL.eitherResult (APL.parse parser bs) of
    Left err -> fail ("Could not parse multipart: " ++ err)
    Right v  -> return v

multipartPartP :: B.ByteString -> Parser Body
multipartPartP boundary = do
  part <- toLazyByteString <$> mconcat <$> many (multipartLineP boundary)
  takeLine
  case APL.eitherResult (APL.parse multipartBodyP part) of
    Left err -> fail ("Could not parse part: " ++ err)
    Right v  -> return v

multipartBodyP :: Parser Body
multipartBodyP = do
  headers <- fields
  let contentType =
        fromMaybe mimeApplicationOctetStream (listToMaybe (lookupField fContentType headers))
      contentTransferEncoding =
        fromMaybe EightBit (listToMaybe (lookupField fContentTransferEncoding headers))
  encoded_message contentType contentTransferEncoding

multipartLineP :: B.ByteString -> Parser Builder
multipartLineP boundary = do
  let dashdash = B.pack "--"
      endofline = B.pack "\r\n"
  line <- takeLine
  if line == (dashdash <> boundary) || line == (dashdash <> boundary <> dashdash)
     then fail "End of part"
     else return (byteString (line <> endofline))

decodeEncoding :: EncodingType -> BL.ByteString -> BL.ByteString
decodeEncoding e = fromMaybe (BL.pack "DECODING ERROR") . decodeEncoding' e
  where
    decodeEncoding' e = case e of
      QuotedPrintable ->
        Just . BL.pack . QuotedPrintable.decode . BL.unpack
      Base64          ->
        Just . BL.pack . Base64.decodeToString . BL.unpack
      EightBit        ->
        Just

decodeCharset :: Charset -> BL.ByteString -> T.Text
decodeCharset c = fromMaybe (T.pack "CHARSET ERROR") . decodeCharset' c
  where
    decodeCharset' c = case c of
      Nothing -> Just . TE.decodeUtf8 . BL.toStrict
      Just c  -> fmap T.pack . either (const Nothing) Just . Enc.decodeLazyByteStringExplicit c

takeLine :: Parser B.ByteString
takeLine = takeWhile (notInClass "\r\n") <* crlf
