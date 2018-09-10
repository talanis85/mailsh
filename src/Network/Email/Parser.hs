module Network.Email.Parser
  ( messageBodyP
  , messageHeaderP
  , messageP
  , nameAddrP
  , nameAddrsP
  ) where

import Prelude hiding (takeWhile)

import qualified Codec.MIME.Base64 as Base64
import qualified Codec.MIME.QuotedPrintable as QuotedPrintable
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Utils
import qualified Data.Attoparsec.ByteString.Lazy as APL
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Encoding as Enc

import Network.Email.Rfc2822
import Network.Email.Rfc2234
import Network.Email.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

type Charset = Maybe Enc.DynEncoding

encodedMessageP :: MimeType -> EncodingType -> Parser PartTree
encodedMessageP t e = case mimeType t of
  "multipart" -> do
    case Map.lookup "boundary" (mimeParams t) of
      Nothing ->
        fail "Multipart without boundary"
      Just boundary ->
        PartMulti <$> pure (multipartType (mimeSubtype t)) <*> (multipartP e boundary <?> "multipartP")
  "text" -> do
    let charset = Map.lookup "charset" (mimeParams t) >>= Enc.encodingFromStringExplicit
    PartSingle <$> (PartText <$> pure (mimeSubtype t) <*> (singlepartTextP e charset <?> "singlepartTextP"))
  _ ->
    PartSingle <$> (PartBinary <$> pure t <*> (singlepartBinaryP e <?> "singlepartBinaryP"))

multipartType :: String -> MultipartType
multipartType "alternative" = MultipartAlternative
multipartType _             = MultipartMixed

singlepartTextP :: EncodingType -> Charset -> Parser T.Text
singlepartTextP e charset =
  T.filter (/= '\r') <$> decodeCharset charset <$> decodeEncoding e <$> takeByteString

singlepartBinaryP :: EncodingType -> Parser B.ByteString
singlepartBinaryP e = decodeEncoding e <$> takeByteString

multipartP :: EncodingType -> String -> Parser [PartTree]
multipartP e boundary = do
  bs <- decodeEncoding e <$> takeByteString
  let parser n = do
        (part, last) <- multipartPartP boundary <?> ("part" ++ show n)
        if last
        then return [part]
        else do
          rest <- parser (n + 1)
          return (part : rest)
  case parseOnlyPretty (parser 0) bs of
    Left err -> fail (subparserError ("Could not parse multipart (boundary='" ++ boundary ++ "')") err)
    Right [] -> return []
    Right [x] -> return [x]
    Right (x:xs) -> return xs -- Drop first part that says "This is a multi-part message..."

multipartPartContentP :: B.ByteString -> Parser (Builder, Bool)
multipartPartContentP boundary = do
  let dashdash = B.pack "--"
      endofline = B.pack "\r\n"
  line <- takeLine
  if line == (dashdash <> boundary)
  then return (mempty, False)
  else
    if line == (dashdash <> boundary <> dashdash)
    then return (mempty, True)
    else choice
      [ endOfInput >> return (byteString (line <> endofline), True)
      , do
        (rest, last) <- multipartPartContentP boundary
        return (byteString (line <> endofline) <> rest, last)
      ]

multipartPartP :: String -> Parser (PartTree, Bool)
multipartPartP boundary = do
  (partBuilder, last) <- multipartPartContentP (B.pack boundary) <?> "multipartPartContentP"
  let part = toLazyByteString partBuilder
  case APL.eitherResult (APL.parse (messageP (mimeTextPlain "utf8")) part) of
    Left err -> fail (subparserError ("Could not parse part (boundary='" ++ boundary ++ "')") err)
    Right (h, b)  -> return (b, last)

messageHeaderP :: Parser [Field]
messageHeaderP = fields

messageP :: MimeType -> Parser ([Field], PartTree)
messageP defMime = do
  h <- messageHeaderP <?> "messageHeaderP"
  crlf <?> "crlf after headers"
  b <- messageBodyP defMime h <?> "messageBodyP"
  return (h, b)

messageBodyP :: MimeType -> [Field] -> Parser PartTree
messageBodyP defMime headers = do
  headers <- fields
  let contentType =
        fromMaybe defMime (listToMaybe (lookupField fContentType headers))
      contentTransferEncoding =
        fromMaybe EightBit (listToMaybe (lookupField fContentTransferEncoding headers))
      filename =
        listToMaybe (lookupField fContentDisposition headers) >>= attachmentFileName
      contentType' = case filename of
        Nothing -> contentType
        Just filename' -> withMimeParam "name" filename' contentType
  encodedMessageP contentType' contentTransferEncoding <?> "encodedMessageP"

decodeEncoding :: EncodingType -> B.ByteString -> B.ByteString
decodeEncoding e = fromMaybe (B.pack "DECODING ERROR") . decodeEncoding' e
  where
    decodeEncoding' e = case e of
      QuotedPrintable ->
        Just . B.pack . QuotedPrintable.decode . B.unpack
      Base64          ->
        Just . B.pack . Base64.decodeToString . B.unpack
      EightBit        ->
        Just

decodeCharset :: Charset -> B.ByteString -> T.Text
decodeCharset c = fromMaybe (T.pack "CHARSET ERROR") . decodeCharset' c
  where
    decodeCharset' c = case c of
      Nothing -> Just . TE.decodeUtf8With TE.lenientDecode
      Just c  -> fmap T.pack . either (const Nothing) Just . Enc.decodeStrictByteStringExplicit c

takeLine :: Parser B.ByteString
takeLine = (takeWhile (notInClass "\r\n") <* crlf) <?> "takeLine"

nameAddrP :: Parser NameAddr
nameAddrP = name_addr

nameAddrsP :: Parser [NameAddr]
nameAddrsP = mailbox_list
