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
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Encoding as Enc

import Network.Email.Rfc2822
import Network.Email.Rfc2234
import Network.Email.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

type MultipartParser = WriterT Any (ReaderT (Maybe B.ByteString) Parser)

(<??>) :: MultipartParser a -> String -> MultipartParser a
(<??>) p msg = mapWriterT (mapReaderT (<?> msg)) p

liftMultipart :: Parser a -> MultipartParser a
liftMultipart = lift . lift

runSinglepart :: MultipartParser a -> Parser a
runSinglepart p = fmap fst (runReaderT (runWriterT p) Nothing)

runMultipart :: MultipartParser a -> Maybe B.ByteString -> Parser (a, Any)
runMultipart p boundary = runReaderT (runWriterT p) boundary

wrapPart :: B.ByteString -> MultipartParser a -> MultipartParser (a, Bool)
wrapPart boundary p = fmap getAny <$> lift (local (const (Just boundary)) (runWriterT p))

type Charset = Maybe Enc.DynEncoding

messageP :: MimeType -> Parser ([Field], PartTree)
messageP deftype = runSinglepart $ messageP' deftype

messageBodyP :: MimeType -> [Field] -> Parser PartTree
messageBodyP deftype h = runSinglepart $ messageBodyP' deftype h

encodedMessageP :: MimeType -> EncodingType -> MultipartParser PartTree
encodedMessageP t e = case mimeType t of
  "multipart" -> do
    case Map.lookup "boundary" (mimeParams t) of
      Nothing ->
        fail "Multipart without boundary"
      Just boundary ->
        PartMulti <$> pure (multipartType (mimeSubtype t)) <*> (multipartP (B.pack boundary) <??> "multipartP")
  "text" -> do
    let charset = Map.lookup "charset" (mimeParams t) >>= Enc.encodingFromStringExplicit
    PartSingle <$> (PartText <$> pure (mimeSubtype t) <*> (singlepartTextP e charset <??> "singlepartTextP"))
  _ ->
    PartSingle <$> (PartBinary <$> pure t <*> (singlepartBinaryP e <??> "singlepartBinaryP"))

multipartType :: String -> MultipartType
multipartType "alternative" = MultipartAlternative
multipartType _             = MultipartMixed

singlepartTextP :: EncodingType -> Charset -> MultipartParser T.Text
singlepartTextP e charset =
  T.filter (/= '\r') <$> decodeCharset charset <$> decodeEncoding e <$> takePart

singlepartBinaryP :: EncodingType -> MultipartParser B.ByteString
singlepartBinaryP e = decodeEncoding e <$> takePart

multipartP :: B.ByteString -> MultipartParser [PartTree]
multipartP boundary = do
  let parts n = do
        (part, last) <- wrapPart boundary (snd <$> messageP' (mimeTextPlain "utf8")) <??> ("part" ++ show n)
        if last
        then takePart >> return [part]
        else do
          rest <- parts (n + 1)
          return (part : rest)

  (_, last) <- wrapPart boundary takePart <??> ("part0")
  if last then return []
          else parts 1

multipartPartContentP :: B.ByteString -> Parser (B.ByteString, Bool)
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
      [ endOfInput >> return (line <> endofline, True)
      , do
        (rest, last) <- multipartPartContentP boundary
        return (line <> endofline <> rest, last)
      ]

takePart :: MultipartParser B.ByteString
takePart = do
  boundary <- ask
  case boundary of
    Nothing -> liftMultipart takeByteString
    Just boundary -> do
      (bs, last) <- liftMultipart $ multipartPartContentP boundary
      tell (Any last)
      return bs

messageHeaderP :: Parser [Field]
messageHeaderP = fields

messageP' :: MimeType -> MultipartParser ([Field], PartTree)
messageP' defMime = do
  h <- liftMultipart messageHeaderP <??> "messageHeaderP"
  liftMultipart $ if null h then return () else (crlf <?> "crlf after headers") >> return ()
  b <- messageBodyP' defMime h <??> "messageBodyP"
  return (h, b)

messageBodyP' :: MimeType -> [Field] -> MultipartParser PartTree
messageBodyP' defMime headers = do
  let contentType =
        fromMaybe defMime (listToMaybe (lookupField fContentType headers))
      contentTransferEncoding =
        fromMaybe EightBit (listToMaybe (lookupField fContentTransferEncoding headers))
      filename =
        listToMaybe (lookupField fContentDisposition headers) >>= attachmentFileName
      contentType' = case filename of
        Nothing -> contentType
        Just filename' -> withMimeParam "name" filename' contentType
  encodedMessageP contentType' contentTransferEncoding <??> "encodedMessageP"

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
