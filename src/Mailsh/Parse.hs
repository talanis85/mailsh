{-# LANGUAGE OverloadedStrings #-}
module Mailsh.Parse
  ( messageP
  ) where

import qualified Codec.Text.IConv as IConv
import           Control.Lens
import           Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.CaseInsensitive as CI
import           Data.Foldable (fold)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.MIME as PB
import qualified Data.MIME.Charset as PB
import qualified Data.MIME.EncodedWord as PB
import           Data.Monoid
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Mailsh.Fields
import Mailsh.Message
import Mailsh.MimeType
import Mailsh.Rfc5322Date

messageP :: Parser Message
messageP = do
  r <- PB.message PB.mime
  case readPBMessage r of
    Left err -> fail err
    Right v -> return v

readPBMessage :: PB.MIMEMessage -> Either String Message
readPBMessage pbm@(PB.Message headers body) = Message <$> readPBFields headers <*> readPBParts pbm

readPBParts :: PB.MIMEMessage -> Either String PartTree
readPBParts msg@(PB.Message headers body) = case body of
  PB.Multipart parts ->
    let multipartType = if headers ^. PB.contentType ^. PB.ctSubtype == "alternative"
                           then MultipartAlternative
                           else MultipartMixed
    in MultiPart <$> pure multipartType <*> mapM readPBParts parts
  PB.Part bs ->
    let mt = readPBContentType (headers ^. PB.contentType)
        disposition = fromMaybe DispositionInline $ readPBContentDisposition <$> headers ^? PB.contentDisposition
    in if mimeType mt == "text"
          then SinglePart <$> (Part <$> pure disposition <*> (PartText <$> pure (mimeSubtype mt) <*> (transferDecode headers bs >>= charsetDecode headers)))
          else SinglePart <$> (Part <$> pure disposition <*> (PartBinary <$> pure mt <*> transferDecode headers bs))

readPBContentDisposition :: PB.ContentDisposition -> Disposition
readPBContentDisposition cd = case cd ^. PB.dispositionType of
  PB.Inline -> DispositionInline
  PB.Attachment -> DispositionAttachment (cd ^? PB.filename)

readPBFields :: PB.Headers -> Either String [Field]
readPBFields (PB.Headers headers) = mapM readPBField headers

readPBField :: PB.Header -> Either String Field
readPBField (key, value) = case key of
  "from"          -> From <$> map readPBMailbox <$> parseOnly PB.mailboxList value
  "sender"        -> Sender <$> readPBMailbox <$> parseOnly PB.mailbox value
  "return-path"   -> ReturnPath <$> return (B.unpack value)
  "reply-to"      -> ReplyTo <$> map readPBMailbox <$> parseOnly PB.mailboxList value
  "to"            -> To <$> map readPBMailbox <$> parseOnly PB.mailboxList value
  "cc"            -> Cc <$> map readPBMailbox <$> parseOnly PB.mailboxList value
  "bcc"           -> Bcc <$> map readPBMailbox <$> parseOnly PB.mailboxList value
  "message-id"    -> MessageID <$> return (MsgID (T.decodeUtf8 value))
  -- "in-reply-to"   -> TODO
  -- "references"    -> TODO
  "subject"       -> Subject <$> return (PB.decodeEncodedWords value)
  "comments"      -> Comments <$> return (PB.decodeEncodedWords value)
  -- "keywords"      -> TODO
  "date"          -> Date <$> maybe (Left ("Invalid date format: '" ++ B.unpack value ++ "'")) Right (parseRfc5322Date (B.unpack value))
  "content-type"  -> ContentType <$> readPBContentType <$> parseOnly PB.parseContentType value
  _               -> pure $ OptionalField (CI.map T.decodeUtf8 key) (T.decodeUtf8 value)

readPBMailbox :: PB.Mailbox -> Mailbox
readPBMailbox (PB.Mailbox name addr) = Mailbox
  { mailboxName = PB.decodeEncodedWords <$> T.encodeUtf8 <$> name
  , mailboxAddr = readPBAddrSpec addr
  }

readPBAddrSpec :: PB.AddrSpec -> T.Text
readPBAddrSpec (PB.AddrSpec name (PB.DomainLiteral domain)) =
  T.decodeUtf8 name <> "@" <> T.decodeUtf8 domain
readPBAddrSpec (PB.AddrSpec name (PB.DomainDotAtom components)) =
  T.decodeUtf8 name <> "@" <> fold (NonEmpty.intersperse "." (fmap T.decodeUtf8 components))

readPBContentType :: PB.ContentType -> MimeType
readPBContentType (PB.ContentType t st (PB.Parameters ps)) = MimeType
  { mimeType = CI.map T.decodeUtf8 t
  , mimeSubtype = CI.map T.decodeUtf8 st
  , mimeParams = Map.fromList (map convertParam ps)
  }
  where
    convertParam (key, value) = (CI.map T.decodeUtf8 key, T.decodeUtf8 value)

transferDecode :: PB.Headers -> B.ByteString -> Either String B.ByteString
transferDecode headers body =
  let wireMessage = PB.Message headers body :: PB.WireEntity
      result = view PB.transferDecoded wireMessage :: Either PB.EncodingError PB.ByteEntity
  in case result of
       Left _ -> Left "Transfer decoding failed"
       Right (PB.Message _ r) -> Right r

charsetDecode :: PB.Headers -> B.ByteString -> Either String T.Text
charsetDecode headers body =
  let byteMessage = PB.Message headers body :: PB.ByteEntity
      charset = B.unpack <$> CI.foldedCase <$> byteMessage ^. PB.charsetName
  in return $ T.decodeUtf8 $ BL.toStrict $ IConv.convertFuzzy IConv.Transliterate (fromMaybe "utf8" charset) "utf8" $ BL.fromStrict body

{-
detectCrlf :: B.ByteString -> Bool
detectCrlf bs =
  let detect s = do
        (c, s') <- B.uncons s
        if c == 13
           then do
             (c', s'') <- B.uncons s'
             if c' == 10
                then return True
                else return False
           else detect s'
  in fromMaybe False (detect bs)

parseFileAuto :: FilePath -> Parser a -> IO (Either String a)
parseFileAuto fp p = do
  bs <- B.readFile fp
  return $ if detectCrlf bs then parseByteStringCrlf bs p else parseByteStringLf bs p

parseByteStringAuto :: B.ByteString -> Parser a -> Either String a
parseByteStringAuto bs p = if detectCrlf bs then parseByteStringCrlf bs p else parseByteStringLf bs p

parseByteStringLf :: B.ByteString -> Parser a -> Either String a
parseByteStringLf bs p = parseOnly p $ B.toStrict $ fixCrlfL bs

parseByteStringCrlf :: B.ByteString -> Parser a -> Either String a
parseByteStringCrlf bs p = parseOnly p (B.toStrict bs)

parseByteString :: B.ByteString -> Parser a -> Either String a
parseByteString = parseByteStringCrlf

parseString :: Parser a -> String -> Either String a
parseString p s = parseOnly p (BChar8.pack s)

parseStringMaybe :: Parser a -> String -> Maybe a
parseStringMaybe p s = either (const Nothing) Just (parseString p s)

fixCrlfL :: B.ByteString -> B.ByteString
fixCrlfL = BB.toLazyByteString . B.foldr fixCrlf' mempty
  where
    fixCrlf' 10 b = BB.word8 13 <> BB.word8 10 <> b
    fixCrlf' x  b = BB.word8 x <> b

fixCrlfS :: BS.ByteString -> BS.ByteString
fixCrlfS = BS.concatMap fixCrlf'
  where
    fixCrlf' 10 = BS.pack [13, 10]
    fixCrlf' x  = BS.pack [x]
-}
