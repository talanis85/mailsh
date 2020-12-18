{-# LANGUAGE OverloadedStrings #-}
module Mailsh.Parse
  ( messageP
  ) where

import qualified Codec.Text.IConv as IConv
import           Control.Applicative
import           Control.Monad
import           Control.Lens
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.CaseInsensitive as CI
import           Data.Foldable (fold)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.MIME as PB
import qualified Data.MIME.Charset as PB
import           Data.List
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
    in MultiPart <$> pure multipartType <*> mapM readPBParts (NonEmpty.toList parts)
  PB.Part bs ->
    let mt = readPBContentType (headers ^. PB.contentType)
        disposition =
          fromMaybe DispositionInline $
            readPBContentDisposition <$> join (headers ^? PB.contentDisposition)
    in if mimeType mt == "text"
          then SinglePart <$> (Part <$> pure disposition <*> (PartText <$> pure (mimeSubtype mt) <*> (transferDecode headers bs >>= charsetDecode headers)))
          else SinglePart <$> (Part <$> pure disposition <*> (PartBinary <$> pure mt <*> transferDecode headers bs))

readPBContentDisposition :: PB.ContentDisposition -> Disposition
readPBContentDisposition cd = case cd ^. PB.dispositionType of
  PB.Inline -> DispositionInline
  PB.Attachment ->
    case cd ^. PB.filenameParameter of
      Nothing -> DispositionAttachment Nothing
      Just (PB.ParameterValue csname lang bs) ->
        case csname of
          Nothing -> DispositionAttachment (Just (defaultDecodeEncodedWords bs))
          _ -> DispositionAttachment (cd ^? PB.filename PB.defaultCharsets)

readPBFields :: PB.Headers -> Either String [Field]
readPBFields (PB.Headers headers) = mapM readPBField headers

subError :: String -> Either String a -> Either String a
subError s m = case m of
                 Right v -> Right v
                 Left err -> Left (s ++ ": " ++ err)

readPBField :: PB.Header -> Either String Field
readPBField (key, value) = case key of
  "from"          -> subError "From" $ From <$> map readPBMailbox <$> parseOnly defaultMailboxList value
  "sender"        -> subError "Sender" $ Sender <$> readPBMailbox <$> parseOnly defaultMailbox value
  "return-path"   -> subError "ReturnPath" $ ReturnPath <$> return (B.unpack value)
  "reply-to"      -> subError "ReplyTo" $ ReplyTo <$> map readPBMailbox <$> parseOnly defaultMailboxList value
  "to"            -> subError "To" $ To <$> map readPBMailbox <$> parseOnly defaultMailboxList value
  "cc"            -> subError "Cc" $ Cc <$> map readPBMailbox <$> parseOnly defaultMailboxList value
  "bcc"           -> subError "Bcc" $ Bcc <$> map readPBMailbox <$> parseOnly defaultMailboxList value
  "message-id"    -> subError "MessageID" $ MessageID <$> parseOnly messageIdP value
  "in-reply-to"   -> subError "InReplyTo" $ InReplyTo <$> parseOnly messageIdsP value
  "references"    -> subError "References" $ References <$> parseOnly messageIdsP value
  "subject"       -> subError "Subject" $ Subject <$> return (defaultDecodeEncodedWords value)
  "comments"      -> subError "Comments" $ Comments <$> return (defaultDecodeEncodedWords value)
  "keywords"      -> subError "Keywords" $ Keywords <$> parseOnly keywordsP value
  "date"          -> subError "Date" $ Date <$> maybe (Left ("Invalid date format: '" ++ B.unpack value ++ "'")) Right (parseRfc5322Date (B.unpack value))
  "content-type"  -> subError "ContentType" $ ContentType <$> readPBContentType <$> parseOnly PB.parseContentType value
  _               -> pure $ OptionalField (CI.map T.decodeUtf8 key) (T.decodeUtf8 value)

defaultMailboxList :: Parser [PB.Mailbox]
defaultMailboxList = PB.mailboxList PB.defaultCharsets

defaultMailbox :: Parser PB.Mailbox
defaultMailbox = PB.mailbox PB.defaultCharsets

defaultDecodeEncodedWords :: B.ByteString -> T.Text
defaultDecodeEncodedWords = PB.decodeEncodedWords PB.defaultCharsets

readPBMailbox :: PB.Mailbox -> Mailbox
readPBMailbox (PB.Mailbox name addr) = Mailbox
  { mailboxName = defaultDecodeEncodedWords <$> T.encodeUtf8 <$> name
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

messageIdsP :: Parser [MsgID]
messageIdsP = many messageIdP

messageIdP :: Parser MsgID
messageIdP = do
  _ <- option "" (many space)
  _ <- char '<'
  t <- takeWhile1 (\x -> not (x `elem` ("\n\r\t >" :: [Char])))
  _ <- char '>'
  _ <- option "" (many space)
  return $ MsgID $ T.decodeUtf8 t

keywordsP :: Parser [T.Text]
keywordsP = keywordP `sepBy` char ','

keywordP :: Parser T.Text
keywordP = mconcat . intersperse " " <$> many1 wordP

wordP :: Parser T.Text
wordP = do
  _ <- option "" (many space)
  t <- takeWhile1 (\x -> not (x `elem` ("\n\r\t ," :: [Char])))
  _ <- option "" (many space)
  return (defaultDecodeEncodedWords t)
