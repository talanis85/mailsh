{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Mailsh.Message
  (
  -- * Re-exports
    Message (..)
  , MIME (..)
  , MIMEMessage
  , WireEntity
  , ByteEntity
  , TextEntity
  , EncStateWire
  , EncStateByte
  , MessageContext
  , Headers
  , message
  , mime
  , parse
  , renderMessage

  -- * Addresses
  , Address (..)
  , Mailbox (..)
  , AddrSpec (..)
  , Domain (..)

  -- * Charsets
  , CharsetLookup
  , defaultCharsets

  -- * Decoding
  , charsetDecoded
  , charsetDecoded'
  , transferDecoded
  , transferDecoded'
  , transferDecodedEmbedError
  , charsetTransferDecoded
  , EncodingError

  -- * Manipulating messages
  , HasHeaders (..)
  , headerList
  , body
  , entities
  , ientities
  , attachments

  -- * Message id
  , MessageID
  , messageIDParser
  , messageIDsParser

  -- * Keywords
  , keywordParser
  , keywordParser'
  , keywordsParser
  , keywordsParser'

  -- * Headers
  , headerTo
  , headerFrom
  , headerCC
  , headerBCC
  , headerSubject
  , headerDate
  , headerReplyTo
  , headerInReplyTo
  , headerReferences
  , headerKeywords
  , headerMessageID
  , headerAttachments

  , ContentType (..)
  , ContentDisposition (..)
  , Parameters (..)
  , module Mailsh.Message.ContentType

  , ctType, ctSubtype
  , defaultContentType

  , headerText

  , DispositionType (..)
  , dispositionType
  , filename

  -- * Attachments
  , AttachmentFile
  , attachmentFile
  , attachmentFilePath
  , attachmentFileName
  , attachmentFileContentType
  , attachmentFileDefaultName
  , attachmentFileParser

  -- * Mailbox
  , module Mailsh.Message.Mailbox

  -- * Compose
  , composed
  , emptyMessage
  , composedToMime

  ) where

import Control.Applicative (many)
import Control.Monad ((>=>))
import Control.Lens
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec.ByteString
import Data.Attoparsec.Text hiding (parse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Builder as Builder
import Data.Bifunctor
import qualified Data.CaseInsensitive as CI
import Data.Either (fromRight)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe, fromMaybe)
import Data.MIME hiding
  ( headerFrom, headerReplyTo, headerTo, headerCC, headerBCC
  , defaultCharsets, contentType, contentDisposition, filename )
import Data.MIME.EncodedWord
import Data.MIME.Charset (charsetPrism)
import Data.Reparser
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Mime (defaultMimeLookup)
import System.FilePath (takeFileName)

import Mailsh.Message.Charsets
import Mailsh.Message.ContentType
import Mailsh.Message.Mailbox

-- * Message id

messageIDParser :: Reparser String T.Text MessageID
messageIDParser = reparser a b
  where
    a = first (("Invalid MessageID: " ++) . show) . Attoparsec.ByteString.parseOnly parseMessageID . T.encodeUtf8
    b = T.decodeUtf8 . renderMessageID

messageIDsParser :: Reparser String T.Text [MessageID]
messageIDsParser = reparser a b
  where
    a = first (("Invalid MessageID list: " ++) . show) . Attoparsec.ByteString.parseOnly (many parseMessageID) . T.encodeUtf8
    b = T.decodeUtf8 . BS.intercalate " " . map renderMessageID

-- * Keywords

keywordParser :: CharsetLookup -> Reparser String T.Text T.Text
keywordParser charsets = reparser a b
  where
    a = Right . decodeEncodedWords charsets . T.encodeUtf8
    b = T.decodeUtf8 . renderKeyword

keywordsParser :: CharsetLookup -> Reparser String T.Text [T.Text]
keywordsParser charsets = reparser a b
  where
    a = Right . T.words . decodeEncodedWords charsets . T.encodeUtf8
    b = T.decodeUtf8 . renderKeywords

keywordParser' :: Reparser String T.Text T.Text
keywordParser' = keywordParser defaultCharsets

keywordsParser' :: Reparser String T.Text [T.Text]
keywordsParser' = keywordsParser defaultCharsets

renderKeyword :: T.Text -> BS.ByteString
renderKeyword = encodeEncodedWords

renderKeywords :: [T.Text] -> BS.ByteString
renderKeywords = BS.intercalate ", " . map renderKeyword

-- * Headers

instance Semigroup Headers where
  Headers a <> Headers b = Headers (a ++ b)

instance Monoid Headers where
  mempty = Headers []

headerSingleToList
  :: (HasHeaders s)
  => (BS.ByteString -> [a])
  -> ([a] -> BS.ByteString)
  -> CI.CI BS.ByteString
  -> Lens' s [a]
headerSingleToList f g k =
  headers . at k . iso (maybe [] f) (\l -> if null l then Nothing else Just (g l))

headerMultiToList
  :: (HasHeaders s)
  => (BS.ByteString -> [a])
  -> ([a] -> BS.ByteString)
  -> CI.CI BS.ByteString
  -> Lens' s [a]
headerMultiToList f g k = lens a b
  where
    a headers = concatMap f (headers ^.. header k)
    b headers values =
      let otherHeaders = filter ((/= k) . fst) (headers ^. headerList)
          result = g values
          newHeaders = if BS.null result then otherHeaders else (k, result) : otherHeaders
      in headers & headerList .~ newHeaders
    -- TODO: define this more point-free

headerMultiSingleToList
  :: (HasHeaders s)
  => (BS.ByteString -> Maybe a)
  -> (a -> BS.ByteString)
  -> CI.CI BS.ByteString
  -> Lens' s [a]
headerMultiSingleToList f g k = lens a b
  where
    a headers = mapMaybe f (headers ^.. header k)
    b headers values =
      (headerList .~ (headers ^.. headerList . traversed . filtered ((k /=) . fst)) ++ map (((,) k) . g) values) headers

headerAddressList :: (HasHeaders a) => CI.CI BS.ByteString -> CharsetLookup -> Lens' a [Address]
headerAddressList k charsets = headerMultiToList
  (fromRight [] . Attoparsec.ByteString.parseOnly (addressList charsets))
  renderAddresses
  k

headerFrom, headerReplyTo, headerTo, headerCC, headerBCC
  :: HasHeaders a => CharsetLookup -> Lens' a [Address]
headerFrom = headerAddressList "From"
headerReplyTo = headerAddressList "Reply-To"
headerTo = headerAddressList "To"
headerCC = headerAddressList "Cc"
headerBCC = headerAddressList "Bcc"

headerKeywords :: HasHeaders a => CharsetLookup -> Lens' a [T.Text]
headerKeywords cl = headerMultiToList
  (T.words . decodeEncodedWords cl)
  renderKeywords
  "Keywords"

-- * Attachments

data AttachmentFile = AttachmentFile
  { _attachmentFilePath :: T.Text
  , _attachmentFileName :: Maybe T.Text
  , _attachmentFileContentType :: Maybe ContentType
  }
  deriving (Eq, Show)

makeLenses ''AttachmentFile

attachmentFile :: T.Text -> Maybe T.Text -> Maybe ContentType -> AttachmentFile
attachmentFile = AttachmentFile

attachmentFileParser :: Reparser String T.Text AttachmentFile
attachmentFileParser = reparser a b
  where
    a = first (("Invalid attachment: " ++) . show) . parseOnly attachmentFileP
    b x =
      let k = case x ^. attachmentFileName of
                Nothing -> ""
                Just name -> "(" <> name <> ") "
          l = case x ^. attachmentFileContentType of
                Nothing -> ""
                Just ct -> "; " <> reprint contentTypeParser ct
      in k <> (x ^. attachmentFilePath) <> l

attachmentFileP :: Parser AttachmentFile
attachmentFileP = do
  skipSpace

  filename <- option Nothing $ Just <$> do
    char '('
    skipSpace
    name <- takeWhile1 (notInClass ")")
    char ')'
    skipSpace
    return name

  path <- takeWhile1 (notInClass ";")
  ct <- option Nothing $ Just <$> do
    char ';'
    skipSpace
    s <- takeText
    case reparse contentTypeParser s of
      Left err -> fail err
      Right x -> return x
  return AttachmentFile
    { _attachmentFilePath = path
    , _attachmentFileName = filename
    , _attachmentFileContentType = ct
    }

headerAttachments :: HasHeaders a => CharsetLookup -> Lens' a [AttachmentFile]
headerAttachments charsets = headerMultiSingleToList
  (reparse' attachmentFileParser . decodeEncodedWords charsets)
  (encodeEncodedWords . reprint attachmentFileParser)
  "Attachment"

attachmentFileDefaultName :: Getter AttachmentFile T.Text
attachmentFileDefaultName = to f
  where
    f a = fromMaybe (fromPath (a ^. attachmentFilePath)) (a ^. attachmentFileName)
    fromPath p = T.pack $ takeFileName $ T.unpack p

-- * Compose mails

type instance MessageContext T.Text = ()

instance RenderMessage T.Text where
  tweakHeaders = id
  buildBody headers body = Just (Builder.byteString (T.encodeUtf8 body))

composed :: Headers -> BodyHandler T.Text
composed headers = RequiredBody $ T.decodeUtf8 <$> Attoparsec.ByteString.takeByteString

emptyMessage :: Message a ()
emptyMessage = Message (Headers []) ()

composedToMime :: TextEntity -> IO MIMEMessage
composedToMime msg = do
  let attachments = msg ^. headerAttachments defaultCharsets
  let msg' = msg & headerAttachments defaultCharsets .~ []

  if null attachments
     then return (composeSinglePart msg')
     else composeMultiPart msg' (NonEmpty.fromList attachments)

composeSinglePart :: TextEntity -> MIMEMessage
composeSinglePart msg = setTextPlainBody (msg ^. body) msg

composeMultiPart :: TextEntity -> NonEmpty.NonEmpty AttachmentFile -> IO MIMEMessage
composeMultiPart msg attachments = do
  let mainPart = createTextPlainMessage (msg ^. body)
  parts <- mapM createAttachmentFromFile' attachments
  return $ createMultipartMixedMessage' (msg ^. headers) "FREESNOWDEN" (mainPart NonEmpty.<| parts)

createMultipartMixedMessage' :: Headers -> BS.ByteString -> NonEmpty.NonEmpty MIMEMessage -> MIMEMessage
createMultipartMixedMessage' h b parts =
  let hdrs = h & contentType .~ (contentTypeMultipartMixed b)
  in Message hdrs (Multipart parts)

createAttachmentFromFile' :: AttachmentFile -> IO MIMEMessage
createAttachmentFromFile' a = do
  attachment <- createAttachmentFromFile t (T.unpack p)
  return $ attachment
    & (contentDisposition . traversed . filename defaultCharsets .~ (a ^. attachmentFileDefaultName))
  where
    t = fromMaybe (lookupContentType p) (a ^. attachmentFileContentType)
    p = a ^. attachmentFilePath

lookupContentType :: T.Text -> ContentType
lookupContentType p =
  fromMaybe contentTypeApplicationOctetStream (reparse' contentTypeParser (T.decodeUtf8 (defaultMimeLookup p)))

-- * Manipulating messages

charsetTransferDecoded :: CharsetLookup -> IndexPreservingGetter WireEntity (Either EncodingError TextEntity)
charsetTransferDecoded charsets = to (view transferDecoded >=> view (charsetDecoded charsets))

transferDecodedEmbedError :: IndexPreservingGetter WireEntity ByteEntity
transferDecodedEmbedError = to f
  where
    f msg@(Message headers b) = case msg ^. transferDecoded' of
      Left err -> Message headers (BS8.pack (show err))
      Right msg' -> msg'

ientities :: IndexedTraversal' Int MIMEMessage WireEntity
ientities = indexing entities

-- * Filenames with encoded words

-- TODO: Fix this upstream in purebred-email. Should probably be fixed in
-- Data.MIME.Parameters, instance HasCharset EncodedParameterValue

utf8 :: Iso' T.Text BS.ByteString
utf8 = iso T.encodeUtf8 T.decodeUtf8

encodedWords :: CharsetLookup -> Iso' BS.ByteString T.Text
encodedWords charsets = iso (decodeEncodedWords charsets) encodeEncodedWords

filename :: (HasParameters a) => CharsetLookup -> Traversal' a T.Text
filename m = filenameParameter . traversed . charsetPrism m . value . utf8 . encodedWords m
