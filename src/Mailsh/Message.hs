{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Mailsh.Message
  (
  -- * Re-exports
    Message (..)
  , MIME (..)
  , MIMEMessage
  , WireEntity
  , ByteEntity
  , TextEntity
  , message
  , mime
  , parse

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

  -- * Manipulating messages
  , body
  , headers
  , entities

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
  , contentType
  , defaultContentType
  , Parameters (..)

  , headerText

  , ContentDisposition (..)
  , DispositionType (..)
  , contentDisposition
  , dispositionType
  , filename

  -- * Attachments
  , AttachmentFile
  , attachmentFile
  , attachmentFilePath
  , attachmentFileContentType
  , attachmentFileParser

  -- * Reparsers
  , mailboxParser
  , addressParser
  , contentTypeParser
  ) where

import Control.Applicative (many)
import Control.Lens
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec.ByteString
import Data.Attoparsec.Text hiding (parse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Bifunctor
import qualified Data.CaseInsensitive as CI
import Data.Either (fromRight)
import Data.Maybe (mapMaybe)
import Data.MIME hiding (headerFrom, headerReplyTo, headerTo, headerCC, headerBCC)
import Data.MIME.EncodedWord
import Data.Reparser
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time

import Mailsh.Message.Mailbox

contentTypeParser :: Reparser String T.Text ContentType
contentTypeParser = reparser a b
  where
    a = first (("Error parsing content-type: " ++) . show) . Attoparsec.ByteString.parseOnly parseContentType . T.encodeUtf8
    b = showContentType

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
    b headers values = (header k .~ g values) headers
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
  , _attachmentFileContentType :: Maybe ContentType
  }
  deriving (Eq, Show)

makeLenses ''AttachmentFile

attachmentFile :: T.Text -> Maybe ContentType -> AttachmentFile
attachmentFile = AttachmentFile

attachmentFileParser :: Reparser String T.Text AttachmentFile
attachmentFileParser = reparser a b
  where
    a = first (("Invalid attachment: " ++) . show) . parseOnly attachmentFileP
    b x = case x ^. attachmentFileContentType of
            Nothing -> x ^. attachmentFilePath
            Just ct -> x ^. attachmentFilePath <> "; " <> reprint contentTypeParser ct

attachmentFileP :: Parser AttachmentFile
attachmentFileP = do
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
    , _attachmentFileContentType = ct
    }

headerAttachments :: HasHeaders a => CharsetLookup -> Lens' a [AttachmentFile]
headerAttachments charsets = headerMultiSingleToList
  (preview _Right . reparse attachmentFileParser . decodeEncodedWords charsets)
  (encodeEncodedWords . reprint attachmentFileParser)
  "Attachment"
