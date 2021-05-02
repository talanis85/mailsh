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

  , ContentType
  , contentType
  , defaultContentType

  , headerText

  , ContentDisposition (..)
  , DispositionType (..)
  , contentDisposition
  , dispositionType
  , filename

  -- * Reparsers
  , mailboxParser
  , addressParser
  , contentTypeParser
  ) where

import Control.Applicative (many)
import Control.Lens
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec.ByteString
import qualified Data.Attoparsec.Text as Attoparsec.Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Bifunctor
import qualified Data.CaseInsensitive as CI
import Data.Either (fromRight)
import Data.MIME
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

headerKeywords :: HasHeaders a => CharsetLookup -> Lens' a [T.Text]
headerKeywords cl = headerSingleToList
  (T.words . decodeEncodedWords cl)
  renderKeywords
  "Keywords"
