module Mailsh.Message.Utf8
  ( messageUtf8
  ) where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.CaseInsensitive as CI
import Data.Char (ord)
import Data.IMF.Syntax (optionalFWS, isWsp, wsp, ci, crlf, (<<>>), foldMany)
import qualified Data.IMF.Syntax
import Data.MIME (Headers (..), BodyHandler (..), MessageContext, Message (..))
import Data.MIME.EncodedWord
import qualified Data.Text.Encoding as T

-- | A variant of 'message' that accepts utf8 for all header fields that support
--   encoded-words.
messageUtf8 :: (Headers -> BodyHandler a) -> Parser (Message (MessageContext a) a)
messageUtf8 f = fields >>= \hdrs -> Message hdrs <$> case f hdrs of
  RequiredBody b -> crlf *> b
  OptionalBody (b, a) -> optional crlf >>= maybe (pure a) (const b)
  NoBody b -> pure b

fields :: Parser Headers
fields = Headers <$> many field

isFtext :: Char -> Bool
isFtext c = (ord c >= 33 && ord c <= 57) || (ord c >= 59 && ord c <= 126)

field :: Parser (CI.CI B.ByteString, B.ByteString)
field = do
  name <- ci (takeWhile1 isFtext)
  char ':'
  many wsp
  content <- unstructured (supportsEncodedWords name)
  crlf
  return (name, content)

isVchar :: Char -> Bool
isVchar c = ord c >= 0x21

vchar :: Parser Char
vchar = satisfy isVchar

unstructured :: Bool -> Parser B.ByteString
unstructured True = encodeEncodedWords <$> T.decodeUtf8 <$>
  (foldMany (optionalFWS <<>> (B8.singleton <$> vchar))
  <<>> takeWhile isWsp)
unstructured False =
  foldMany (optionalFWS <<>> (B.singleton <$> Data.IMF.Syntax.vchar))
  <<>> takeWhile isWsp

supportsEncodedWords :: CI.CI B.ByteString -> Bool
supportsEncodedWords x = x `elem` headersThatSupportEncodedWords

headersThatSupportEncodedWords :: [CI.CI B.ByteString]
headersThatSupportEncodedWords =
  [ "Subject"
  , "From"
  , "To"
  , "Cc"
  , "Bcc"
  , "Reply-To"
  , "Attachment"
  ]
