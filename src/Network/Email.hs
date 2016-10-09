module Network.Email
  ( module Network.Email.Types
  , module Network.Email.Render
  , simpleContentType
  , parseHeaders
  , parseMessage
  , parseNameAddr
  , parseNameAddrs
  , formatHeaders
  ) where

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as P
import Control.Lens
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Network.Email.Rfc2234 (crlf)
import Network.Email.Rfc2822
import Network.Email.Message
import Network.Email.Types
import Network.Email.Render
import System.IO

simpleContentType :: MimeType -> String
simpleContentType t = mimeType t ++ "/" ++ mimeSubtype t

parseHeaders :: P.Parser [Field]
parseHeaders = do
  r <- fields
  crlf
  return r

parseMessage :: MimeType -> [Field] -> P.Parser Body
parseMessage defMime headers =
  let contentType =
        fromMaybe defMime (listToMaybe (lookupField fContentType headers))
      contentTransferEncoding =
        fromMaybe EightBit (listToMaybe (lookupField fContentTransferEncoding headers))
  in encoded_message contentType contentTransferEncoding

parseNameAddr :: P.Parser NameAddr
parseNameAddr = name_addr

parseNameAddrs :: P.Parser [NameAddr]
parseNameAddrs = mailbox_list

formatHeaders :: [IsField] -> [Field] -> String
formatHeaders filter hs = unlines $ concatMap (formatHeader hs) filter
  where
    formatHeader hs (IsField f) = map (formatSingleHeader (fieldName f)) (lookupField f hs)
    formatSingleHeader name value = name ++ ": " ++ showFieldValue value
