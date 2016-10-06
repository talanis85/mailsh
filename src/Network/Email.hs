module Network.Email
  ( module Network.Email.Types
  , simpleContentType
  , parseHeaders
  , parseMessage
  ) where

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as P
import Control.Lens
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Network.Email.Rfc2822
import Network.Email.Message
import Network.Email.Types
import System.IO

simpleContentType :: MimeType -> String
simpleContentType t = mimeType t ++ "/" ++ mimeSubtype t

parseHeaders :: P.Parser [Field]
parseHeaders = fields

parseMessage :: [Field] -> P.Parser Body
parseMessage headers =
  let contentType =
        fromMaybe defaultMimeType (listToMaybe (lookupField fContentType headers))
      contentTransferEncoding =
        fromMaybe EightBit (listToMaybe (lookupField fContentTransferEncoding headers))
  in encoded_message contentType contentTransferEncoding
