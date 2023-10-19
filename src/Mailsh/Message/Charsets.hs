module Mailsh.Message.Charsets
  ( defaultCharsets
  ) where

import Control.Applicative ((<|>))
import Control.Exception (catch)
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.MIME (CharsetLookup)
import qualified Data.MIME
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.Text.ICU.Error as ICU

import System.IO.Unsafe (unsafePerformIO)

type Charset = BS.ByteString -> T.Text

defaultCharsets :: CharsetLookup
defaultCharsets k = Data.MIME.defaultCharsets k <|> icuCharsets k

icuCharsets :: CharsetLookup
icuCharsets k =
  let
    k' = T.unpack . T.decodeUtf8With T.lenientDecode . CI.original $ k
    handler = const (pure Nothing) :: ICU.ICUError -> IO (Maybe a)
    conv = unsafePerformIO $ (Just <$> ICU.open k' Nothing) `catch` handler
  in ICU.toUnicode <$> conv
