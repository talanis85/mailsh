module Mailsh.Printer.Utils
  ( forAllM
  , headers
  , printHeader
  , utf8decoder
  ) where

import Pipes
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Text.IO as PTIO
import qualified Pipes.Parse as PP
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Text.Printf

import Mailsh.Message

forAllM :: (Monad m) => (a -> m ()) -> PP.Parser a m ()
forAllM f = PP.foldAllM (const f) (return ()) (const (return ()))

headers :: PP.Parser B.ByteString IO [(String, String)]
headers = do
  headers <- PA.parse parseHeaders
  case headers of
    Nothing -> return []
    Just (Left err) -> return [("Error", show err)]
    Just (Right headers) -> return headers

printHeader :: (String, String) -> IO ()
printHeader (key, value) = printf "%s: %s\n" key value

utf8decoder :: PP.Parser B.ByteString IO ()
utf8decoder = forAllM (liftIO . TIO.putStr . TE.decodeUtf8)
