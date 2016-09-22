module Mailsh.Printer
  ( Printer (Printer)
  , outputWithPrinter
  , utf8Printer
  , headersOnlyPrinter
  , simplePrinter
  ) where

import Pipes
import qualified Pipes.Attoparsec as PA
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import qualified Pipes.Text.IO as PTIO
import qualified Pipes.Parse as PP
import qualified Data.ByteString as B
import Data.List
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.IO
import Text.Printf

import Mailsh.Message

-- | We need this because type aliases and RankNTypes dont seem to mix nicely.
type Printer' = PP.Parser B.ByteString IO ()
newtype Printer = Printer { getPrinter :: Printer' }

parserError s = liftIO (putStrLn ("Error: " ++ s))

outputWithPrinter :: (MonadIO m) => Printer -> FilePath -> m ()
outputWithPrinter printer fp =
  liftIO $ withFile fp ReadMode $ \hIn ->
    void $ PP.execStateT (getPrinter printer) (PB.fromHandle hIn)

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

utf8Printer :: PP.Parser B.ByteString IO ()
utf8Printer = forAllM (liftIO . TIO.putStr . TE.decodeUtf8)

headersOnlyPrinter :: PP.Parser B.ByteString IO ()
headersOnlyPrinter = do
  hs <- headers
  mapM_ (liftIO . printHeader) hs

simplePrinter :: PP.Parser B.ByteString IO ()
simplePrinter = do
  hs <- headers
  mapM_ (liftIO . printHeader) $ filterHeaders ["From", "To", "Cc", "Bcc", "Reply-To"] hs
  case lookup "Content-Type" hs of
    Nothing -> utf8Printer
    Just ct -> mimePrinter ct

mimePrinter :: String -> PP.Parser B.ByteString IO ()
mimePrinter ct
  | "text/html" `isPrefixOf` ct = textHtmlPrinter
  | otherwise                   = textPlainPrinter

textPlainPrinter :: PP.Parser B.ByteString IO ()
textPlainPrinter = utf8Printer

textHtmlPrinter :: PP.Parser B.ByteString IO ()
textHtmlPrinter = liftIO $ putStrLn "HTML message not shown"
