module Mailsh.Printer.Simple
  ( simplePrinter
  ) where

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Parse as PP
import qualified Data.ByteString as B
import Data.List
import System.Process
import System.IO

import Mailsh.Printer.Utils
import Mailsh.Message

simplePrinter :: PP.Parser B.ByteString IO ()
simplePrinter = do
  hs <- headers
  mapM_ (liftIO . printHeader) $ filterHeaders ["From", "To", "Cc", "Bcc", "Reply-To"] hs
  case lookup "Content-Type" hs of
    Nothing -> textPlainPrinter
    Just ct -> mimePrinter ct

mimePrinter :: String -> PP.Parser B.ByteString IO ()
mimePrinter ct
  | "text/html" `isPrefixOf` ct = textHtmlPrinter
  | otherwise                   = textPlainPrinter

textPlainPrinter :: PP.Parser B.ByteString IO ()
textPlainPrinter = utf8decoder

textHtmlPrinter :: PP.Parser B.ByteString IO ()
textHtmlPrinter = do
  (Just inH, _, _, procH) <- liftIO $
    createProcess_ "html viewer" (shell "w3m -T text/html") { std_in = CreatePipe }
  feedToHandle inH
  void $ liftIO $ waitForProcess procH
    where
      feedToHandle h = PP.foldAllM (const (liftIO . B.hPut h))
                                   (return ())
                                   (const (hClose h))
