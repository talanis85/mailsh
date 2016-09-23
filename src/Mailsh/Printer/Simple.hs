module Mailsh.Printer.Simple
  ( simplePrinter
  ) where

import Control.Monad.Reader
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Parse as PP
import qualified Data.ByteString as B
import Data.List
import System.Process
import System.IO

import Mailsh.Printer.Types
import Mailsh.Printer.Utils
import Mailsh.Message

simplePrinter :: Printer' ()
simplePrinter = do
  hs <- headers
  filter <- proptHeaders <$> lift ask
  mapM_ (liftIO . printHeader) $ filterHeaders filter hs
  case lookup "Content-Type" hs of
    Nothing -> textPlainPrinter
    Just ct -> mimePrinter ct

mimePrinter :: String -> Printer' ()
mimePrinter ct
  | "text/html" `isPrefixOf` ct = textHtmlPrinter
  | otherwise                   = textPlainPrinter

textPlainPrinter :: Printer' ()
textPlainPrinter = utf8decoder

textHtmlPrinter :: Printer' ()
textHtmlPrinter = do
  (Just inH, _, _, procH) <- liftIO $
    createProcess_ "html viewer" (shell "w3m -T text/html") { std_in = CreatePipe }
  feedToHandle inH
  void $ liftIO $ waitForProcess procH
    where
      feedToHandle h = PP.foldAllM (const (liftIO . B.hPut h))
                                   (return ())
                                   (const (liftIO (hClose h)))
