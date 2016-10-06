module Mailsh.Printer.Simple
  ( simplePrinter
  ) where

import Control.Lens
import Control.Monad.Reader
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Parse as PP
import qualified Pipes.Attoparsec as PA
import qualified Data.ByteString as B
import Data.Maybe
import System.Process
import System.IO

import Mailsh.Printer.Types
import Mailsh.Printer.Utils

import Network.Email

simplePrinter :: Printer' ()
simplePrinter = do
  hs <- parseOrFail parseHeaders
  filter <- proptHeaders <$> lift ask
  liftIO $ putStrLn $ formatHeaders filter hs
  case simpleContentType <$> listToMaybe (lookupField fContentType hs) of
    Nothing -> textPlainPrinter
    Just ct -> mimePrinter ct

mimePrinter :: String -> Printer' ()
mimePrinter mimeType = case mimeType of
                        "text/html" -> w3mPrinter
                        _           -> textPlainPrinter

seePrinter :: String -> Printer' ()
seePrinter mimeType = do
  (Just inH, _, _, procH) <- liftIO $
    createProcess_ "see" (shell ("see " ++ mimeType ++ ":-")) { std_in = CreatePipe }
  feedToHandle inH
  void $ liftIO $ waitForProcess procH
    where
      feedToHandle h = PP.foldAllM (const (liftIO . B.hPut h))
                                   (return ())
                                   (const (liftIO (hClose h)))

w3mPrinter :: Printer' ()
w3mPrinter = do
  (Just inH, _, _, procH) <- liftIO $
    createProcess_ "see" (shell "w3m | cat") { std_in = CreatePipe }
  feedToHandle inH
  void $ liftIO $ waitForProcess procH
    where
      feedToHandle h = PP.foldAllM (const (liftIO . B.hPut h))
                                   (return ())
                                   (const (liftIO (hClose h)))

textPlainPrinter :: Printer' ()
textPlainPrinter = utf8decoder
