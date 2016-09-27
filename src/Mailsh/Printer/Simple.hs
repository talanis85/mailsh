module Mailsh.Printer.Simple
  ( simplePrinter
  ) where

import Control.Monad.Reader
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Parse as PP
import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import System.Process
import System.IO

import Mailsh.Printer.Types
import Mailsh.Printer.Utils
import Mailsh.Message

simplePrinter :: Printer' ()
simplePrinter = do
  hs <- parseHeaders
  filter <- proptHeaders <$> lift ask
  mapM_ (liftIO . putStrLn . showField) $ filterFields filter hs
  case simpleContentType <$> listToMaybe (lookupOptionalField "Content-Type" hs) of
    Nothing -> textPlainPrinter
    Just ct -> seePrinter ct

seePrinter :: String -> Printer' ()
seePrinter mimeType = do
  (Just inH, _, _, procH) <- liftIO $
    createProcess_ "see" (shell ("see --nopager " ++ mimeType ++ ":-")) { std_in = CreatePipe }
  feedToHandle inH
  void $ liftIO $ waitForProcess procH
    where
      feedToHandle h = PP.foldAllM (const (liftIO . B.hPut h))
                                   (return ())
                                   (const (liftIO (hClose h)))

textPlainPrinter :: Printer' ()
textPlainPrinter = utf8decoder
