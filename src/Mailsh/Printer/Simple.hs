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
  liftIO $ putStrLn $ replicate 78 '-'
  liftIO $ putStr $ formatHeaders filter hs
  liftIO $ putStrLn $ replicate 78 '-'
  msg <- parseOrFail (parseMessage mimeTextPlain hs)
  let (t, body) = head (bodiesOf ["text/plain", "text/html"] msg)
  liftIO $ mimeOut t body

mimeOut :: MimeType -> String -> IO ()
mimeOut mimeType = case simpleMimeType mimeType of
                     "text/html" -> w3mOut
                     _           -> textPlainOut

w3mOut :: String -> IO ()
w3mOut s = do
  hFlush stdout
  (Just inH, _, _, procH) <- liftIO $
    createProcess_ "see" (shell "w3m -T text/html | cat") { std_in = CreatePipe }
  hPutStrLn inH s
  void $ waitForProcess procH

textPlainOut :: String -> IO ()
textPlainOut = putStrLn
