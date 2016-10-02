module Mailsh.Printer
  ( Printer (Printer)
  , outputWithPrinter
  , utf8Printer
  , headersOnlyPrinter
  , module Mailsh.Printer.Simple
  , module Mailsh.Printer.Types
  ) where

import Control.Monad.Reader
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

import Mailsh.Printer.Simple
import Mailsh.Printer.Types
import Mailsh.Printer.Utils
import Mailsh.Parse

import Network.Email

parserError s = liftIO (putStrLn ("Error: " ++ s))

outputWithPrinter :: (MonadIO m) => Printer -> PrinterOptions -> FilePath -> m ()
outputWithPrinter printer propts fp =
  liftIO $ withFile fp ReadMode $ \hIn ->
    void $ runReaderT (PP.execStateT (getPrinter printer) (PB.fromHandle hIn >-> P.map fixCrlfS)) propts

utf8Printer :: Printer' ()
utf8Printer = utf8decoder

headersOnlyPrinter :: Printer' ()
headersOnlyPrinter = do
  hs <- ignoreError <$> PA.parse parseHeaders
  filter <- proptHeaders <$> lift ask
  liftIO $ putStrLn $ formatHeaders filter hs
