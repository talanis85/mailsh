module Mailsh.Printer
  ( Printer (Printer)
  , outputWithPrinter
  , utf8Printer
  , headersOnlyPrinter
  , module Mailsh.Printer.Simple
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

import Mailsh.Printer.Utils
import Mailsh.Printer.Simple

-- | We need this because type aliases and RankNTypes dont seem to mix nicely.
type Printer' = PP.Parser B.ByteString IO ()
newtype Printer = Printer { getPrinter :: Printer' }

parserError s = liftIO (putStrLn ("Error: " ++ s))

outputWithPrinter :: (MonadIO m) => Printer -> FilePath -> m ()
outputWithPrinter printer fp =
  liftIO $ withFile fp ReadMode $ \hIn ->
    void $ PP.execStateT (getPrinter printer) (PB.fromHandle hIn)

utf8Printer :: PP.Parser B.ByteString IO ()
utf8Printer = utf8decoder

headersOnlyPrinter :: PP.Parser B.ByteString IO ()
headersOnlyPrinter = do
  hs <- headers
  mapM_ (liftIO . printHeader) hs
