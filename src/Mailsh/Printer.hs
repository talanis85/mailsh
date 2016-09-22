module Mailsh.Printer
  ( Printer (Printer)
  , outputWithPrinter
  , utf8Passthrough
  ) where

import Pipes
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import qualified Pipes.Text.IO as PTIO
import qualified Pipes.Parse as PP
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.IO

-- | We need this because type aliases and RankNTypes dont seem to mix nicely.
type Printer' = PP.Parser B.ByteString IO ()
newtype Printer = Printer { getPrinter :: Printer' }

outputWithPrinter :: (MonadIO m) => Printer -> FilePath -> m ()
outputWithPrinter printer fp =
  liftIO $ withFile fp ReadMode $ \hIn ->
    void $ PP.execStateT (getPrinter printer) (PB.fromHandle hIn)

forAllM :: (Monad m) => (a -> m ()) -> PP.Parser a m ()
forAllM f = PP.foldAllM (const f) (return ()) (const (return ()))

utf8Passthrough :: Printer'
utf8Passthrough = forAllM (liftIO . TIO.putStr . TE.decodeUtf8)
