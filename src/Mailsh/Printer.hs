module Mailsh.Printer
  ( Printer
  , outputWithPrinter
  , utf8Passthrough
  ) where

import Pipes
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import qualified Pipes.Text.IO as PTIO
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO

type Printer = Pipe B.ByteString T.Text IO ()

outputWithPrinter :: (MonadIO m) => Printer -> FilePath -> m ()
outputWithPrinter printer fp =
  liftIO $ withFile fp ReadMode $ \hIn -> runEffect (PB.fromHandle hIn >-> printer >-> PTIO.stdout)

utf8Passthrough :: Printer
utf8Passthrough = P.map TE.decodeUtf8
