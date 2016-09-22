module Mailsh.Printer
  ( Printer (Printer)
  , outputWithPrinter
  , utf8Passthrough
  , headersOnly
  ) where

import Pipes
import qualified Pipes.Attoparsec as PA
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import qualified Pipes.Text.IO as PTIO
import qualified Pipes.Parse as PP
import qualified Data.ByteString as B
import qualified Data.Text as T
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

utf8Passthrough :: Printer'
utf8Passthrough = forAllM (liftIO . TIO.putStr . TE.decodeUtf8)

headersOnly :: Printer'
headersOnly = do
  headers <- PA.parse parseHeaders
  case headers of
    Nothing -> parserError "No headers"
    Just (Left err) -> parserError (show err)
    Just (Right headers) -> do
      mapM_ (liftIO . printHeader) headers
  where
    printHeader (key, value) = printf "%s: %s\n" key value
