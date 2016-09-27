module Mailsh.Printer.Utils
  ( forAllM
  , utf8decoder
  ) where

import Control.Monad.Trans
import qualified Pipes.Text.IO as PTIO
import qualified Pipes.Parse as PP
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Mailsh.Printer.Types

forAllM :: (Monad m) => (a -> m ()) -> PP.Parser a m ()
forAllM f = PP.foldAllM (const f) (return ()) (const (return ()))

utf8decoder :: Printer' ()
utf8decoder = forAllM (liftIO . TIO.putStr . TE.decodeUtf8)
