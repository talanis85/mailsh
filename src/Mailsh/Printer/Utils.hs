module Mailsh.Printer.Utils
  ( forAllM
  , utf8decoder
  , formatHeaders
  ) where

import Control.Monad.Trans
import Data.List
import qualified Pipes.Text.IO as PTIO
import qualified Pipes.Parse as PP
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Mailsh.Printer.Types
import Mailsh.Message

forAllM :: (Monad m) => (a -> m ()) -> PP.Parser a m ()
forAllM f = PP.foldAllM (const f) (return ()) (const (return ()))

utf8decoder :: Printer' ()
utf8decoder = forAllM (liftIO . TIO.putStr . TE.decodeUtf8)

formatHeaders :: [IsField] -> [Field] -> String
formatHeaders filter hs = unlines $ map (formatHeader hs) filter
  where
    formatHeader hs (IsField f) = unlines $ map (formatSingleHeader (fieldName f)) (lookupField f hs)
    formatSingleHeader name value = name ++ ": " ++ showFieldValue value
