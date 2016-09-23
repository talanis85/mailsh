module Mailsh.Printer.Types
  ( Printer (..)
  , Printer'
  , PrinterOptions (..)
  , defaultPrinterOptions
  ) where

import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Pipes.Parse as PP

-- | We need this because type aliases and RankNTypes dont seem to mix nicely.
type Printer' a = PP.Parser B.ByteString (ReaderT PrinterOptions IO) a
newtype Printer = Printer { getPrinter :: Printer' () }

data PrinterOptions = PrinterOptions
  { proptHeaders :: [String]
  }

defaultPrinterOptions :: PrinterOptions
defaultPrinterOptions = PrinterOptions
  { proptHeaders = ["To", "From", "Subject", "Cc", "Bcc", "Reply-To"]
  }
