module Mailsh.Printer.Types
  ( Printer (..)
  , Printer'
  , PrinterOptions (..)
  , defaultPrinterOptions
  ) where

import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Pipes.Parse as PP

import Network.Email

-- | We need this because type aliases and RankNTypes dont seem to mix nicely.
type Printer' a = PP.Parser B.ByteString (ReaderT PrinterOptions IO) a
newtype Printer = Printer { getPrinter :: Printer' () }

data PrinterOptions = PrinterOptions
  { proptHeaders :: [IsField]
  }

defaultPrinterOptions :: PrinterOptions
defaultPrinterOptions = PrinterOptions
  { proptHeaders = [ IsField fTo, IsField fFrom, IsField fSubject, IsField fCc, IsField fBcc
                   , IsField fReplyTo
                   ]
  }
