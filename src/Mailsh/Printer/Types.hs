module Mailsh.Printer.Types
  ( Printer (..)
  , Printer'
  , PrinterOptions (..)
  , defaultPrinterOptions
  ) where

import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Pipes.Parse as PP

import Mailsh.Message

-- | We need this because type aliases and RankNTypes dont seem to mix nicely.
type Printer' a = PP.Parser B.ByteString (ReaderT PrinterOptions IO) a
newtype Printer = Printer { getPrinter :: Printer' () }

data PrinterOptions = PrinterOptions
  { proptHeaders :: [IsField]
  }

defaultPrinterOptions :: PrinterOptions
defaultPrinterOptions = PrinterOptions
  { proptHeaders = [ IsField _To, IsField _From, IsField _Subject, IsField _Cc, IsField _Bcc
                   , IsField _ReplyTo
                   ]
  }
