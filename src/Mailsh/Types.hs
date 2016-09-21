{-# LANGUAGE TemplateHaskell #-}
module Mailsh.Types
  ( Recipient
  , MID
  , ReplyStrategy (..)
  , Flag (..)
  , module Data.Fix
  ) where

import Control.Monad.Reader
import Control.Lens
import Data.Fix

import Mailsh.Maildir

type Recipient = String

type MID = Integer

data ReplyStrategy = SingleReply | GroupReply
  deriving (Show)

data Flag = FlagOld | FlagNew | FlagReplied
  deriving (Show)

data Flags = Flags
  { _flagD :: Bool
  , _flagR :: Bool
  , _flagS :: Bool
  , _flagT :: Bool
  , _flagF :: Bool
  }

flagsEmpty :: Flags
flagsEmpty = Flags
  { _flagD = False
  , _flagR = False
  , _flagS = False
  , _flagT = False
  , _flagF = False
  }

makeLenses ''Flags

data MailshSession = MailshSession
  { _msMaildir :: Maildir
  }

makeLenses ''MailshSession

data Mailsh a = Mailsh { _runMailsh :: ReaderT MailshSession IO a }

runMailsh :: Mailsh a -> MailshSession -> IO a
runMailsh m = runReaderT (_runMailsh m)
