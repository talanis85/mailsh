{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Mailsh.Types
  ( Recipient
  , MID
  , MaildirFile
  , ReplyStrategy (..)
  , Flag (..)
  , Flags
  , flagD, flagR, flagS, flagT, flagF
  , Mailsh
  , runMailsh
  , runMailshWithMaildir
  , MailshSession
  , msMaildir
  , mkMailshSession
  , module Data.Fix
  ) where

import Control.Monad.Reader
import Control.Lens
import Data.Fix

import Mailsh.Maildir

type Recipient = String

type MID = Int

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

mkMailshSession :: Maildir -> MailshSession
mkMailshSession maildir = MailshSession
  { _msMaildir = maildir
  }

makeLenses ''MailshSession

newtype Mailsh a = Mailsh { _runMailsh :: ReaderT MailshSession IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader MailshSession)

runMailsh :: Mailsh a -> MailshSession -> IO a
runMailsh m = runReaderT (_runMailsh m)

runMailshWithMaildir :: Mailsh a -> FilePath -> IO a
runMailshWithMaildir m fp = do
  md <- openMaildir fp
  runMailsh m (mkMailshSession md)
