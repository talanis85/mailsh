{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Mailsh.Types
  ( Recipient
  , MessageNumber
  , ReplyStrategy (..)
  , Flag (..)
  , Flags
  , flagD, flagR, flagS, flagT, flagF
  , module Data.Fix
  ) where

import Control.Monad.Reader
import Control.Lens
import Data.Fix

type Recipient = String

type MessageNumber = Int

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
