{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Mailsh.Types
  ( Recipient
  , ReplyStrategy (..)
  , Flag (..)
  , flagToChar
  , Flags
  , flagD, flagR, flagS, flagT, flagF
  , module Data.Fix
  ) where

import Control.Monad.Reader
import Control.Lens
import Data.Fix

type Recipient = String

data ReplyStrategy = SingleReply | GroupReply
  deriving (Show)

data Flag = FlagD | FlagR | FlagS | FlagT | FlagF
  deriving (Show)

flagToChar :: Flag -> Char
flagToChar FlagD = 'D'
flagToChar FlagR = 'R'
flagToChar FlagS = 'S'
flagToChar FlagT = 'T'
flagToChar FlagF = 'F'

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
