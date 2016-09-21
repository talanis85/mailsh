module Mailsh.Types
  ( Recipient
  , MID
  , ReplyStrategy (..)
  , Flag (..)
  , module Data.Fix
  ) where

import Data.Fix

type Recipient = String

type MID = Integer

data ReplyStrategy = SingleReply | GroupReply
  deriving (Show)

data Flag = FlagOld | FlagNew | FlagReplied
  deriving (Show)
