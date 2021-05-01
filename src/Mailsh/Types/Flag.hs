module Mailsh.Types.Flag
  ( Flag (..)
  ) where

data Flag = FlagD | FlagR | FlagS | FlagT | FlagF
  deriving (Eq, Show)
