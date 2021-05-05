module Mailsh.Types.Flag
  ( Flag (..)
  , flagChar
  ) where

data Flag = FlagD | FlagR | FlagS | FlagT | FlagF
  deriving (Eq, Show)

flagChar :: Flag -> Char
flagChar FlagD = 'D'
flagChar FlagR = 'R'
flagChar FlagS = 'S'
flagChar FlagT = 'T'
flagChar FlagF = 'F'
