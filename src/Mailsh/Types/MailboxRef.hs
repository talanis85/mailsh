module Mailsh.Types.MailboxRef
  ( MailboxRef (..)
  , mailboxRefParser
  ) where

import Data.Bifunctor (first)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

import Data.Reparser

{-
data Mailbox
  = MailboxRelative String
  | MailboxAbsolute FilePath String
-}

data MailboxRef = MailboxRef FilePath

mailboxRefParser :: Reparser String T.Text MailboxRef
mailboxRefParser = reparser parseMailboxRef printMailboxRef

parseMailboxRef str = first (("Mailbox reference parse error: " ++) . show) $ parse (mailboxP <* eof) "<mailbox>" str
  where mailboxP :: Parser MailboxRef
        mailboxP = MailboxRef <$> many1 anyChar

printMailboxRef (MailboxRef x) = T.pack x
