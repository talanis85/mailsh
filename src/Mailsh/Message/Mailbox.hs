module Mailsh.Message.Mailbox
  ( mailboxParser
  , mailboxesParser
  , addressParser
  ) where

import Data.Bifunctor
import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.IMF.Text (renderMailbox, renderAddress, renderMailboxes)
import Data.IMF.Syntax (mk, domainLiteral, dotAtom, localPart)
import Data.MIME hiding (renderMailbox, renderAddress, renderMailboxes, mailbox, address, mailboxList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Reparser

mailboxParser :: Reparser String T.Text Mailbox
mailboxParser = reparser a b
  where
    a = first (("Error parsing mailbox: " ++) . show) . parseOnly mailbox
    b = renderMailbox

addressParser :: Reparser String T.Text Address
addressParser = reparser a b
  where
    a = first (("Error parsing address: " ++) . show) . parseOnly address
    b = renderAddress

mailboxesParser :: Reparser String T.Text [Mailbox]
mailboxesParser = reparser a b
  where
    a = first (("Error parsing mailboxes: " ++) . show) . parseOnly mailboxList
    b = renderMailboxes

tok :: Parser a -> Parser a
tok p = skipSpace *> p <* skipSpace

word :: String -> Parser T.Text
word exceptions = choice
  [ tok (char '"' *> takeWhile1 (notInClass "\"") <* char '"')
  , tok (takeWhile1 (\c -> not (isSpace c) && notInClass ('"':exceptions) c))
  ]

mailbox :: Parser Mailbox
mailbox = choice
  [ do
      skipSpace
      as <- addressSpec
      return $ Mailbox Nothing as
  , do
      words <- many1 (word "<")
      char '<'
      as <- addressSpec
      char '>'

      return $ Mailbox (Just (T.unwords words)) as
  ]

address :: Parser Address
address = choice
  [ do
      words <- many1 (word ":")
      tok (char ':')
      mailboxes <- mailboxList
      return $ Group (T.unwords words) mailboxes
  , Single <$> mailbox
  ]

mailboxList :: Parser [Mailbox]
mailboxList = mailbox `sepBy` tok (char ',')

addressSpec :: Parser AddrSpec
addressSpec = AddrSpec <$> (T.encodeUtf8 <$> localPart) <*> (char '@' *> domain)

domain :: Parser Domain
domain = (DomainDotAtom . fmap (mk . T.encodeUtf8) <$> dotAtom)
         <|> (DomainLiteral . T.encodeUtf8 <$> domainLiteral)
