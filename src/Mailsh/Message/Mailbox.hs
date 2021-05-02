module Mailsh.Message.Mailbox
  ( mailboxParser
  , addressParser
  ) where

import Data.Bifunctor
import Control.Applicative
import Data.Attoparsec.Text
import Data.IMF.Text (renderMailbox, renderAddress)
import Data.IMF.Syntax (mk, domainLiteral, dotAtom, localPart)
import Data.MIME hiding (renderMailbox, renderAddress, mailbox, address)
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

tok :: Parser a -> Parser a
tok p = skipSpace *> p <* skipSpace

mailbox :: Parser Mailbox
mailbox = choice
  [ do
      skipSpace
      as <- addressSpec
      return $ Mailbox Nothing as
  , do
      words <- many1 (tok (takeWhile1 (notInClass "< ")))
      char '<'
      as <- addressSpec
      char '>'

      return $ Mailbox (Just (T.unwords words)) as
  ]

address :: Parser Address
address = choice
  [ do
      name <- tok (takeWhile1 (notInClass ": "))
      tok (char ':')
      mailboxes <- mailbox `sepBy` tok (char ',')
      return $ Group name mailboxes
  , Single <$> mailbox
  ]

addressSpec :: Parser AddrSpec
addressSpec = AddrSpec <$> (T.encodeUtf8 <$> localPart) <*> (char '@' *> domain)

domain :: Parser Domain
domain = (DomainDotAtom . fmap (mk . T.encodeUtf8) <$> dotAtom)
         <|> (DomainLiteral . T.encodeUtf8 <$> domainLiteral)
