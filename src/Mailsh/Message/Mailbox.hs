module Mailsh.Message.Mailbox
  ( mailboxParser
  , mailboxesParser
  , addressParser
  , addressesParser
  , addrSpecParser
  ) where

import Data.Bifunctor
import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Attoparsec.ByteString as APBS
import Data.Char (isSpace)
import Data.IMF.Text (renderMailbox, renderAddress, renderMailboxes, renderAddressSpec, renderAddresses)
import Data.IMF.Syntax (mk, domainLiteral, dotAtom, localPart)
import Data.MIME hiding (renderMailbox, renderAddress, renderMailboxes, renderAddressSpec
                        , renderAddresses)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Reparser

import Debug.Trace

mailboxParser :: Reparser String T.Text Mailbox
mailboxParser = reparser a b
  where
    a = first (("Error parsing mailbox: " ++) . show) . APBS.parseOnly (mailbox defaultCharsets) . T.encodeUtf8
    b = renderMailbox

addressParser :: Reparser String T.Text Address
addressParser = reparser a b
  where
    a = first (("Error parsing address: " ++) . show) . APBS.parseOnly (address defaultCharsets) . T.encodeUtf8
    b = renderAddress

mailboxesParser :: Reparser String T.Text [Mailbox]
mailboxesParser = reparser a b
  where
    a = first (("Error parsing mailboxes: " ++) . show) . APBS.parseOnly (mailboxList defaultCharsets) . T.encodeUtf8
    b = renderMailboxes

addressesParser :: Reparser String T.Text [Address]
addressesParser = reparser a b
  where
    a = first (("Error parsing addresses: " ++) . show) . APBS.parseOnly (addressList defaultCharsets) . T.encodeUtf8
    b = renderAddresses

addrSpecParser :: Reparser String T.Text AddrSpec
addrSpecParser = reparser a b
  where
    a = first (("Error parsing addr-spec: " ++) . show) . APBS.parseOnly addressSpec . T.encodeUtf8
    b = renderAddressSpec

tok :: Parser a -> Parser a
tok p = skipSpace *> p <* skipSpace

word :: String -> Parser T.Text
word exceptions = choice
  [ tok (char '"' *> takeWhile1 (notInClass "\"") <* char '"')
  , tok (takeWhile1 (\c -> not (isSpace c) && notInClass ('"':exceptions) c))
  ]
