{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mailsh.Fields where

import           Control.Lens
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.List (intersperse)
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Time.Format

import           Mailsh.MimeType

newtype MsgID = MsgID { getMsgID :: T.Text }
  deriving (Show, Read, Eq)

formatMsgID :: MsgID -> T.Text
formatMsgID m = "<" <> getMsgID m <> ">"

data Mailbox = Mailbox
  { mailboxName :: Maybe T.Text
  , mailboxAddr :: T.Text
  }
  deriving (Show, Read, Eq)

formatMailbox :: Mailbox -> T.Text
formatMailbox mb = case mailboxName mb of
  Nothing -> mailboxAddr mb
  Just name -> name <> " <" <> mailboxAddr mb <> ">"

formatMailboxShort :: Mailbox -> T.Text
formatMailboxShort mb = fromMaybe (mailboxAddr mb) (mailboxName mb)

data Field      = OptionalField       (CI T.Text) T.Text
                | From                [Mailbox]
                | Sender              Mailbox
                | ReturnPath          String
                | ReplyTo             [Mailbox]
                | To                  [Mailbox]
                | Cc                  [Mailbox]
                | Bcc                 [Mailbox]
                | MessageID           MsgID
                | InReplyTo           [MsgID]
                | References          [MsgID]
                | Subject             T.Text
                | Comments            T.Text
                | Keywords            [T.Text]
                | Date                UTCTime
                | ContentType         MimeType
                deriving (Show)

makePrisms ''Field

fOptionalField name = AField name          (_OptionalField . isOptionalField (CI.mk name))
fFrom               = AField "From"        _From
fSender             = AField "Sender"      _Sender
fReturnPath         = AField "ReturnPath"  _ReturnPath
fReplyTo            = AField "ReplyTo"     _ReplyTo
fTo                 = AField "To"          _To
fCc                 = AField "Cc"          _Cc 
fBcc                = AField "Bcc"         _Bcc
fMessageID          = AField "Message-ID"  _MessageID
fInReplyTo          = AField "In-Reply-To" _InReplyTo
fReferences         = AField "References"  _References
fSubject            = AField "Subject"     _Subject
fKeywords           = AField "Keywords"    _Keywords
fDate               = AField "Date"        _Date
fContentType        = AField "Content-Type" _ContentType

isOptionalField :: CI T.Text -> Prism' (CI T.Text, T.Text) T.Text
isOptionalField key = prism' (\x -> (key, x)) (\(key', x) -> if key' == key then Just x else Nothing)

data AField a = AField
  { fieldName :: T.Text
  , fieldPrism :: Prism' Field a
  }

data IsField = forall a. (ShowField a) => IsField (AField a)

lookupField :: AField a -> [Field] -> [a]
lookupField f = mapMaybe (^? fieldPrism f)

isn't' :: IsField -> Field -> Bool
isn't' (IsField x) = isn't (fieldPrism x)

mkField :: AField a -> a -> Field
mkField f v = v ^. re (fieldPrism f)

filterFields :: [IsField] -> [Field] -> [Field]
filterFields f = filter (\x -> any (not . flip isn't' x) f)

class ShowField a where
  showFieldValue :: a -> T.Text

instance ShowField [Char] where
  showFieldValue = T.pack
instance ShowField Mailbox where
  showFieldValue = formatMailbox
instance ShowField [Mailbox] where
  showFieldValue = mconcat . intersperse ", " . map formatMailbox
instance ShowField UTCTime where
  showFieldValue = T.pack . formatTime defaultTimeLocale "%a %b %d %H:%M"
instance ShowField [String] where
  showFieldValue = mconcat . intersperse ", " . map T.pack
instance ShowField MsgID where
  showFieldValue = formatMsgID
instance ShowField [MsgID] where
  showFieldValue = mconcat . intersperse ", " . map formatMsgID
instance ShowField T.Text where
  showFieldValue = id

showFieldValues :: (ShowField a) => [a] -> T.Text
showFieldValues = mconcat . intersperse ", " . map showFieldValue

formatFields :: Bool -> [IsField] -> [Field] -> T.Text
formatFields showEmpty wanted all = T.unlines (mapMaybe formatField wanted)
  where
    formatField (IsField f) =
      let values = lookupField f all
      in if not showEmpty && null values
            then Nothing
            else Just $ fieldName f <> ": " <> showFieldValues (lookupField f all)
