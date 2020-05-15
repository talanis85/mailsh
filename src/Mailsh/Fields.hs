{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mailsh.Fields
  ( MsgID (..), formatMsgID
  , Mailbox (..), formatMailbox, formatMailboxShort
  , Field (..)
  , IsField (..)
  , AField (..)
  , fOptionalField, fFrom, fSender, fReturnPath, fReplyTo
  , fTo, fCc, fBcc, fMessageID, fInReplyTo, fReferences
  , fSubject, fComments, fKeywords, fDate, fContentType
  , isOptionalField
  , lookupField, isn't', mkField, filterFields, formatFields
  ) where

import           Prelude hiding (showString)

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
import           Mailsh.Rfc5322Date

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

fOptionalField name = AField name           showOptionalField  renderOptionalField (_OptionalField . isOptionalField (CI.mk name))
fFrom               = AField "From"         showMailboxes      renderMailboxes     _From
fSender             = AField "Sender"       showMailbox        renderMailbox       _Sender
fReturnPath         = AField "ReturnPath"   showString         renderString        _ReturnPath
fReplyTo            = AField "ReplyTo"      showMailboxes      renderMailboxes     _ReplyTo
fTo                 = AField "To"           showMailboxes      renderMailboxes     _To
fCc                 = AField "Cc"           showMailboxes      renderMailboxes     _Cc 
fBcc                = AField "Bcc"          showMailboxes      renderMailboxes     _Bcc
fMessageID          = AField "Message-ID"   showMsgID          renderMsgID         _MessageID
fInReplyTo          = AField "In-Reply-To"  showMsgIDs         renderMsgIDs        _InReplyTo
fReferences         = AField "References"   showMsgIDs         renderMsgIDs        _References
fSubject            = AField "Subject"      showText           renderText          _Subject
fComments           = AField "Comments"     showText           renderText          _Comments
fKeywords           = AField "Keywords"     showTexts          renderTexts         _Keywords
fDate               = AField "Date"         showDate           renderDate          _Date
fContentType        = AField "Content-Type" showMimeType       renderMimeType      _ContentType

showOptionalField = id
showMailbox = formatMailbox
showMailboxes = mconcat . intersperse ", " . map showMailbox
showString = T.pack
showMsgID = formatMsgID
showMsgIDs = mconcat . intersperse ", " . map showMsgID
showText = id
showTexts = mconcat . intersperse ", " . map showText
showDate = T.pack . formatTime defaultTimeLocale "%a %b %d %H:%M (%z)"
showMimeType = formatMimeTypeShort

renderOptionalField = showOptionalField
renderMailbox = showMailbox
renderMailboxes = showMailboxes
renderString = showString
renderMsgID = showMsgID
renderMsgIDs = showMsgIDs
renderText = showText
renderTexts = showTexts
renderDate = T.pack . renderRfc5322Date
renderMimeType = showMimeType

isOptionalField :: CI T.Text -> Prism' (CI T.Text, T.Text) T.Text
isOptionalField key = prism' (\x -> (key, x)) (\(key', x) -> if key' == key then Just x else Nothing)

data AField a = AField
  { fieldName :: T.Text
  , fieldShow :: a -> T.Text
  , fieldRender :: a -> T.Text
  , fieldPrism :: Prism' Field a
  }

data IsField = forall a. IsField (AField a)

lookupField :: AField a -> [Field] -> [a]
lookupField f = mapMaybe (^? fieldPrism f)

isn't' :: IsField -> Field -> Bool
isn't' (IsField x) = isn't (fieldPrism x)

mkField :: AField a -> a -> Field
mkField f v = v ^. re (fieldPrism f)

filterFields :: [IsField] -> [Field] -> [Field]
filterFields f = filter (\x -> any (not . flip isn't' x) f)

formatFields :: Bool -> [IsField] -> [Field] -> T.Text
formatFields showEmpty wanted all = T.unlines (mapMaybe formatField wanted)
  where
    formatField (IsField f) =
      let values = lookupField f all
      in if not showEmpty && null values
            then Nothing
            else Just $ fieldName f <> ": " <> mconcat (intersperse "," (map (fieldShow f) (lookupField f all)))
