{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Email.Types
  ( NameAddr (..)
  , GenericMessage (..)
  , Message
  , Field (..)
  , AField (..)
  , IsField (IsField)
  , fOptionalField, fFrom, fSender, fReturnPath, fReplyTo
  , fTo, fCc, fBcc
  -- , fMessageID, fInReplyTo, fReferences
  , fSubject
  -- , fComments, fKeywords
  , fDate
  -- , _Resent*
  -- , fReceived, fObsReceived
  , lookupField, lookupOptionalField, filterFields
  , ShowField (..)
  , formatNameAddr
  ) where

import Control.Lens
import Data.List
import Data.Maybe
import System.Time

-- |A NameAddr is composed of an optional realname a mandatory
-- e-mail 'address'.

data NameAddr = NameAddr { nameAddr_name :: Maybe String
                         , nameAddr_addr :: String
                         }
                deriving (Show,Eq)

-- |This data type repesents a parsed Internet Message as defined in
-- this RFC. It consists of an arbitrary number of header lines,
-- represented in the 'Field' data type, and a message body, which may
-- be empty.

data GenericMessage a = Message [Field] a deriving Show
type Message = GenericMessage String

-- |This data type represents any of the header fields defined in this
-- RFC. Each of the various instances contains with the return value
-- of the corresponding parser.

data Field      = OptionalField       String String
                | From                [NameAddr]
                | Sender              NameAddr
                | ReturnPath          String
                | ReplyTo             [NameAddr]
                | To                  [NameAddr]
                | Cc                  [NameAddr]
                | Bcc                 [NameAddr]
                | MessageID           String
                | InReplyTo           [String]
                | References          [String]
                | Subject             String
                | Comments            String
                | Keywords            [[String]]
                | Date                CalendarTime
                | ResentDate          CalendarTime
                | ResentFrom          [NameAddr]
                | ResentSender        NameAddr
                | ResentTo            [NameAddr]
                | ResentCc            [NameAddr]
                | ResentBcc           [NameAddr]
                | ResentMessageID     String
                | ResentReplyTo       [NameAddr]
                | Received            ([(String,String)], CalendarTime)
                | ObsReceived         [(String,String)]
                deriving (Show)

makePrisms ''Field

fOptionalField name = AField name          (_OptionalField . isOptionalField name)
fFrom               = AField "From"        _From
fSender             = AField "Sender"      _Sender
fReturnPath         = AField "ReturnPath"  _ReturnPath
fReplyTo            = AField "ReplyTo"     _ReplyTo
fTo                 = AField "To"          _To
fCc                 = AField "Cc"          _Cc 
fBcc                = AField "Bcc"         _Bcc
fSubject            = AField "Subject"     _Subject
fDate               = AField "Date"        _Date

isOptionalField :: String -> Prism' (String, String) String
isOptionalField key = prism' (\x -> (key, x)) (\(key', x) -> if key' == key then Just x else Nothing)

data AField a = AField
  { fieldName :: String
  , fieldPrism :: Prism' Field a
  }

data IsField = forall a. (ShowField a) => IsField (AField a)

class ShowField a where
  showFieldValue :: a -> String

instance ShowField [Char] where showFieldValue = id
instance ShowField NameAddr where showFieldValue = formatNameAddr
instance ShowField [NameAddr] where showFieldValue = mconcat . intersperse "," . map formatNameAddr

lookupField :: AField a -> [Field] -> [a]
lookupField f = mapMaybe (^? (fieldPrism f))

isn't' :: IsField -> Field -> Bool
isn't' (IsField x) = isn't (fieldPrism x)

filterFields :: [IsField] -> [Field] -> [Field]
filterFields f = filter (\x -> or (map (not . flip isn't' x) f))

lookupOptionalField :: String -> [Field] -> [String]
lookupOptionalField key = map (dropWhile (== ' '))
                        . lookupField (fOptionalField key)

formatNameAddr :: NameAddr -> String
formatNameAddr na = case nameAddr_name na of
                      Nothing -> nameAddr_addr na
                      Just name -> name ++ " <" ++ nameAddr_addr na ++ ">"
