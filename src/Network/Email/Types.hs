{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Email.Types
  ( Body (..)
  , MultipartType (..)
  , textBodies, collapseAlternatives
  , outline
  , anyF
  , isSimpleMimeType
  , MsgID (..), formatMsgID
  , NameAddr (..)
  , MimeType (..)
  , simpleMimeType
  , mimeApplicationOctetStream, mimeTextPlain
  , EncodingType (..)
  , Field (..)
  , AField (..)
  , IsField (IsField)
  , fOptionalField, fFrom, fSender, fReturnPath, fReplyTo
  , fTo, fCc, fBcc
  , fMessageID, fInReplyTo, fReferences
  , fSubject
  -- , fComments, fKeywords
  , fDate
  -- , _Resent*
  -- , fReceived, fObsReceived
  , fContentType, fContentTransferEncoding
  , mkField
  , lookupField, lookupOptionalField, filterFields
  , ShowField (..)
  , formatNameAddr, formatNameAddrShort
  ) where

import Control.Lens
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import System.Time
import System.Locale
import Text.Printf
import qualified Data.Text as T
import Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Lazy as BL

{-# ANN module "HLint: ignore Use camelCase" #-}

data Body = BodyText String T.Text
          | BodyBinary MimeType BL.ByteString
          | BodyMultipart MultipartType [Body]

data MultipartType = MultipartMixed | MultipartAlternative
  deriving (Show)

textBodies :: Body -> [(String, T.Text)]
textBodies (BodyText t s)       = [(t, s)]
textBodies (BodyMultipart _ bs) = concatMap textBodies bs
textBodies _                    = []

collapseAlternatives :: (MimeType -> Bool) -> Body -> Maybe Body
collapseAlternatives types (BodyMultipart MultipartAlternative bs) =
  getLast $ mconcat $ map (Last . filterBody types) bs
  where
    filterBody types (BodyText t s)
      | types (MimeType "text" t mempty) = Just (BodyText t s)
      | otherwise = Nothing
    filterBody types (BodyBinary t s)
      | types t   = Just (BodyBinary t s)
      | otherwise = Nothing
    filterBody types (BodyMultipart mt bs) =
      collapseAlternatives types (BodyMultipart mt bs)
collapseAlternatives types body = Just body

outline :: Body -> String
outline = outline' 0
  where
    outline' indent (BodyText t s)
      = replicate (indent * 2) ' ' ++ printf "%-15s: %s\n" ("text/" ++ t) (condensed (T.unpack s))
    outline' indent (BodyBinary t s)
      = replicate (indent * 2) ' ' ++ printf "%-15s: (binary)\n" (simpleMimeType t)
    outline' indent (BodyMultipart mt bodies)
      = replicate (indent * 2) ' ' ++ printf "%s:\n" (show mt) ++ concatMap (outline' (indent+1)) bodies
    condensed = (++ "...") . take 50 . filter (/= '\n')

isSimpleMimeType :: String -> MimeType -> Bool
isSimpleMimeType s t = s == simpleMimeType t

anyF :: [a -> Bool] -> a -> Bool
anyF fs = getAny . foldMap (Any .) fs

newtype MsgID = MsgID { getMsgID :: String }
  deriving (Show, Eq)

formatMsgID :: MsgID -> String
formatMsgID m = "<" ++ getMsgID m ++ ">"

-- |A NameAddr is composed of an optional realname a mandatory
-- e-mail 'address'.

data NameAddr = NameAddr { nameAddr_name :: Maybe String
                         , nameAddr_addr :: String
                         }
                deriving (Show,Eq)

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
                | MessageID           MsgID
                | InReplyTo           [MsgID]
                | References          [MsgID]
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
                | ResentMessageID     MsgID
                | ResentReplyTo       [NameAddr]
                | Received            ([(String,String)], CalendarTime)
                | ObsReceived         [(String,String)]
                | ContentType         MimeType
                | ContentTransferEncoding EncodingType
                deriving (Show)

data MimeType = MimeType
  { mimeType :: String
  , mimeSubtype :: String
  , mimeParams :: Map.Map String String
  }

simpleMimeType :: MimeType -> String
simpleMimeType t = mimeType t ++ "/" ++ mimeSubtype t

mimeApplicationOctetStream :: MimeType
mimeApplicationOctetStream = MimeType
  { mimeType = "application"
  , mimeSubtype = "octet-stream"
  , mimeParams = mempty
  }

mimeTextPlain :: String -> MimeType
mimeTextPlain cs = MimeType
  { mimeType = "text"
  , mimeSubtype = "plain"
  , mimeParams = Map.fromList [("charset", cs)]
  }

instance Show MimeType where
  show t = mimeType t ++ "/" ++ mimeSubtype t ++ concatMap showParam (Map.toList (mimeParams t))
    where showParam (k,v) = ";" ++ k ++ "=" ++ v

data EncodingType = EightBit | Base64 | QuotedPrintable
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
fMessageID          = AField "Message-ID"  _MessageID
fInReplyTo          = AField "In-Reply-To" _InReplyTo
fReferences         = AField "References"  _References
fSubject            = AField "Subject"     _Subject
fDate               = AField "Date"        _Date
fContentType        = AField "Content-Type" _ContentType
fContentTransferEncoding = AField "Content-Transfer-Encoding" _ContentTransferEncoding

isOptionalField :: String -> Prism' (String, String) String
isOptionalField key = prism' (\x -> (key, x)) (\(key', x) -> if key' == key then Just x else Nothing)

data AField a = AField
  { fieldName :: String
  , fieldPrism :: Prism' Field a
  }

data IsField = forall a. (ShowField a) => IsField (AField a)

class ShowField a where
  showFieldValue :: a -> String

instance ShowField [Char] where
  showFieldValue = id
instance ShowField NameAddr where
  showFieldValue = formatNameAddr
instance ShowField [NameAddr] where
  showFieldValue = mconcat . intersperse "," . map formatNameAddr
instance ShowField CalendarTime where
  showFieldValue = formatCalendarTime defaultTimeLocale "%a %b %d %H:%M"
instance ShowField [String] where
  showFieldValue xs = intercalate ", " $ map showFieldValue xs
instance ShowField MsgID where
  showFieldValue = formatMsgID
instance ShowField [MsgID] where
  showFieldValue xs = intercalate ", " $ map showFieldValue xs

lookupField :: AField a -> [Field] -> [a]
lookupField f = mapMaybe (^? fieldPrism f)

isn't' :: IsField -> Field -> Bool
isn't' (IsField x) = isn't (fieldPrism x)

mkField :: AField a -> a -> Field
mkField f v = v ^. re (fieldPrism f)

filterFields :: [IsField] -> [Field] -> [Field]
filterFields f = filter (\x -> any (not . flip isn't' x) f)

lookupOptionalField :: String -> [Field] -> [String]
lookupOptionalField key = map (dropWhile (== ' '))
                        . lookupField (fOptionalField key)

formatNameAddr :: NameAddr -> String
formatNameAddr na = case nameAddr_name na of
                      Nothing -> nameAddr_addr na
                      Just name -> name ++ " <" ++ nameAddr_addr na ++ ">"

formatNameAddrShort :: NameAddr -> String
formatNameAddrShort na = fromMaybe (nameAddr_addr na) (nameAddr_name na)
