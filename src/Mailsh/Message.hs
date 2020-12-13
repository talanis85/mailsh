{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mailsh.Message
  ( Message (..)
  , PartTree (..)
  , Part (..)
  , PartBody (..)
  , MultipartType (..)
  , Disposition (..)
  , allParts, attachmentParts, inlineParts, partType
  , partBody, partFilename
  , textPart, binaryPart
  , collapsedAlternatives
  , outline
  , isSimpleMimeType
  , anyF

  , DigestMessage (..)
  , digestMessage
  , messageAttachments, isAttachment
  ) where

import           Control.Applicative
import           Control.Lens
import qualified Data.ByteString as B
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.List
import           Data.Maybe
import           Data.Monoid (Any (..), Last (..), (<>))
import qualified Data.Text as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Text.Printf

import           Mailsh.Fields
import           Mailsh.MimeType

data Message = Message [Field] PartTree
data PartTree = SinglePart Part | MultiPart MultipartType [PartTree]
data Part = Part Disposition PartBody
data PartBody = PartText (CI T.Text) T.Text | PartBinary MimeType B.ByteString
  deriving (Show)
data Disposition = DispositionInline | DispositionAttachment (Maybe T.Text)

data MultipartType = MultipartMixed | MultipartAlternative
  deriving (Show)

makePrisms ''PartBody

textPart :: Prism' PartBody (CI T.Text, T.Text)
textPart = _PartText

binaryPart :: Prism' PartBody (MimeType, B.ByteString)
binaryPart = _PartBinary

partBody :: Getter Part PartBody
partBody = to f
  where
    f (Part _ x) = x

partFilename :: Fold Part T.Text
partFilename = folding f
  where f (Part (DispositionAttachment fn) _) = fn
        f _ = Nothing

allParts :: Traversal' PartTree Part
allParts f (SinglePart part) = SinglePart <$> f part
allParts f (MultiPart t parts) = MultiPart <$> pure t <*> sequenceA (map (allParts f) parts)

attachmentPart :: Prism' Part (Maybe T.Text, PartBody)
attachmentPart = prism' inj proj
  where
    inj (filename, body) = Part (DispositionAttachment filename) body
    proj (Part (DispositionAttachment filename) body) = Just (filename, body)
    proj _ = Nothing

inlinePart :: Prism' Part PartBody
inlinePart = prism' inj proj
  where
    inj body = Part DispositionInline body
    proj (Part DispositionInline body) = Just body
    proj _ = Nothing

attachmentParts :: Traversal' PartTree (Maybe T.Text, PartBody)
attachmentParts = allParts . attachmentPart

inlineParts :: Traversal' PartTree PartBody
inlineParts = allParts . inlinePart

partType :: Getter PartBody MimeType
partType = to f
  where
    f (PartText st _) = MimeType
      { mimeType = "text"
      , mimeSubtype = st
      , mimeParams = mempty
      }
    f (PartBinary t _) = t

collapsedAlternatives :: (MimeType -> Bool) -> Fold PartTree PartTree
collapsedAlternatives types = folding f
  where
    f (MultiPart MultipartAlternative parts) = getLast $ mconcat $ map (Last . filterPart types) parts
    f part = Just part

    filterPart types (SinglePart (Part d (PartText t s)))
      | types (MimeType "text" t mempty) = Just (SinglePart (Part d (PartText t s)))
      | otherwise = Nothing
    filterPart types (SinglePart (Part d (PartBinary t s)))
      | types t   = Just (SinglePart (Part d (PartBinary t s)))
      | otherwise = Nothing
    filterPart types (MultiPart mt parts) =
      f (MultiPart mt parts)

outline :: PartTree -> String
outline = outline' 0
  where
    outline' indent (SinglePart (Part _ (PartText t s)))
      = replicate (indent * 2) ' ' ++ printf "%-15s: %s\n" ("text/" <> CI.foldedCase t) (condensed (T.unpack s))
    outline' indent (SinglePart (Part _ (PartBinary t s)))
      = replicate (indent * 2) ' ' ++ printf "%-15s: (binary)\n" (formatMimeTypeShort t)
    outline' indent (MultiPart mt bodies)
      = replicate (indent * 2) ' ' ++ printf "%s:\n" (show mt) ++ concatMap (outline' (indent+1)) bodies
    condensed = (++ "...") . take 50 . filter (/= '\n')

isSimpleMimeType :: T.Text -> MimeType -> Bool
isSimpleMimeType s t = s == formatMimeTypeShort t

anyF :: [a -> Bool] -> a -> Bool
anyF fs = getAny . foldMap (Any .) fs

data DigestMessage = DigestMessage
  { messageDate        :: UTCTime
  , messageMessageId   :: MsgID
  , messageFrom        :: [Mailbox]
  , messageTo          :: [Mailbox]
  , messageCc          :: [Mailbox]
  , messageBcc         :: [Mailbox]
  , messageReferences  :: [MsgID]
  , messageReplyTo     :: [Mailbox]
  , messageSubject     :: T.Text
  , messageKeywords    :: [T.Text]
  , messageBody        :: T.Text
  , messageBodyType    :: CI T.Text
  , messageParts       :: [(Maybe T.Text, MimeType)]
  }

digestMessage :: Message -> DigestMessage
digestMessage (Message headers msg) =
  let defaultUTCTime = UTCTime { utctDay = ModifiedJulianDay 0, utctDayTime = 0 }
      (mainBodyType, mainBody) = fromMaybe ("plain", T.pack "NO TEXT") $ firstTextPart msg
  in DigestMessage
    { messageDate         = fromMaybe defaultUTCTime (listToMaybe (lookupField fDate headers))
    , messageMessageId    = fromMaybe (MsgID "") (listToMaybe (lookupField fMessageID headers))
    , messageFrom         = mconcat (lookupField fFrom headers)
    , messageTo           = mconcat (lookupField fTo headers)
    , messageCc           = mconcat (lookupField fCc headers)
    , messageBcc          = mconcat (lookupField fBcc headers)
    , messageReferences   = mconcat (lookupField fReferences headers) `union` mconcat (lookupField fInReplyTo headers)
    , messageReplyTo      = mconcat (lookupField fReplyTo headers)
    , messageSubject      = fromMaybe "" (listToMaybe (lookupField fSubject headers))
    , messageKeywords     = mconcat (lookupField fKeywords headers)
    , messageBody         = mainBody
    , messageBodyType     = mainBodyType
    , messageParts        = msg ^.. allParts . to (\x -> (x ^? partFilename, view (partBody . partType) x))
    }

-- | Get the first text/plain (preferred) or text/html part
firstTextPart :: PartTree -> Maybe (CI T.Text, T.Text)
firstTextPart msg =
  let textPlain t  = mimeType t == "text" && mimeSubtype t == "plain"
      textHtml t   = mimeType t == "text" && mimeSubtype t == "html"
      firstPlain   = msg ^? collapsedAlternatives textPlain . inlineParts . textPart
      firstHtml    = msg ^? collapsedAlternatives textHtml . inlineParts . textPart
  in firstPlain <|> firstHtml

messageAttachments :: DigestMessage -> [(T.Text, MimeType)]
messageAttachments msg = mapMaybe isAttachment (messageParts msg)

isAttachment :: (Maybe T.Text, MimeType) -> Maybe (T.Text, MimeType)
isAttachment (Nothing, _) = Nothing
isAttachment (Just fn, t) = Just (fn, t)
