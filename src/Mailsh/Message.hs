{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Mailsh.Message
  ( Field
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
  , simpleContentType
  , parseHeaders
  , getMessageFileHeaders
  , formatNameAddr
  ) where

import Control.Applicative
import Control.Lens
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Pipes
import qualified Pipes.Attoparsec as PA
import qualified Pipes.ByteString as PB
import qualified Pipes.Parse as PP
import qualified Text.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Rfc2822 as EH
import Network.Email
import System.IO

import Mailsh.Message.Rfc2047

makePrisms ''Field

{-
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
type Traversal s t a b = forall f. (Applicative f) => (String -> f String) -> String -> f String
type Getting r s a = (a -> Const r a) -> s -> Const r s 

Prism' Field String
= Prism Field Field String String
= forall p f. (Choice p, Applicative f) => p String (f String) -> p Field (f Field)

Lens' String String
= Lens String String String String
= forall f. (Functor f) => (String -> f String) -> String -> f String

Prism' String String
= Prism String String String String
= forall p f. (Choice p, Applicative f) => p String (f String) -> p String (f String)

-}

fOptionalField name = AField name          (_OptionalField . isOptionalField name)
fFrom               = AField "From"        (_From . below mimeDecodeNameAddr)
fSender             = AField "Sender"      (_Sender . mimeDecodeNameAddr)
fReturnPath         = AField "ReturnPath"  (_ReturnPath)
fReplyTo            = AField "ReplyTo"     (_ReplyTo . below mimeDecodeNameAddr)
fTo                 = AField "To"          (_To . below mimeDecodeNameAddr)
fCc                 = AField "Cc"          (_Cc . below mimeDecodeNameAddr)
fBcc                = AField "Bcc"         (_Bcc . below mimeDecodeNameAddr)
fSubject            = AField "Subject"     (_Subject . mimeDecodeString)
fDate               = AField "Date"        (_Date)

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
instance ShowField EH.NameAddr where showFieldValue = formatNameAddr
instance ShowField [EH.NameAddr] where showFieldValue = mconcat . intersperse "," . map formatNameAddr

lookupField :: AField a -> [Field] -> [a]
lookupField f = mapMaybe (^? (fieldPrism f))

isn't' :: IsField -> Field -> Bool
isn't' (IsField x) = isn't (fieldPrism x)

filterFields :: [IsField] -> [Field] -> [Field]
filterFields f = filter (\x -> or (map (not . flip isn't' x) f))

lookupOptionalField :: String -> [Field] -> [String]
lookupOptionalField key = map (dropWhile (== ' '))
                        . lookupField (fOptionalField key)

simpleContentType :: String -> String
simpleContentType = takeWhile (\x -> x /= ' ' && x /= ';')

mimeDecodeString :: Prism' String String
mimeDecodeString = prism' id (Just . parseRfc2047)

mimeDecodeNameAddr :: Prism' EH.NameAddr EH.NameAddr
mimeDecodeNameAddr = prism' id $
  \na -> Just $ na { EH.nameAddr_name = parseRfc2047 <$> EH.nameAddr_name na }

parseHeaders :: (Monad m) => PP.Parser B.ByteString m [Field]
parseHeaders = do
  headerPart' <- PA.parse $ do
    lines <- many $ do
      x <- AP.takeWhile1 (AP.notInClass "\n")
      nl <- AP.take 1
      return (x <> nl)
    let fixNL 10 = B.pack [13, 10]
        fixNL x  = B.pack [x]
    return (B.concatMap fixNL (mconcat lines))
    -- TODO clean this up
  case headerPart' of
    Nothing -> return []
    Just (Left err) -> return []
    Just (Right headerPart) ->
      case Parsec.parse EH.fields "" (T.unpack (TE.decodeUtf8 headerPart)) of
        Left err -> error (show err) >> return []
        Right fields -> return fields

getMessageFileHeaders :: FilePath -> IO [Field]
getMessageFileHeaders fp =
  withFile fp ReadMode $ \hIn ->
    PP.evalStateT parseHeaders (PB.fromHandle hIn)

formatNameAddr :: EH.NameAddr -> String
formatNameAddr na = case EH.nameAddr_name na of
                      Nothing -> EH.nameAddr_addr na
                      Just name -> name ++ " <" ++ EH.nameAddr_addr na ++ ">"
