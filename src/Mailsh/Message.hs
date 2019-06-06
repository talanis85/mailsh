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
  ) where

import           Control.Lens
import qualified Data.ByteString as B
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Monoid (Any (..), Last (..), (<>))
import qualified Data.Text as T
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

{-
collapseAlternatives :: (MimeType -> Bool) -> PartTree -> Maybe PartTree
collapseAlternatives types (MultiPart MultipartAlternative bs) =
  getLast $ mconcat $ map (Last . filterPart types) bs
  where
    filterPart types (SinglePart (PartText t s))
      | types (MimeType "text" t mempty) = Just (SinglePart (PartText t s))
      | otherwise = Nothing
    filterPart types (SinglePart (PartBinary t s))
      | types t   = Just (SinglePart (PartBinary t s))
      | otherwise = Nothing
    filterPart types (MultiPart mt bs) =
      collapseAlternatives types (MultiPart mt bs)
collapseAlternatives types part = Just part
-}

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
