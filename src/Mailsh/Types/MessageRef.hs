module Mailsh.Types.MessageRef
  ( MessageNumber (..)
  , messageNumberParser
  , MessageRef (..)
  , messageRefParser
  , PartRef (..)
  , partRefParser
  ) where

import Data.Bifunctor (first)
import qualified Data.Text as T
import Numeric.Natural
import Text.Parsec
import Text.Parsec.Text

import Data.Reparser

-- * MessageNumber

data MessageNumber = MessageNumberDefault | MessageNumber Natural
  deriving (Eq, Show)

messageNumberParser :: Reparser String T.Text MessageNumber
messageNumberParser = reparser a b
  where
    a = parseMessageNumber
    b = printMessageNumber

parseMessageNumber :: T.Text -> Either String MessageNumber
parseMessageNumber str =
  first (("MessageNumber parse error: " ++) . show) $ parse (messageNumberP <* eof) "<message-number>" str

messageNumberP :: Parser MessageNumber
messageNumberP = choice $ map try
  [ char '_' >> return MessageNumberDefault
  , MessageNumber <$> read <$> many1 digit
  ]

printMessageNumber :: MessageNumber -> T.Text
printMessageNumber MessageNumberDefault = "_"
printMessageNumber (MessageNumber n) = T.pack (show n)

-- * MessageRef

data MessageRef
  = MessageRefNumber MessageNumber
  | MessageRefPath T.Text
  | MessageRefStdin
  deriving (Eq, Show)

messageRefParser :: Reparser String T.Text MessageRef
messageRefParser = reparser a b
  where
    a = parseMessageRef
    b = printMessageRef

parseMessageRef :: T.Text -> Either String MessageRef
parseMessageRef str =
  first (("MessageRef parse error: " ++) . show) $ parse (messageRefP <* eof) "<message-ref>" str

messageRefP :: Parser MessageRef
messageRefP = choice $ map try
  [ MessageRefNumber <$> messageNumberP
  , char '-' >> return MessageRefStdin
  , MessageRefPath <$> T.pack <$> many1 (noneOf "#")
  ]

printMessageRef :: MessageRef -> T.Text
printMessageRef mref = case mref of
  MessageRefNumber n       -> printMessageNumber n
  MessageRefPath p         -> p
  MessageRefStdin          -> "-"

-- * PartRef

data PartRef = PartRef MessageRef Natural
  deriving (Eq, Show)

partRefParser :: Reparser String T.Text PartRef
partRefParser = reparser a b
  where
    a = parsePartRef
    b = printPartRef

parsePartRef :: T.Text -> Either String PartRef
parsePartRef str =
  first (("PartRef parse error: " ++) . show) $ parse (partRefP <* eof) "<part-ref>" str

partRefP :: Parser PartRef
partRefP = choice $ map try
  [ do
      char '#'
      p <- read <$> many1 digit
      return (PartRef (MessageRefNumber MessageNumberDefault) p)
  , do
      mref <- messageRefP
      char '#'
      p <- read <$> many1 digit
      return (PartRef mref p)
  ]

printPartRef :: PartRef -> T.Text
printPartRef (PartRef (MessageRefNumber MessageNumberDefault) p) = "#" <> T.pack (show p)
printPartRef (PartRef mref p) = printMessageRef mref <> "#" <> T.pack (show p)
