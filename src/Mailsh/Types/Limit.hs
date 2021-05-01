module Mailsh.Types.Limit
  ( Limit (..)
  , limitParser
  ) where

import Data.Bifunctor (first)
import qualified Data.Text as T
import Numeric.Natural
import Text.Parsec
import Text.Parsec.Text

import Data.Reparser

data Limit = NoLimit | Limit Natural
  deriving (Eq, Show)

limitParser :: Reparser String T.Text Limit
limitParser = reparser a b
  where
    a = parseLimit
    b = printLimit

parseLimit :: T.Text -> Either String Limit
parseLimit str =
  first (("Limit parse error: " ++) . show) $ parse (limitP <* eof) "<limit>" str

limitP :: Parser Limit
limitP = (choice $ map try
  [ string "auto" >> return NoLimit
  , Limit <$> read <$> many1 digit
  ]) <?> "limit expression"

printLimit :: Limit -> T.Text
printLimit NoLimit = "auto"
printLimit (Limit n) = T.pack (show n)
