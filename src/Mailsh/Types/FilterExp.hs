module Mailsh.Types.FilterExp
  ( FilterExp (..)
  , filterExpParser
  ) where

import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time
import Numeric.Natural
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Expr

import Data.Reparser

import Mailsh.Types.Flag

data FilterExp
  = FilterFlag Flag
  | FilterUnseen
  | FilterUntrashed
  | FilterAll
  | FilterKeyword T.Text
  | FilterString T.Text
  | FilterReferencedByID T.Text
  | FilterReferencedByNumber Natural
  | FilterDate (Maybe LocalTime) (Maybe LocalTime)
  | FilterNot FilterExp
  | FilterAnd FilterExp FilterExp
  | FilterOr FilterExp FilterExp
  deriving (Eq, Show)

filterExpParser :: Reparser String T.Text FilterExp
filterExpParser = reparser a b
  where
    a = parseFilterExp
    b = printFilterExp

parseFilterExp :: T.Text -> Either String FilterExp
parseFilterExp str =
  first (("Filter parse error: " ++) . show) $ parse (filterExpP <* eof) "<filter>" str

filterExpP :: Parser FilterExp
filterExpP = buildExpressionParser
  [ [ Prefix (char '~' >> return FilterNot)            ]
  , [ Infix  (char '&' >> return FilterAnd) AssocRight ]
  , [ Infix  (char '|' >> return FilterOr ) AssocRight ]
  ] filterTermP <?> "filter expression"

filterTermP :: Parser FilterExp
filterTermP = (choice $ map try
  [ char '(' *> filterExpP <* char ')'
  , string "new" >> return FilterUnseen
  , string "all" >> return FilterUntrashed
  , char 'a' >> return FilterAll
  , char 'd' >> return (FilterFlag FlagD)
  , char 'r' >> return (FilterFlag FlagR)
  , char 's' >> return (FilterFlag FlagS)
  , char 't' >> return (FilterFlag FlagT)
  , char 'f' >> return (FilterFlag FlagF)
  , char '#' >> FilterKeyword <$> T.pack <$> many1 (noneOf "#") <* char '#'
  , char '/' >> FilterString <$> T.pack <$> many1 (noneOf "/") <* char '/'
  , char '[' >> FilterReferencedByNumber <$> read <$> many1 digit <* char ']'
  , char '<' >> FilterReferencedByID <$> T.pack <$> many1 (noneOf ">") <* char '>'
  , do
    char '@'
    from <- optionMaybe (many1 (noneOf "@-"))
    char '-'
    to <- optionMaybe (many1 (noneOf "@"))
    from' <- sequence (fmap (parseTimeM False defaultTimeLocale dateFormat) from)
    to' <- sequence (fmap (parseTimeM False defaultTimeLocale dateFormat) to)
    char '@'
    return (FilterDate from' to')
  ]) <?> "filter term"

printFilterExp :: FilterExp -> T.Text
printFilterExp x = case x of
  FilterFlag FlagD           -> "d"
  FilterFlag FlagR           -> "r"
  FilterFlag FlagS           -> "s"
  FilterFlag FlagT           -> "t"
  FilterFlag FlagF           -> "f"
  FilterUnseen               -> "new"
  FilterUntrashed            -> "all"
  FilterAll                  -> "a"
  FilterKeyword kw           -> "#" <> kw <> "#"
  FilterString s             -> "/" <> s <> "/"
  FilterReferencedByID mid   -> "<" <> mid <> ">"
  FilterReferencedByNumber n -> "[" <> T.pack (show n) <> "]"
  FilterDate from to         ->
    let from' = fromMaybe "" $ formatTime defaultTimeLocale dateFormat <$> from
        to' = fromMaybe "" $ formatTime defaultTimeLocale dateFormat <$> to
    in "@" <> T.pack from' <> "-" <> T.pack to' <> "@"
  FilterNot a                -> "~(" <> printFilterExp a <> ")"
  FilterOr a b               -> "(" <> printFilterExp a <> ")|(" <> printFilterExp b <> ")"
  FilterAnd a b              -> "(" <> printFilterExp a <> ")&(" <> printFilterExp b <> ")"

dateFormat :: String
dateFormat = "%Y/%m/%d"

{-
filterExpP :: Parser FilterExp
filterExpP = buildExpressionParser
  [ [ Prefix (char '~' >> return (liftM  filterNot))            ]
  , [ Infix  (char '&' >> return (liftM2 filterAnd)) AssocRight ]
  , [ Infix  (char '|' >> return (liftM2 filterOr )) AssocRight ]
  ] filterTermP <?> "filter expression"

filterTermP :: Parser (StoreM FilterExp)
filterTermP = (choice $ map try
  [ char '(' *> filterExpP <* char ')'
  , string "new" >> return (return filterUnseen)
  , string "all" >> return (return filterUntrashed)
  , char 'a' >> return (return filterAll)
  , char 'd' >> return (return (filterFlag 'D'))
  , char 'r' >> return (return (filterFlag 'R'))
  , char 's' >> return (return (filterFlag 'S'))
  , char 't' >> return (return (filterFlag 'T'))
  , char 'f' >> return (return (filterFlag 'F'))
  , char '#' >> (return . filterKeyword <$> T.pack <$> many1 (noneOf "#") <* char '#')
  , char '/' >> (return . filterString <$> T.pack <$> many1 (noneOf "/") <* char '/')
  -- , char '[' >> (filterReferencedByNumber . read . B.unpack <$> takeWhile1 (notInClass "]") <* char ']')
  ]) <?> "filter term"

parseLimit :: String -> Either String (Maybe Limit)
parseLimit str = case parse (limitP <* eof) "<limit>" str of
                   Left err -> Left ("Limit parse error: " ++ show err)
                   Right v -> Right v

limitP :: Parser (Maybe Limit)
limitP = (choice $ map try
  [ string "auto" >> return Nothing
  , Just . read <$> many1 digit
  ]) <?> "limit expression"

-}
