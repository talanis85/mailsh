module Mailsh.Filter
  ( parseFilterExp
  , parseLimit
  ) where

import Control.Monad
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Expr

import Mailsh.Store

type Parser = Parsec String ()

parseFilterExp :: String -> Either String (StoreM FilterExp)
parseFilterExp str = case parse (filterExpP <* eof) "<filter>" str of
                      Left err -> Left ("Filter parse error: " ++ show err)
                      Right v -> Right v

filterExpP :: Parser (StoreM FilterExp)
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
