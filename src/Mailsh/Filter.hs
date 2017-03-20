module Mailsh.Filter
  ( parseFilterExp
  , parseLimit
  ) where

import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Expr
import qualified Data.ByteString.Char8 as B

import Mailsh.Store

parseFilterExp :: String -> Either String (StoreM FilterExp)
parseFilterExp str = case parseOnly (filterExpP <* endOfInput) (B.pack str) of
                      Left err -> Left ("Filter parse error: " ++ show err)
                      Right v -> Right v

filterExpP :: Parser (StoreM FilterExp)
filterExpP = buildExpressionParser
  [ [ Prefix (char '~' >> return (liftM  filterNot))            ]
  , [ Infix  (char '&' >> return (liftM2 filterAnd)) AssocRight ]
  , [ Infix  (char '|' >> return (liftM2 filterOr )) AssocRight ]
  ] filterTermP <?> "filter expression"

filterTermP :: Parser (StoreM FilterExp)
filterTermP = choice
  [ string (B.pack "new") >> return (return filterUnseen)
  , string (B.pack "all") >> return (return filterUntrashed)
  , char 'a' >> return (return filterAll)
  , char 'd' >> return (return (filterFlag 'D'))
  , char 'r' >> return (return (filterFlag 'R'))
  , char 's' >> return (return (filterFlag 'S'))
  , char 't' >> return (return (filterFlag 'T'))
  , char 'f' >> return (return (filterFlag 'F'))
  , char '/' >> (return . filterString <$> B.unpack <$> takeWhile1 (notInClass "/") <* char '/')
  -- , char '[' >> (filterReferencedByNumber . read . B.unpack <$> takeWhile1 (notInClass "]") <* char ']')
  ] <?> "filter term"

parseLimit :: String -> Either String (Maybe Limit)
parseLimit str = case parseOnly (limitP <* endOfInput) (B.pack str) of
                   Left err -> Left ("Limit parse error: " ++ show err)
                   Right v -> Right v

limitP :: Parser (Maybe Limit)
limitP = choice
  [ string (B.pack "auto") >> return Nothing
  , Just . read . B.unpack <$> takeWhile1 isDigit
  ]
