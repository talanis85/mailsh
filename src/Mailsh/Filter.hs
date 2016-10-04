{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Mailsh.Filter
  ( FilterExp
  , filterAll
  , filterUnseen
  , parseFilterExp
  , runFilter
  ) where

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Expr
import qualified Data.ByteString.Char8 as B

import Mailsh.Maildir
import Mailsh.Types

type FilterExp = Fix FilterExpF

data FilterExpF a
  = FilterAnd a a
  | FilterOr a a
  | FilterNot a
  | FilterFlag Flag
  | FilterAll
  -- ...
  deriving (Functor, Foldable, Traversable, Show)

filterAnd :: FilterExp -> FilterExp -> FilterExp
filterAnd a b = Fix (FilterAnd a b)

filterOr :: FilterExp -> FilterExp -> FilterExp
filterOr a b = Fix (FilterOr a b)

filterNot :: FilterExp -> FilterExp
filterNot a = Fix (FilterNot a)

filterFlag :: Flag -> FilterExp
filterFlag f = Fix (FilterFlag f)

filterAll :: FilterExp
filterAll = Fix FilterAll

filterUnseen :: FilterExp
filterUnseen = filterNot (filterFlag FlagS) `filterAnd` filterNot (filterFlag FlagT)

parseFilterExp :: String -> Either String FilterExp
parseFilterExp str = case parseOnly (filterExpP <* endOfInput) (B.pack str) of
                      Left err -> Left ("Filter parse error: " ++ show err)
                      Right v -> Right v

runFilter :: FilterExp -> MID -> MaildirM Bool
runFilter f m = cataM (check m) f
  where
    check m (FilterAnd a b) = return (a && b)
    check m (FilterOr a b)  = return (a || b)
    check m (FilterNot a)   = return (not a)
    check m (FilterFlag f)  = hasFlag (flagToChar f) m
    check m (FilterAll)     = return True

filterExpP :: Parser FilterExp
filterExpP = buildExpressionParser
  [ [ Prefix (char '~' >> return filterNot)            ]
  , [ Infix  (char '&' >> return filterAnd) AssocRight ]
  , [ Infix  (char '|' >> return filterOr ) AssocRight ]
  ] filterTermP <?> "filter expression"

filterTermP :: Parser FilterExp
filterTermP = choice
  [ char 'a' >> return filterAll
  , char 'd' >> return (filterFlag FlagD)
  , char 'r' >> return (filterFlag FlagR)
  , char 's' >> return (filterFlag FlagS)
  , char 't' >> return (filterFlag FlagT)
  , char 'f' >> return (filterFlag FlagF)
  ] <?> "filter term"
