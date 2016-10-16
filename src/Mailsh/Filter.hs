{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Mailsh.Filter
  ( FilterExp
  , filterAll
  , filterUnseen
  , parseFilterExp
  , runFilter
  ) where

import Control.Monad.Trans
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Expr
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List

import Network.Email
import Mailsh.Maildir
import Mailsh.Parse
import Mailsh.Types

type FilterExp = Fix FilterExpF

data FilterExpF a
  = FilterAnd a a
  | FilterOr a a
  | FilterNot a
  | FilterFlag Flag
  | FilterString String
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

filterString :: String -> FilterExp
filterString s = Fix (FilterString s)

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
    check m (FilterAnd a b)  = return (a && b)
    check m (FilterOr a b)   = return (a || b)
    check m (FilterNot a)    = return (not a)
    check m (FilterFlag f)   = hasFlag (flagToChar f) m
    check m (FilterString s) = containsString s m
    check m (FilterAll)      = return True

containsString :: String -> MID -> MaildirM Bool
containsString s m = do
  fp <- absoluteMaildirFile m
  headers <- liftIO $ parseCrlfFile fp parseHeaders
  case headers of
    Left err -> return False
    Right headers -> return $ any (isInfixOfCI s) $ lookupField fSubject headers
  where
    isInfixOfCI a b = map toLower a `isInfixOf` map toLower b

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
  , char '/' >> (filterString . B.unpack <$> takeWhile1 (notInClass "/") <* char '/')
  ] <?> "filter term"
