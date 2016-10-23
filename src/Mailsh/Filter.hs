{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Mailsh.Filter
  ( FilterExp
  , filterAll
  , filterNone
  , filterUnseen
  , parseFilterExp
  , runFilter
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Expr
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List

import Network.Email
import Mailsh.Maildir
import Mailsh.Parse
import Mailsh.Types
import Mailsh.MessageNumber

type FilterExp = Fix FilterExpF

data FilterExpF a
  = FilterAnd a a
  | FilterOr a a
  | FilterNot a
  | FilterFlag Flag
  | FilterString String
  | FilterMessageID MsgID
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

filterMessageID :: MsgID -> FilterExp
filterMessageID m = Fix (FilterMessageID m)

filterReferencedByNumber :: Int -> MaildirM FilterExp
filterReferencedByNumber n = do
  mids <- listMaildir
  case lookupMessageUnsafe (unsafeMessageNumber n) mids of
    Nothing -> throwError $ "Message not found: " ++ show n
    Just mid -> do
      fp <- absoluteMaildirFile mid
      headers <- liftIO $ parseCrlfFile fp parseHeaders
      case headers of
        Left err -> throwError $ "Could not parse message " ++ show n ++ ": " ++ err
        Right headers -> do
          let refs = concat $ lookupField fReferences headers
          return $ foldr (filterOr . filterMessageID) filterNone refs

filterAll :: FilterExp
filterAll = Fix FilterAll

filterNone :: FilterExp
filterNone = filterNot filterAll

filterUnseen :: FilterExp
filterUnseen = filterNot (filterFlag FlagS) `filterAnd` filterNot (filterFlag FlagT)

parseFilterExp :: String -> Either String (MaildirM FilterExp)
parseFilterExp str = case parseOnly (filterExpP <* endOfInput) (B.pack str) of
                      Left err -> Left ("Filter parse error: " ++ show err)
                      Right v -> Right v

runFilter :: FilterExp -> MID -> MaildirM Bool
runFilter f m = cataM (check m) f
  where
    check m (FilterAnd a b)            = return (a && b)
    check m (FilterOr a b)             = return (a || b)
    check m (FilterNot a)              = return (not a)
    check m (FilterFlag f)             = hasFlag (flagToChar f) m
    check m (FilterString s)           = containsString s m
    check m (FilterMessageID msgid)    = hasMessageID msgid m
    check m (FilterAll)                = return True

containsString :: String -> MID -> MaildirM Bool
containsString s m = do
  headers <- getHeaders m
  case headers of
    Nothing -> return False
    Just headers -> return $ any (isInfixOfCI s) $ lookupField fSubject headers
  where
    isInfixOfCI a b = map toLower a `isInfixOf` map toLower b

hasMessageID :: MsgID -> MID -> MaildirM Bool
hasMessageID msgid mid = do
  headers <- getHeaders mid
  case headers of
    Nothing -> return False
    Just headers -> return $ msgid `elem` lookupField fMessageID headers

filterExpP :: Parser (MaildirM FilterExp)
filterExpP = buildExpressionParser
  [ [ Prefix (char '~' >> return (liftM  filterNot))            ]
  , [ Infix  (char '&' >> return (liftM2 filterAnd)) AssocRight ]
  , [ Infix  (char '|' >> return (liftM2 filterOr )) AssocRight ]
  ] filterTermP <?> "filter expression"

filterTermP :: Parser (MaildirM FilterExp)
filterTermP = choice
  [ char 'a' >> return (return filterAll)
  , char 'd' >> return (return (filterFlag FlagD))
  , char 'r' >> return (return (filterFlag FlagR))
  , char 's' >> return (return (filterFlag FlagS))
  , char 't' >> return (return (filterFlag FlagT))
  , char 'f' >> return (return (filterFlag FlagF))
  , char '/' >> (return . filterString <$> B.unpack <$> takeWhile1 (notInClass "/") <* char '/')
  , char '[' >> (filterReferencedByNumber . read . B.unpack <$> takeWhile1 (notInClass "]") <* char ']')
  ] <?> "filter term"
