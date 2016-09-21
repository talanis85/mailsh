{-# LANGUAGE DeriveFunctor #-}
module Mailsh.Filter where

import Mailsh.Types

type FilterExp = Fix FilterExpF

data FilterExpF a
  = FilterAnd a a
  | FilterOr a a
  | FilterNot a
  | FilterFlag Flag
  | FilterAll
  -- ...
  deriving (Functor, Show)

filterAll :: FilterExp
filterAll = ana (const FilterAll) undefined

parseFilterExp :: String -> Either String FilterExp
parseFilterExp str = Right filterAll
