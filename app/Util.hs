module Util
  ( joinEither
  , joinMaybe
  ) where

import Control.Monad.Except

joinEither :: (MonadError String m) => String -> m (Either String a) -> m a
joinEither s m = do
  r <- m
  case r of
    Left e -> throwError (s ++ ": " ++ show e)
    Right v -> return v

joinMaybe :: (MonadError String m) => String -> m (Maybe a) -> m a
joinMaybe s m = do
  r <- m
  case r of
    Nothing -> throwError s
    Just v -> return v
