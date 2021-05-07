module Util
  ( joinEither
  , joinMaybe
  , readFileIfExists
  ) where

import Control.Monad.Except
import System.Directory

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

readFileIfExists :: FilePath -> IO (Maybe String)
readFileIfExists fp = do
  ex <- doesFileExist fp
  if ex
     then Just <$> readFile fp
     else return Nothing
