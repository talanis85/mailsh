module Data.Reparser
  ( Reparser
  , reparse
  , reparse'
  , reprint
  , reparser
  , reparserChoice
  ) where

data Reparser e a b = Reparser
  { reparse :: a -> Either e b
  , reprint :: b -> a
  }

reparser :: (a -> Either e b) -> (b -> a) -> Reparser e a b
reparser = Reparser

reparserChoice :: Reparser e a b -> Reparser e a c -> Reparser e a (Either b c)
reparserChoice a b = Reparser
  { reparse = \x -> (Left <$> reparse a x) <> (Right <$> reparse b x)
  , reprint = \x -> either (reprint a) (reprint b) x
  }

reparse' :: Reparser e a b -> a -> Maybe b
reparse' p x = case reparse p x of
                 Left _ -> Nothing
                 Right x -> Just x
