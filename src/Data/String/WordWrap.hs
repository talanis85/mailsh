module Data.String.WordWrap
  ( wordwrap
  ) where

wordwrap :: Int -> String -> String
wordwrap n s = unlines (mconcat (map (wordwrapLine n) (lines s)))

wordwrapLine :: Int -> String -> [String]
wordwrapLine n l = reverse $ map unwords $ fst $ foldl f ([[]],0) (words l)
  where
    f ((x:xs),c) w = if length w + c > n
                        then ([w]:x:xs, 0)
                        else ((x ++ [w]):xs, length w + c + 1)
