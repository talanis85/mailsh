module Data.Attoparsec.ByteString.Utils
  ( parseOnlyPretty
  , subparserError
  ) where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import Data.List (intercalate)

parseOnlyPretty :: Parser a -> BS.ByteString -> Either String a
parseOnlyPretty m s = makeResult (parse m s)

subparserError :: String -> String -> String
subparserError thiserr othererr = thiserr ++ "\nBecause of this:\n" ++ (unlines $ map ("  " ++) $ lines othererr)

makeResult r = case r of
  Done remainder result   -> Right result
  Partial resume          -> makeResult (resume BS.empty)
  Fail remainder []   err -> Left $ "Parse error: " ++ err
  Fail remainder ctxs err -> Left $ "Parse error: " ++ err ++ "\nContext:\n" ++ intercalate " > " ctxs

