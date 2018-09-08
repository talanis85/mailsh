module Email where

import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified Data.Text as T
import Test.HUnit
import Text.Printf
import System.Directory

import Mailsh.Parse
import Network.Email

test :: Test
test = TestList $ map testMsg $ [1..46] \\ [19,25,35,43]
       -- 19, 25, 35 and 43 are WONTFIX

testMsg :: Integer -> Test
testMsg n =
  let filename = printf "msg_%02d.txt" n
      test = TestCase $ do
        file <- BL.readFile ("test/data/" ++ filename)
        let parser = do
              h <- parseHeaders
              b <- parseMessage (mimeTextPlain "utf8") h
              return (b, h)
        let result = if detectCrlf file then parseByteString file parser else parseCrlfByteString file parser
        case result of
          Left err -> assertFailure err
          Right (b, h) -> do
            createDirectoryIfMissing False "test/results"
            writeFile ("test/results/" ++ filename) (prettyHeaders h ++ "\n" ++ prettyBody b)
  in TestLabel filename test

prettyBody :: PartTree -> String
prettyBody (PartSingle (PartText t s)) =
  printf "%s:\n%s\n" ("text/" ++ t) (indentBy 1 (T.unpack s))
prettyBody (PartSingle (PartBinary t s)) =
  printf "%s: (binary)\n" (simpleMimeType t)
prettyBody (PartMulti mt bodies) =
  printf "%s:\n" (show mt) ++ indentBy 1 (unlines $ map prettyBody bodies)

prettyHeaders :: [Field] -> String
prettyHeaders = unlines . map show

indentBy n = unlines . map (replicate (n * 2) ' ' ++) . lines
