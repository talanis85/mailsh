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
test = TestList [cpythonTests, myTests]

testFile :: Integer -> String
testFile = printf "msg_%02d.txt"

cpythonTests :: Test
cpythonTests = TestList $ map (testMsg "cpython" . testFile) $ [1..46] \\ [19,25,35,43]
               -- 19, 25, 35 and 43 are WONTFIX

myTests :: Test
myTests = TestList $ map (testMsg "misc" . testFile) $ [1]

testMsg :: String -> String -> Test
testMsg dir filename =
  let test = TestCase $ do
        file <- BL.readFile ("test/data/" ++ dir ++ "/" ++ filename)
        let parser = messageP (mimeTextPlain "utf8")
            result = if detectCrlf file then parseByteString file parser else parseCrlfByteString file parser
        case result of
          Left err -> assertFailure err
          Right (h, b) -> do
            createDirectoryIfMissing True ("test/results/" ++ dir)
            let resultFile = prettyHeaders h ++ "\n" ++ prettyBody b
            writeFile ("test/results/" ++ dir ++ "/" ++ filename) resultFile
            let expectedPath = "test/expected/" ++ dir ++ "/" ++ filename
            haveExpected <- doesFileExist expectedPath
            if haveExpected
            then do
              expectedFile <- readFile expectedPath
              assertEqual "Result does not match expectation" resultFile expectedFile
            else return ()
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
