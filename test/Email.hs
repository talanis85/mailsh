module Email where

import Test.HUnit
import Text.Printf

import Mailsh.Parse
import Network.Email

test :: Test
test = TestList $ map testMsg [1..46]

testMsg :: Integer -> Test
testMsg n =
  let filename = printf "msg_%02d.txt" n
      test = TestCase $ do
        result <- parseCrlfFile ("test/data/" ++ filename) $ do
          h <- parseHeaders
          parseMessage (mimeTextPlain "utf8") h
        case result of
          Left err -> assertFailure err
          Right _  -> return ()
  in TestLabel filename test
