module Main where

import Test.Tasty

import qualified Message
import qualified MIME
import qualified Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ MIME.tests
  , Types.tests
  -- , Message.tests
  ]
