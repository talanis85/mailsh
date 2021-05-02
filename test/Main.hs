module Main where

import Test.Tasty

import qualified Types
import qualified MIME

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Types.tests
  , MIME.tests
  ]
