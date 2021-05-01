module Main where

import Test.Tasty

import qualified Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Types.tests
  ]
