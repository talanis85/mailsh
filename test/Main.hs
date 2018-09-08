module Main where

import Test.HUnit

import qualified Email as Email

main :: IO ()
main = do
  runTestTT Email.test
  return ()
