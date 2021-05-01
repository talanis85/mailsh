{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Types where

import qualified Data.Text as T
import Generic.Random
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Reparser
import Mailsh.Types.FilterExp
import Mailsh.Types.Flag
import Mailsh.Types.Limit
import Mailsh.Types.MessageRef

tests :: TestTree
tests = testGroup "Types"
  [ testsFilterExp
  , testsLimit
  , testsMessageNumber
  , testsMessageRef
  , testsPartRef
  ]

testReparseRoundTrip name p =
  let description = "reparse " ++ name ++ " . reprint " ++ name ++ " === id"
  in testProperty description (\x -> reparse p (reprint p x) === Right x)

-- * FilterExp

testsFilterExp :: TestTree
testsFilterExp = testGroup "FilterExp"
  [ testReparseRoundTrip "filterExpParser" filterExpParser
  ]

newtype AlphaString = AlphaString { getAlphaString :: String }
  deriving (Eq, Show)

instance Arbitrary AlphaString where
  arbitrary = AlphaString <$> listOf1 (choose ('a','z'))
  shrink (AlphaString xs) = AlphaString <$> shrink xs

instance Arbitrary FilterExp where
  arbitrary = oneof
    [ FilterFlag <$> arbitrary
    , pure FilterUnseen
    , pure FilterUntrashed
    , pure FilterAll
    , FilterKeyword <$> T.pack <$> getAlphaString <$> arbitrary
    , FilterString <$> T.pack <$> getAlphaString <$> arbitrary
    , FilterReferencedByID <$> T.pack <$> getAlphaString <$> arbitrary
    , FilterReferencedByNumber <$> arbitrary
    , FilterNot <$> arbitrary
    , FilterAnd <$> arbitrary <*> arbitrary
    , FilterOr <$> arbitrary <*> arbitrary
    ]

deriving instance Generic Flag
instance Arbitrary Flag where arbitrary = genericArbitrary uniform

-- * Limit

testsLimit :: TestTree
testsLimit = testGroup "Limit"
  [ testReparseRoundTrip "limitParser" limitParser
  ]

deriving instance Generic Limit
instance Arbitrary Limit where arbitrary = genericArbitrary uniform

-- * MessageNumber

testsMessageNumber :: TestTree
testsMessageNumber = testGroup "MessageNumber"
  [ testReparseRoundTrip "messageNumberParser" messageNumberParser
  ]

deriving instance Generic MessageNumber
instance Arbitrary MessageNumber where arbitrary = genericArbitrary uniform

-- * MessageRef

testsMessageRef :: TestTree
testsMessageRef = testGroup "MessageRef"
  [ testReparseRoundTrip "messageRefParser" messageRefParser
  ]

instance Arbitrary MessageRef where
  arbitrary = oneof
    [ MessageRefNumber <$> arbitrary
    , MessageRefPath <$> T.pack <$> getAlphaString <$> arbitrary
    , pure MessageRefStdin
    ]

-- * PartRef

testsPartRef :: TestTree
testsPartRef = testGroup "PartRef"
  [ testReparseRoundTrip "partRefParser" partRefParser
  ]

deriving instance Generic PartRef
instance Arbitrary PartRef where arbitrary = genericArbitrary uniform
