{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Message ( tests ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Char (isPrint, isSpace)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Reparser
import Mailsh.Message

tests :: TestTree
tests = testGroup "Message"
  [ testsAddress
  , testsContentType
  -- , testsMessageID -- TODO
  -- , testsKeyword -- TODO
  , testsAttachmentFile
  ]

testReparseRoundTrip name p =
  let description = "reparse " ++ name ++ " . reprint " ++ name ++ " === id"
  in testProperty description (\x -> reparse p (reprint p x) === Right x)

-- * Address/Mailbox

instance Arbitrary Domain where
  arbitrary = oneof
    [ DomainDotAtom <$> genDomainDotAtom
    -- purebred-email's renderAddress is wrong for DomainLiterals!
    -- TODO: Make pull request
    -- , DomainLiteral <$> genDomainLiteral
    ]

instance Arbitrary AddrSpec where
  arbitrary = AddrSpec <$> genLocalPart <*> arbitrary

instance Arbitrary Mailbox where
  arbitrary = Mailbox <$> genMailboxName <*> arbitrary

instance Arbitrary Address where
  arbitrary = oneof [ Single <$> arbitrary, Group <$> genAddressGroupName <*> arbitrary ]

genAddressGroupName :: Gen T.Text
genAddressGroupName = do
  first <- arbitrary `suchThat` (\c -> isPrint c && not (isSpace c) && c `notElem` ("\":" :: String))
  s <- T.strip <$> T.pack <$> (first :) <$>
    listOf (arbitrary `suchThat` (\c -> isPrint c && c `notElem` ("\":" :: String)))
  return s

genMailboxName :: Gen (Maybe T.Text)
genMailboxName = do
  s <- T.strip <$> T.pack <$> listOf (arbitrary `suchThat` (\c -> isPrint c && c /= '"'))
  if T.null s
    then return Nothing
    else return (Just s)

genLocalPart :: Gen BS.ByteString
genLocalPart = BS8.pack <$> genAtom

genDomainDotAtom :: Gen (NonEmpty.NonEmpty (CI.CI BS.ByteString))
genDomainDotAtom = NonEmpty.fromList <$> listOf1 (CI.mk <$> BS8.pack <$> genAtom)

genAtom :: Gen String
genAtom = listOf1 (arbitrary `suchThat` (`elem` ("-A-Za-z0-9!#$%&'*+/=?^_`{|}~" :: String)))

genDomainLiteral :: Gen BS.ByteString
genDomainLiteral = do
  a <- elements [0..255] :: Gen Int
  b <- elements [0..255] :: Gen Int
  c <- elements [0..255] :: Gen Int
  d <- elements [0..255] :: Gen Int
  return $ BS8.pack $ show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d

testsAddress :: TestTree
testsAddress = testGroup "Address/Mailbox"
  [ testReparseRoundTrip "addressParser" addressParser
  , testReparseRoundTrip "mailboxParser" mailboxParser
  , testReparseRoundTrip "addrSpecParser" addrSpecParser
  ]

-- * ContentType

instance Arbitrary ContentType where
  arbitrary = ContentType <$> genSubType <*> genSubType <*> genParameters

genSubType :: Gen (CI.CI BS.ByteString)
genSubType = CI.mk <$> BS8.pack <$> listOf1 (elements ['a'..'z'])

genParameters :: Gen Parameters
genParameters = return (Parameters []) -- TODO: maybe test this properly some day

testsContentType :: TestTree
testsContentType = testGroup "ContentType"
  [ testReparseRoundTrip "contentTypeParser" contentTypeParser
  ]

-- * AttachmentFile

instance Arbitrary AttachmentFile where
  arbitrary = attachmentFile <$> genAttachmentFilePath <*> genAttachmentFileName <*> arbitrary

genAttachmentFilePath :: Gen T.Text
genAttachmentFilePath = do
  first <- arbitrary `suchThat` (\c -> isPrint c && not (isSpace c) && c /= ';')
  s <- T.strip <$> T.pack <$> (first :) <$>
    listOf (arbitrary `suchThat` (\c -> isPrint c && c /= ';'))
  return s

genAttachmentFileName :: Gen (Maybe T.Text)
genAttachmentFileName = do
  s <- T.strip <$> T.pack <$> listOf (arbitrary `suchThat` (\c -> isPrint c && c /= ')'))
  if T.null s
    then return Nothing
    else return (Just s)

testsAttachmentFile :: TestTree
testsAttachmentFile = testGroup "AttachmentFile"
  [ testReparseRoundTrip "attachmentFileParser" attachmentFileParser
  ]
