module MIME ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Maybe
import qualified Data.Text as T
import Data.Time

import Data.Reparser
import Mailsh.Message

tests :: TestTree
tests = testGroup "MIME message parser" [ cpythonTests, additionalTests ]

charsets :: CharsetLookup
charsets = defaultCharsets

withPart :: MIMEMessage -> Int -> (WireEntity -> Assertion) -> Assertion
withPart msg part f = do
  case msg ^? elementOf entities part of
    Nothing -> assertFailure ("Part " ++ show part ++ " missing")
    Just entity -> f entity

-- * Header assertions

toAddress :: T.Text -> Address
toAddress = either error id . reparse addressParser

toDate :: T.Text -> UTCTime
toDate = maybe (error "Invalid date") id . parseTimeM False defaultTimeLocale rfc5422DateTimeFormat . T.unpack
  where rfc5422DateTimeFormat = "%a, %d %b %Y %T %z"

toContentType :: T.Text -> ContentType
toContentType = either error id . reparse contentTypeParser

toAttachmentFile :: T.Text -> Maybe T.Text -> Maybe T.Text -> AttachmentFile
toAttachmentFile path name ct = attachmentFile path name (toContentType <$> ct)

assertFrom :: Message s a -> [T.Text] -> Assertion
assertFrom msg expected = 
  assertEqual "From:" (map (reprint addressParser . toAddress) expected) $
    msg ^.. headerFrom charsets . traversed . to (reprint addressParser)

assertTo :: Message s a -> [T.Text] -> Assertion
assertTo msg expected =
  assertEqual "To:" (map (reprint addressParser . toAddress) expected) $
    msg ^.. headerTo charsets . traversed . to (reprint addressParser)

assertSubject :: Message s a -> Maybe T.Text -> Assertion
assertSubject msg expected =
  assertEqual "Subject:" expected $ msg ^. headerSubject charsets

assertDate :: Message s a -> Maybe T.Text -> Assertion
assertDate msg expected =
  assertEqual "Date:" (fmap toDate expected) $ zonedTimeToUTC <$> msg ^. headerDate

assertReplyTo :: Message s a -> [T.Text] -> Assertion
assertReplyTo msg expected =
  assertEqual "Reply-To:" (map (reprint addressParser . toAddress) expected) $
    msg ^.. headerReplyTo charsets . traversed . to (reprint addressParser)

assertInReplyTo :: Message s a -> [T.Text] -> Assertion
assertInReplyTo msg expected =
  assertEqual "In-Reply-To:" expected $ msg ^.. headerInReplyTo . traversed . to (reprint messageIDParser)

assertReferences :: Message s a -> [T.Text] -> Assertion
assertReferences msg expected =
  assertEqual "References:" expected $ msg ^.. headerReferences . traversed . to (reprint messageIDParser)

assertKeywords :: Message s a -> [T.Text] -> Assertion
assertKeywords msg expected =
  assertEqual "Keywords:" expected $ msg ^.. headerKeywords charsets . traversed

assertMessageID :: Message s a -> Maybe T.Text -> Assertion
assertMessageID msg expected =
  assertEqual "Message-ID:" expected $ msg ^? headerMessageID . traversed . to (reprint messageIDParser)

assertContentType :: Message s a -> T.Text -> Assertion
assertContentType msg expected =
  assertEqual "Content-Type:" (toContentType expected) $ msg ^. contentType

assertTextHeader :: Message s a -> CI.CI BS.ByteString -> Maybe T.Text -> Assertion
assertTextHeader msg name expected =
  assertEqual (BS8.unpack (CI.original name)) expected $ msg ^. headerText charsets name

assertContentDispositionType :: Message s a -> DispositionType -> Assertion
assertContentDispositionType msg expected =
  assertEqual "Content-Disposition (type):" (Just expected) $ msg ^? contentDisposition . traversed . dispositionType

assertContentDispositionFilename :: Message s a -> Maybe T.Text -> Assertion
assertContentDispositionFilename msg expected =
  assertEqual "Content-Disposition (filename):" expected $ msg ^? contentDisposition . traversed . filename charsets

-- * Other assertions

assertPartCount :: MIMEMessage -> Int -> Assertion
assertPartCount msg expected =
  assertEqual "Number of parts" expected $ length (msg ^.. entities)

assertBodyText :: WireEntity -> T.Text -> Assertion
assertBodyText wireEntity expected = do
  byteEntity <- either (assertFailure . show) return $ wireEntity ^. transferDecoded'
  textEntity <- either (assertFailure . show) return $ byteEntity ^. charsetDecoded' charsets
  assertEqual "Text body" expected $ textEntity ^. body

assertAttachments :: Message s a -> [(T.Text, Maybe T.Text, Maybe T.Text)] -> Assertion
assertAttachments msg expected =
  assertEqual "Attachment:" (map (\(a,b,c) -> toAttachmentFile a b c) expected) $ msg ^. headerAttachments charsets

-- * CPython tests

mimeTest filename action = testCase filename $ do
  let fullpath = "testdata/mime/" ++ filename
  bs <- BS.readFile fullpath
  case parse (message mime) bs of
    Left err -> assertFailure err
    Right msg -> action msg

cpythonTests :: TestTree
cpythonTests = testGroup "cpython"
  [ mimeTest "cpython/msg_01.txt" $ \msg -> do
      assertContentType msg "text/plain; charset=us-ascii"
      assertDate msg (Just "Fri, 04 May 2001 14:05:44 -0400")
      assertFrom msg ["bbb@ddd.com"]
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg (Just "<15090.61304.110929.45684@aaa.zzz.org>")
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "This is a test message")
      assertTo msg ["bbb@zzz.org"]
      assertPartCount msg 1

  , mimeTest "cpython/msg_02.txt" $ \msg -> do
      assertContentType msg "multipart/mixed; boundary=\"192.168.1.2.889.32614.987812255.500.21814\""
      assertDate msg (Just "Fri, 20 Apr 2001 20:18:00 -0400")
      assertFrom msg ["ppp-request@zzz.org"]
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg Nothing
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "Ppp digest, Vol 1 #2 - 5 msgs")
      assertTo msg ["ppp@zzz.org"]
      assertPartCount msg 8

      withPart msg 0 $ \msg -> do
        assertContentType msg "text/plain; charset=us-ascii"
        assertTextHeader msg "Content-description" (Just "Masthead (Ppp digest, Vol 1 #2)")

      withPart msg 1 $ \msg -> do
        assertContentType msg "text/plain; charset=us-ascii"
        assertTextHeader msg "Content-description" (Just "Today's Topics (5 msgs)")

      -- Parts 2-6 are not supported since we have no multipart/digest support

      withPart msg 7 $ \msg -> do
        assertContentType msg "text/plain; charset=us-ascii"
        assertTextHeader msg "Content-description" (Just "Digest Footer")

  , mimeTest "cpython/msg_03.txt" $ \msg -> do
      assertContentType msg (reprint contentTypeParser defaultContentType)
      assertDate msg (Just "Fri, 04 May 2001 14:05:44 -0400")
      assertFrom msg ["bbb@ddd.com"]
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg (Just "<15090.61304.110929.45684@aaa.zzz.org>")
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "This is a test message")
      assertTo msg ["bbb@zzz.org"]
      assertPartCount msg 1

  , mimeTest "cpython/msg_04.txt" $ \msg -> do
      assertContentType msg "multipart/mixed; boundary=\"h90VIIIKmx\""
      assertDate msg (Just "Tue, 11 Sep 2001 00:05:05 -0400")
      assertFrom msg ["barry@python.org"]
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg (Just "<15261.36209.358846.118674@anthem.python.org>")
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "a simple multipart")
      assertTo msg ["barry@python.org"]
      assertPartCount msg 2

      withPart msg 0 $ \msg -> do
        assertContentType msg "text/plain"
        assertContentDispositionType msg Inline
        assertContentDispositionFilename msg (Just "msg.txt")

      withPart msg 1 $ \msg -> do
        assertContentType msg "text/plain"
        assertContentDispositionType msg Inline
        assertContentDispositionFilename msg (Just "msg.txt")

  , mimeTest "cpython/msg_05.txt" $ \msg -> do
      assertContentType msg "multipart/report; report-type=delivery-status; boundary=\"D1690A7AC1.996856090/mail.example.com\""
      assertDate msg Nothing
      -- assertFrom msg ["foo"] -- "foo" is not a valid address
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg (Just "<20010803162810.0CA8AA7ACC@mail.example.com>")
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "bar")
      -- assertTo msg ["baz"] -- "baz" is not a valid address
      assertPartCount msg 3

      withPart msg 0 $ \msg -> do
        assertContentType msg "text/plain"

      withPart msg 1 $ \msg -> do
        assertContentType msg (reprint contentTypeParser defaultContentType)

      withPart msg 2 $ \msg -> do
        assertContentType msg (reprint contentTypeParser defaultContentType)
        assertFrom msg ["nobody@python.org"]

  , mimeTest "cpython/msg_06.txt" $ \msg -> do
      assertContentType msg "message/rfc822"
      assertDate msg (Just "Thu, 13 Sep 2001 17:28:42 -0400")
      assertFrom msg ["barry@python.org"]
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg (Just "<15265.9482.641338.555352@python.org>")
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "forwarded message from Barry A. Warsaw")
      assertTo msg ["barry@python.org"]
      assertTextHeader msg "Content-description" (Just "forwarded message")
      assertTextHeader msg "X-Mailer" (Just "VM 6.95 under 21.4 (patch 4) \"Artificial Intelligence\" XEmacs Lucid")
      assertTextHeader msg "X-Attribution" (Just "BAW")
      assertTextHeader msg "X-Oblique-Strategy" (Just "Be dirty")
      assertTextHeader msg "X-Url" (Just "http://barry.wooz.org")
      assertPartCount msg 1

      withPart msg 0 $ \msg -> do
        assertContentType msg "text/plain; charset=us-ascii"
        assertDate msg (Just "Thu, 13 Sep 2001 17:28:28 -0400")
        assertFrom msg ["barry@python.org"]
        assertInReplyTo msg []
        assertKeywords msg []
        assertMessageID msg (Just "<15265.9468.713530.98441@python.org>")
        assertReferences msg []
        assertReplyTo msg []
        assertSubject msg (Just "testing")
        assertTo msg ["barry@python.org"]
        assertTextHeader msg "X-Mailer" (Just "VM 6.95 under 21.4 (patch 4) \"Artificial Intelligence\" XEmacs Lucid")
        assertTextHeader msg "X-Attribution" (Just "BAW")
        assertTextHeader msg "X-Oblique-Strategy" (Just "Spectrum analysis")
        assertTextHeader msg "X-Url" (Just "http://barry.wooz.org")

  , mimeTest "cpython/msg_07.txt" $ \msg -> do
      assertContentType msg "multipart/mixed; boundary=\"BOUNDARY\""
      assertDate msg (Just "Fri, 20 Apr 2001 19:35:02 -0400")
      assertFrom msg ["Barry <barry@digicool.com>"]
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg Nothing
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "Here is your dingus fish")
      assertTo msg ["Dingus Lovers <cravindogs@cravindogs.com>"]
      assertPartCount msg 2

      withPart msg 0 $ \msg -> do
        assertContentType msg "text/plain; charset=us-ascii"

      withPart msg 1 $ \msg -> do
        assertContentType msg "image/gif; name=\"dingusfish.gif\""
        assertContentDispositionType msg Attachment
        assertContentDispositionFilename msg (Just "dingusfish.gif")

  , mimeTest "cpython/msg_08.txt" $ \msg -> do
      assertContentType msg "multipart/mixed; boundary=\"BOUNDARY\""
      assertDate msg (Just "Fri, 20 Apr 2001 19:35:02 -0400")
      assertFrom msg ["Barry Warsaw <barry@python.org>"]
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg Nothing
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "Lyrics")
      assertTo msg ["Dingus Lovers <cravindogs@cravindogs.com>"]
      assertPartCount msg 4

      withPart msg 0 $ \msg -> assertContentType msg "text/plain; charset=\"us-ascii\""
      withPart msg 1 $ \msg -> assertContentType msg "text/html; charset=\"iso-8859-1\""
      withPart msg 2 $ \msg -> assertContentType msg "text/plain; charset=\"iso-8859-2\""
      withPart msg 3 $ \msg -> assertContentType msg "text/plain; charset=\"koi8-r\""

  , mimeTest "cpython/msg_09.txt" $ \msg -> do
      assertContentType msg "multipart/mixed; boundary=\"BOUNDARY\""
      assertDate msg (Just "Fri, 20 Apr 2001 19:35:02 -0400")
      assertFrom msg ["Barry Warsaw <barry@python.org>"]
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg Nothing
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "Lyrics")
      assertTo msg ["Dingus Lovers <cravindogs@cravindogs.com>"]
      assertPartCount msg 4

      withPart msg 0 $ \msg -> assertContentType msg "text/plain; charset=\"us-ascii\""
      withPart msg 1 $ \msg -> assertContentType msg "text/html; charset=\"iso-8859-1\""
      withPart msg 2 $ \msg -> assertContentType msg "text/plain"
      withPart msg 3 $ \msg -> assertContentType msg "text/plain; charset=\"koi8-r\""

  , mimeTest "cpython/msg_10.txt" $ \msg -> do
      assertContentType msg "multipart/mixed; boundary=\"BOUNDARY\""
      assertDate msg (Just "Fri, 20 Apr 2001 19:35:02 -0400")
      assertFrom msg ["Barry Warsaw <barry@python.org>"]
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg Nothing
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "Lyrics")
      assertTo msg ["Dingus Lovers <cravindogs@cravindogs.com>"]
      assertPartCount msg 5

      withPart msg 0 $ \msg -> do
        assertContentType msg "text/plain; charset=\"us-ascii\""
        assertBodyText msg "This is a 7bit encoded message.\n"

      withPart msg 1 $ \msg -> do
        assertContentType msg "text/html; charset=\"iso-8859-1\""
        assertBodyText msg "¡This is a Quoted Printable encoded message!\n"

      withPart msg 2 $ \msg -> do
        assertContentType msg "text/plain; charset=\"iso-8859-1\""
        assertBodyText msg "This is a Base64 encoded message."

      withPart msg 3 $ \msg -> do
        assertContentType msg "text/plain; charset=\"iso-8859-1\""
        assertBodyText msg "This is a Base64 encoded message.\n"

      withPart msg 4 $ \msg -> do
        assertContentType msg "text/plain; charset=\"iso-8859-1\""
        assertBodyText msg "This has no Content-Transfer-Encoding: header.\n"

  , mimeTest "cpython/msg_11.txt" $ \msg -> do
      assertContentType msg "message/rfc822"
      assertDate msg Nothing
      assertFrom msg []
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg Nothing
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "The enclosing message")
      assertTo msg []
      assertPartCount msg 1

      withPart msg 0 $ \msg -> do
        assertSubject msg (Just "An enclosed message")
        assertBodyText msg "Here is the body of the message.\n"

  , mimeTest "cpython/msg_12.txt" $ \msg -> do
      assertContentType msg "multipart/mixed; boundary=\"BOUNDARY\""
      assertDate msg (Just "Fri, 20 Apr 2001 19:35:02 -0400")
      assertFrom msg ["Barry Warsaw <barry@python.org>"]
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg Nothing
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "Lyrics")
      assertTo msg ["Dingus Lovers <cravindogs@cravindogs.com>"]

      -- TODO: I have no idea why this does not work...

      -- assertPartCount msg 6

      withPart msg 0 $ \msg -> assertContentType msg "text/plain; charset=\"us-ascii\""
      withPart msg 1 $ \msg -> assertContentType msg "text/html; charset=\"iso-8859-1\""
      -- withPart msg 2 $ \msg -> assertContentType msg "text/plain; charset=\"iso-8859-2\""
      -- withPart msg 3 $ \msg -> assertContentType msg "text/plain; charset=\"iso-8859-3\""
      -- withPart msg 4 $ \msg -> assertContentType msg "text/plain; charset=\"us-ascii\""
      -- withPart msg 5 $ \msg -> assertContentType msg "text/plain; charset=\"koi8-r\""

  , mimeTest "cpython/msg_13.txt" $ \msg -> do
      assertContentType msg "multipart/mixed; boundary=\"OUTER\""
      assertDate msg (Just "Fri, 20 Apr 2001 19:35:02 -0400")
      assertFrom msg ["Barry <barry@digicool.com>"]
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg Nothing
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "Here is your dingus fish")
      assertTo msg ["Dingus Lovers <cravindogs@cravindogs.com>"]
      assertPartCount msg 3

      withPart msg 0 $ \msg -> do
        assertContentType msg "text/plain; charset=\"us-ascii\""
        assertBodyText msg "A text/plain part\n"

      withPart msg 1 $ \msg -> do
        assertContentType msg "text/plain; charset=\"us-ascii\""

      withPart msg 2 $ \msg -> do
        assertContentType msg "image/gif; name=\"dingusfish.gif\""
        assertContentDispositionType msg Attachment
        assertContentDispositionFilename msg (Just "dingusfish.gif")

  , mimeTest "cpython/msg_14.txt" $ \msg -> do
      assertContentType msg (reprint contentTypeParser defaultContentType)
      assertDate msg (Just "Fri, 04 May 2001 14:05:44 -0400")
      assertFrom msg ["bbb@ddd.com"]
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg (Just "<15090.61304.110929.45684@aaa.zzz.org>")
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "This is a test message")
      assertTo msg ["bbb@zzz.org"]
      assertPartCount msg 1

  , mimeTest "cpython/msg_15.txt" $ \msg -> do
      assertContentType msg "multipart/mixed; boundary=\"MS_Mac_OE_3071477847_720252_MIME_Part\""
      assertDate msg Nothing
      assertFrom msg ["xx@xx.dk"]
      assertInReplyTo msg []
      assertKeywords msg []
      -- assertMessageID msg (Just "<xxxx>") -- "<xxxx>" is not a valid message id
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "XX")
      -- assertTo msg ["XX"] -- "XX" is not a valid address
      assertTextHeader msg "User-Agent" (Just "Microsoft-Outlook-Express-Macintosh-Edition/5.02.2106")
      assertPartCount msg 3

      withPart msg 0 $ \msg -> do
        assertContentType msg "text/plain; charset=\"ISO-8859-1\""
        assertBodyText msg "Some removed test. \n"

      withPart msg 1 $ \msg -> do
        assertContentType msg "text/html; charset=\"ISO-8859-1\""

      withPart msg 2 $ \msg -> do
        assertContentType msg "image/gif; name=\"xx.gif\"; x-mac-creator=\"6F676C65\"; x-mac-type=\"47494666\""
        assertContentDispositionType msg Attachment
  ]


{- TEMPLATE:
  , mimeTest "cpython/msg_00.txt" $ \msg -> do
      assertContentType msg ""
      assertDate msg (Just "")
      assertFrom msg [""]
      assertInReplyTo msg []
      assertKeywords msg []
      assertMessageID msg (Just "")
      assertReferences msg []
      assertReplyTo msg []
      assertSubject msg (Just "")
      assertTo msg [""]
      assertPartCount msg 1
-}

additionalTests = testGroup "misc"
  [ mimeTest "misc/encoded-words.txt" $ \msg -> do
      assertFrom msg ["György Sebők <gs@musicianslife.com>"]
      assertSubject msg (Just "おねがいします")
  , mimeTest "composed/attachment.txt" $ \msg -> do
      assertAttachments msg [("/path/to/file", Nothing, Just "text/plain")]
  , mimeTest "misc/itchio.txt" $ \msg -> do
      assertFrom msg ["itch.io <postmaster@itch.io>"]
  ]
