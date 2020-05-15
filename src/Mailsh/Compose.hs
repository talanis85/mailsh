{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Mailsh.Compose
  ( composedMessageP
  , renderComposedMessage
  , renderComposedMessageRaw
  , ComposedMessage (..)
  , emptyComposedMessage
  , Attachment (..)

  , sendMessage

  , mailboxP
  , mailboxListP
  ) where

import Prelude hiding (takeWhile)

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Except
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Text.Encoding
import           Network.Mail.Mime
import           Network.Mime hiding (MimeType)

import           Mailsh.Fields
import           Mailsh.MimeType

data ComposedMessage = ComposedMessage
  { cmessageFields :: [Field]
  , cmessageAttachments :: [Attachment]
  , cmessageText :: T.Text
  }

emptyComposedMessage :: ComposedMessage
emptyComposedMessage = ComposedMessage
  { cmessageFields = []
  , cmessageAttachments = []
  , cmessageText = ""
  }

data Attachment = Attachment
  { attachmentContentType :: MimeType
  , attachmentData :: BL.ByteString
  , attachmentFilename :: T.Text
  }

renderComposedMessage :: ComposedMessage -> T.Text
renderComposedMessage msg =
  let renderedHeaders =
        formatFields False
          [ IsField fFrom
          , IsField fTo
          , IsField fCc
          , IsField fSubject
          , IsField fReplyTo
          , IsField fInReplyTo
          , IsField fReferences
          , IsField (fOptionalField "Attachment")
          ] (cmessageFields msg)
  in (renderedHeaders <> "\n" <> cmessageText msg)

composedHeadersP :: Parser [Field]
composedHeadersP = catMaybes <$> many (headerP <* char '\n')

composedBodyP :: Parser T.Text
composedBodyP = do
  s <- takeByteString
  return (decodeUtf8 s) -- TODO: decoding actually must depend on the locale

composedMessageP :: Parser ComposedMessage
composedMessageP = do
  fields <- composedHeadersP
  _ <- many space
  body <- composedBodyP
  return ComposedMessage
    { cmessageFields = fields
    , cmessageAttachments = []
    , cmessageText = body
    }

headerP :: Parser (Maybe Field)
headerP = choice $ map try
  [ Just . mkField fSubject <$> headerNameP "Subject" unstructuredP
  , Just . mkField fFrom <$> headerNameP "From" mailboxListP
  , Just . mkField fTo <$> headerNameP "To" mailboxListP
  , Just . mkField fCc <$> headerNameP "Cc" mailboxListP
  , Just . mkField fBcc <$> headerNameP "Bcc" mailboxListP
  , Just . mkField fReplyTo <$> headerNameP "Reply-To" mailboxListP
  , Just . mkField fInReplyTo <$> headerNameP "In-Reply-To" msgidsP
  , Just . mkField fReferences <$> headerNameP "References" msgidsP
  , Just . mkField (fOptionalField "Attachment") <$> headerNameP "Attachment" unstructuredP
  , takeWhile1 (notInClass "\n") *> pure Nothing
  ]

tok :: Parser a -> Parser a
tok p = try (p <* takeWhile (inClass "\t "))

headerNameP :: String -> Parser a -> Parser a
headerNameP s p = do
  tok (string (B.pack s))
  tok (char ':')
  p

unstructuredP :: Parser T.Text
unstructuredP = T.decodeUtf8 <$> takeWhile (notInClass "\n")

msgidP :: Parser MsgID
msgidP = MsgID <$> T.decodeUtf8 <$> tok (char '<' *> takeWhile1 (notInClass (">")) <* char '>')

msgidsP :: Parser [MsgID]
msgidsP = tok msgidP `sepBy` tok (char ',')

mailboxP :: Parser Mailbox
mailboxP = try nameAddrP <|> fmap (Mailbox Nothing) addrSpecP
           <?> "mailbox"

nameAddrP :: Parser Mailbox
nameAddrP = Mailbox <$> (Just <$> displayNameP) <*> angleAddrP

maybeOption p = (Just <$> p) <|> pure Nothing

mailboxListP :: Parser [Mailbox]
mailboxListP = mailboxP `sepBy` tok (char ',')

wordP :: String -> Parser T.Text
wordP except = T.decodeUtf8 <$> tok (takeWhile1 (notInClass (" \n'" ++ except)))

displayNameP :: Parser T.Text
displayNameP = tok quotedWordP <|> (T.unwords <$> many1 (wordP "<,"))

quotedWordP :: Parser T.Text
quotedWordP = do
  char '"'
  r <- takeWhile1 (notInClass "\"")
  char '"'
  return (T.decodeUtf8 r)

angleAddrP :: Parser T.Text
angleAddrP = try (do _ <- tok (char '<')
                     r <- tok addrSpecP
                     _ <- tok (char '>')
                     return r)
                  <?> "angle address"

addrSpecP :: Parser T.Text
addrSpecP = T.decodeUtf8 <$> takeWhile1 (notInClass ">\n ,")

sendMessage :: (MonadIO m, MonadError String m) => ComposedMessage -> m ()
sendMessage cmsg = do
  msg <- generateMessage (cmessageFields cmsg) (cmessageAttachments cmsg) (cmessageText cmsg)
  liftIO $ renderSendMailCustom sendmailPath ["-t"] msg

renderComposedMessageRaw :: (MonadIO m, MonadError String m) => ComposedMessage -> m BL.ByteString
renderComposedMessageRaw cmsg = do
  msg <- generateMessage (cmessageFields cmsg) (cmessageAttachments cmsg) (cmessageText cmsg)
  liftIO $ renderMail' msg

sendmailPath :: FilePath
sendmailPath = "sendmail"

maybeError :: (MonadError String m) => String -> Maybe a -> m a
maybeError s m = case m of
                   Nothing -> throwError s
                   Just v  -> return v

generateMessage :: (MonadIO m, MonadError String m) => [Field] -> [Attachment] -> T.Text -> m Mail
generateMessage fields attachments body = do
  from <- maybeError "Missing header: From" $
    listToMaybe $ concat $ lookupField fFrom fields
  let mail = Mail
        { mailFrom    = convertMailbox from
        , mailTo      = map convertMailbox $ concat $ lookupField fTo fields
        , mailCc      = map convertMailbox $ concat $ lookupField fCc fields
        , mailBcc     = map convertMailbox $ concat $ lookupField fBcc fields
        , mailHeaders = additionalHeaders fields
            [ IsField fSubject
            , IsField fReplyTo
            , IsField fInReplyTo
            , IsField fReferences
            , IsField fDate
            ]
        , mailParts   = [mainPart body]
        }
  addedAttachments <- mapM parseAttachment (lookupField (fOptionalField "Attachment") fields)
  mail' <- liftIO $ addAttachments addedAttachments mail
  return $ foldr addAttachment' mail' attachments
  where
    addAttachment' a = addAttachmentBS (formatMimeType (attachmentContentType a)) (attachmentFilename a) (attachmentData a)

additionalHeader :: [Field] -> IsField -> [(B.ByteString, T.Text)]
additionalHeader headers (IsField f) =
  map (mkStringHeader (fieldName f) . (fieldRender f)) (lookupField f headers)
    where
      mkStringHeader name s = (T.encodeUtf8 name, s)

additionalHeaders :: [Field] -> [IsField] -> Headers
additionalHeaders headers fields = mconcat $ map (additionalHeader headers) fields

convertMailbox :: Mailbox -> Address
convertMailbox na = Address
  { addressName = mailboxName na
  , addressEmail = mailboxAddr na
  }

parseAttachment :: (MonadError String m) => T.Text -> m (T.Text, FilePath)
parseAttachment s =
  let (filename, ct) = T.break (== ';') s
      ct' = if T.null ct then ct else T.tail ct
      ct'' = if T.null ct' then T.decodeUtf8 (defaultMimeLookup filename)
                         else ct'
  in return (ct'', T.unpack filename)

mainPart :: T.Text -> Alternatives
mainPart bodyContent = return Part
  { partType = formatMimeType (mimeTextPlain "utf8")
  , partEncoding = if isPGPMessage bodyContent then None else QuotedPrintableText
  , partDisposition = DefaultDisposition
  , partHeaders = []
  , partContent = PartContent (BL.fromStrict $ T.encodeUtf8 bodyContent)
  }

isPGPMessage :: T.Text -> Bool
isPGPMessage content = T.pack "---BEGIN PGP MESSAGE---" `T.isInfixOf` content
