{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Mailsh.Compose
  ( ReplyStrategy (..)
  , sendMessage
  , applyReplyStrategy
  ) where

{-
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

import           Mailsh.Message
-}

import Control.Lens
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import Network.Mail.Mime

import Mailsh.Message

data ReplyStrategy = SingleReply | GroupReply
  deriving (Show)

{-
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
-}

sendmailPath :: FilePath
sendmailPath = "sendmail"

sendMessage :: MIMEMessage -> IO ()
sendMessage msg = sendmailCustom sendmailPath ["-t"] (renderMessage msg)

applyReplyStrategy :: (HasHeaders a, HasHeaders b) => ReplyStrategy -> b -> a -> a
applyReplyStrategy strat msg =
  let from'   = case msg ^. headerReplyTo defaultCharsets of
                  [] -> msg ^. headerFrom defaultCharsets
                  xs -> xs
      to      = case strat of
                  GroupReply -> from' ++ msg ^. headerTo defaultCharsets
                  SingleReply -> from'
      cc      = case strat of
                  GroupReply -> msg ^. headerCC defaultCharsets
                  SingleReply -> []
      subject' = fromMaybe "(no subject)" (msg ^. headerSubject defaultCharsets)
      subject  = if "Re:" `T.isPrefixOf` subject'
                    then subject'
                    else "Re: " <> subject'
  in
      (headerTo defaultCharsets .~ to)
    . (headerCC defaultCharsets .~ cc)
    . (headerInReplyTo .~ (maybeToList $ msg ^. headerMessageID))
    . (headerSubject defaultCharsets .~ Just subject)
    . (headerReferences .~ ((maybeToList $ msg ^. headerMessageID) ++ (msg ^. headerReferences)))

{-
renderComposedMessageRaw :: (MonadIO m, MonadError String m) => ComposedMessage -> m BL.ByteString
renderComposedMessageRaw cmsg = do
  msg <- generateMessage (cmessageFields cmsg) (cmessageAttachments cmsg) (cmessageText cmsg)
  liftIO $ renderMail' msg

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
-}
