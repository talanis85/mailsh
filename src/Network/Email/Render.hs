{-# LANGUAGE FlexibleContexts #-}
module Network.Email.Render
  ( renderMessage
  , renderMessageS
  , sendMessage
  , generateMessage
  , addAttachmentBS
  ) where

import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Maybe
import Network.Email.Types
import Network.Mail.Mime
import Network.Mime

sendmailPath :: FilePath
sendmailPath = "sendmail"

renderMessage :: (MonadIO m, MonadError String m) => [Field] -> T.Text -> m B.ByteString
renderMessage fields body = do
  m <- generateMessage fields body
  liftIO $ renderMail' m

renderMessageS :: (MonadIO m, MonadError String m) => [Field] -> T.Text -> m String
renderMessageS fields body = B.unpack <$> renderMessage fields body

sendMessage :: Mail -> IO ()
sendMessage mail = renderSendMailCustom sendmailPath ["-t"] mail

maybeError :: (MonadError String m) => String -> Maybe a -> m a
maybeError s m = case m of
                   Nothing -> throwError s
                   Just v  -> return v

generateMessage :: (MonadIO m, MonadError String m) => [Field] -> T.Text -> m Mail
generateMessage fields body = do
  from <- maybeError "Missing header: From" $
    listToMaybe $ concat $ lookupField fFrom fields
  let mainMail = Mail
        { mailFrom    = convertAddr from
        , mailTo      = map convertAddr $ concat $ lookupField fTo fields
        , mailCc      = map convertAddr $ concat $ lookupField fCc fields
        , mailBcc     = map convertAddr $ concat $ lookupField fBcc fields
        , mailHeaders = additionalHeaders fields
            [ IsField fSubject
            , IsField fReplyTo
            , IsField fInReplyTo
            , IsField fReferences
            ]
        , mailParts   = [mainPart body]
        }
  attachments <- mapM parseAttachment (lookupOptionalField "Attachment" fields)
  liftIO $ addAttachments attachments mainMail

additionalHeader :: [Field] -> IsField -> [(BS.ByteString, T.Text)]
additionalHeader headers (IsField f) =
  map (mkStringHeader (fieldName f) . showFieldValue) (lookupField f headers)
    where
      mkStringHeader name s = (BS.pack name, T.pack s)

additionalHeaders :: [Field] -> [IsField] -> Headers
additionalHeaders headers fields = mconcat $ map (additionalHeader headers) fields

convertAddr :: NameAddr -> Address
convertAddr na = Address
  { addressName = T.pack <$> nameAddr_name na
  , addressEmail = T.pack (nameAddr_addr na)
  }

parseAttachment :: (MonadError String m) => String -> m (T.Text, FilePath)
parseAttachment s =
  let (filename, ct) = break (== ';') s
      ct' = if null ct then ct else tail ct
      ct'' = if null ct' then BS.unpack (defaultMimeLookup (T.pack filename))
                         else ct'
  in return (T.pack ct'', filename)

mainPart :: T.Text -> Alternatives
mainPart bodyContent = return Part
  { partType = T.pack (formatMimeType (mimeTextPlain "utf8"))
  , partEncoding = QuotedPrintableText
  , partFilename = Nothing
  , partHeaders = []
  , partContent = B.fromStrict $ T.encodeUtf8 bodyContent
  }
