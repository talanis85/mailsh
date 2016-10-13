{-# LANGUAGE FlexibleContexts #-}
module Network.Email.Render
  ( renderMessage
  , renderMessageS
  , sendMessage
  ) where

import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Maybe
import Network.Email.Types
import Network.Mail.Mime

renderMessage :: (MonadIO m, MonadError String m) => [Field] -> Body -> m B.ByteString
renderMessage fields body = do
  m <- genMail fields body
  liftIO $ renderMail' m

renderMessageS :: (MonadIO m, MonadError String m) => [Field] -> Body -> m String
renderMessageS fields body = B.unpack <$> renderMessage fields body

sendMessage :: (MonadIO m, MonadError String m) => [Field] -> Body -> m ()
sendMessage fields body = do
  m <- genMail fields body
  liftIO $ renderSendMail m

maybeError :: (MonadError String m) => String -> Maybe a -> m a
maybeError s m = case m of
                   Nothing -> throwError s
                   Just v  -> return v

genMail :: (MonadIO m, MonadError String m) => [Field] -> Body -> m Mail
genMail fields body = do
  from <- maybeError "Missing header: From" $
    listToMaybe $ concat $ lookupField fFrom fields
  (mainContentType, bodyContent) <- maybeError "Multipart message" $
    listToMaybe $ bodies body
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
        , mailParts   = [mainPart mainContentType bodyContent]
        }
  attachments <- mapM parseAttachment (lookupOptionalField "Attachment" fields)
  liftIO $ addAttachments attachments mainMail

additionalHeader :: [Field] -> IsField -> [(BS.ByteString, T.Text)]
additionalHeader headers (IsField f) =
  map (mkStringHeader (fieldName f)) (map showFieldValue (lookupField f headers))
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
  in return (T.pack (tail ct), filename)

mainPart :: MimeType -> String -> Alternatives
mainPart mainContentType bodyContent = return Part
  { partType = T.pack (show mainContentType)
  , partEncoding = QuotedPrintableText
  , partFilename = Nothing
  , partHeaders = []
  , partContent = B.pack bodyContent
  }
