module Commands
  ( cmdRead
  , cmdNext
  , cmdCat
  , cmdView
  , cmdHeaders
  -- , cmdVisual
  , cmdSave
  , cmdCompose
  , cmdReply
  , cmdForward
  , cmdLs
  , cmdLsn
  , cmdBrowse
  , cmdTrash
  , cmdRecover
  , cmdPurge
  , cmdUnread
  , cmdFlag
  , cmdUnflag
  , cmdFilename
  , cmdOutline
  , cmdTar
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.Logger

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8

import Data.Maybe (catMaybes, fromMaybe)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Time

import Numeric.Natural

import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO

import Text.Printf

import Data.Reparser
import Mailsh.Compose
import Mailsh.Maildir
import Mailsh.Message
import Mailsh.MimeRender
import Mailsh.Store
import Mailsh.Types

import Browse
import Format
import Render
import Util

cmdRead :: Either MessageRef PartRef -> Renderer -> Bool -> StoreM ()
cmdRead (Left mref) renderer quiet = do
  msg <- getMessage mref
  when (not quiet) $ printMessage renderer msg
  mapM_ (liftMaildir . setFlag 'S') (msg ^. body . storedMid)
  setCurrentMessageRef mref
cmdRead (Right pref@(PartRef mref partnum)) renderer quiet = do
  part <- getPart pref
  msg <- getMessage mref
  when (not quiet) $ printMessage renderer (msg `withMainPart` part)

cmdNext :: Renderer -> StoreM ()
cmdNext renderer = do
  filter <- makeStoreFilter FilterUnseen
  result <- queryStore (filterBy filter NoLimit)

  case resultRows result of
    [] -> printStatusMessage "No new messages."
    (msg:_) -> do
      printMessage renderer msg
      mapM_ (liftMaildir . setFlag 'S') (msg ^. body . storedMid)
      mapM_ (setCurrentMessage . storeNumberToNatural) (msg ^. body . storedNumber)

cmdCat :: Either MessageRef PartRef -> StoreM ()
cmdCat (Left mref) = do
  msg <- getMessage mref
  src <- liftIO $ msg ^. body . storedSource
  liftIO $ BS8.putStrLn src
  setCurrentMessageRef mref
cmdCat (Right pref) = do
  part <- getPart pref
  liftIO $ BS8.putStrLn $ part ^. body

cmdView :: Either MessageRef PartRef -> StoreM ()
cmdView (Left mref) = do
  throwError "not implemented"
  setCurrentMessageRef mref
cmdView (Right pref) = do
  part <- getPart pref
  case part ^? contentDisposition . traversed . filename defaultCharsets of
    Nothing -> throwError "Part has no file name"
    Just filename -> liftIO $ runXdgOpen filename (part ^. body)

cmdHeaders :: Either MessageRef PartRef -> StoreM ()
cmdHeaders (Left mref) = do
  msg <- getMessage mref
  msg' <- getMIMEMessage msg
  printHeaders msg'
cmdHeaders (Right pref) = do
  part <- getPart pref
  printHeaders part

{-
cmdVisual :: MessageRef -> StoreM ()
cmdVisual mref = do
  bs <- getRawMessage mref
  Message headers body <- throwEither "Could not parse Message" $ pure $ parseOnly messageP (BL.toStrict bs)
  let htmlBody = fromMaybe "EMPTY" (snd <$> firstHtmlPart body)
  liftIO $ do
    tempdir <- getTemporaryDirectory
    (tempf, temph) <- openTempFile tempdir "mailshvis.html"
    T.hPutStr temph htmlBody
    hClose temph
    let cmd = printf "xdg-open \"file://%s\"" tempf
    (_, _, _, procH) <- createProcess_ "xdg-open" (shell cmd)
    waitForProcess procH
    return ()
  where
    firstHtmlPart msg =
      let textPlain t  = mimeType t == "text" && mimeSubtype t == "plain"
          textHtml t   = mimeType t == "text" && mimeSubtype t == "html"
          firstPlain   = msg ^? collapsedAlternatives textPlain . inlineParts . textPart
          firstHtml    = msg ^? collapsedAlternatives textHtml . inlineParts . textPart
      in firstHtml <|> firstPlain
-}

joinMaybeDir :: Maybe FilePath -> FilePath -> FilePath
joinMaybeDir Nothing b = b
joinMaybeDir (Just a) b = a </> b

cmdSave :: Either MessageRef PartRef -> Maybe FilePath -> StoreM ()
cmdSave (Left mref) dir = do
  msg <- getMessage mref
  src <- liftIO $ msg ^. body . storedSource
  srcmsg <- joinEither "Error parsing message" $ return $ parse (message mime) src
  forM_ (srcmsg ^.. entities . transferDecoded') $ \e -> do
    case e of
      Left err -> throwError $ "Error: " ++ show err
      Right e' -> case e' ^? contentDisposition . traversed . filename defaultCharsets of
        Nothing -> return ()
        Just fn -> do
          let fn' = joinMaybeDir dir (T.unpack fn)
          liftIO $ BS8.writeFile fn' $ e' ^. body
          printStatusMessage $ "Written " <> T.pack fn'
cmdSave (Right pref) dir = do
  part <- getPart pref
  case part ^? contentDisposition . traversed . filename defaultCharsets of
    Nothing -> throwError "Part has no file name"
    Just fn -> do
      let fn' = joinMaybeDir dir (T.unpack fn)
      liftIO $ BS8.writeFile fn' $ part ^. body
      printStatusMessage $ "Written " <> T.pack fn'

cmdCompose :: Bool -> [T.Text] -> Maybe T.Text -> StoreM ()
cmdCompose dry attachments' mailboxes' = do
  mailboxes <- concat <$> mapM (joinEither "Error parsing recipients" . return . reparse mailboxesParser) mailboxes'
  attachments <- joinEither "Error parsing attachments" $ return $ mapM (reparse attachmentFileParser) attachments'
  signature <- liftIO getSignature
  from <- getSender

  let templateHeaders = mempty
        & (headerFrom defaultCharsets .~ [Single from])
        & (headerTo defaultCharsets .~ (Single <$> mailboxes))
        & (headerAttachments defaultCharsets .~ attachments)
      templateBody = signature
      template = Message templateHeaders templateBody

  msg' <- composeMessage template

  askAboutMessage dry msg' $ do
    msg'' <- liftIO $ composedToMime msg'
    liftIO $ sendMessage msg''
    addSentMessage msg''

cmdReply :: Bool -> ReplyStrategy -> [T.Text] -> MessageRef -> StoreM ()
cmdReply dry strat attachments' mref = do
  attachments <- joinEither "Error parsing attachments" $ return $ mapM (reparse attachmentFileParser) attachments'
  msg <- getMessage mref
  from <- getSender

  let templateHeaders = mempty
        & (applyReplyStrategy strat msg)
        & (headerFrom defaultCharsets .~ [Single from])
        & (headerAttachments defaultCharsets .~ attachments)
      templateBody = quotedMessage msg
      template = Message templateHeaders templateBody

  msg' <- composeMessage template

  askAboutMessage dry msg' $ do
    msg'' <- liftIO $ composedToMime msg'
    liftIO $ sendMessage msg''
    addSentMessage msg''
    mapM_ (liftMaildir . setFlag 'R') (msg ^. body . storedMid)

cmdForward :: Bool -> Maybe T.Text -> MessageRef -> StoreM ()
cmdForward dry mailboxes' mref = do
  mailboxes <- concat <$> mapM (joinEither "Error parsing recipients" . return . reparse mailboxesParser) mailboxes'
  signature <- liftIO getSignature
  from <- getSender

  filename <- getMessageFilename mref
  msg <- getMessage mref
  let ct = ContentType "message" "rfc822" mempty
      subject = "Fw: " <> fromMaybe "(no subject)" (msg ^. headerSubject defaultCharsets)

  let templateHeaders = mempty
        & (headerFrom defaultCharsets .~ [Single from])
        & (headerTo defaultCharsets .~ (Single <$> mailboxes))
        & (headerAttachments defaultCharsets .~ [attachmentFile filename (Just "message.eml") (Just ct)])
        & (headerSubject defaultCharsets .~ Just subject)
      templateBody = signature
      template = Message templateHeaders templateBody

  msg' <- composeMessage template

  askAboutMessage dry msg' $ do
    msg'' <- liftIO $ composedToMime msg'
    liftIO $ sendMessage msg''
    addSentMessage msg''

cmdLs :: Limit -> FilterExp -> Maybe RichFormat -> StoreM ()
cmdLs limit' filterExp argumentFormat = do
  maildirFormat <- getMaildirFormat
  let format = fromMaybe defaultMessageFormat $ argumentFormat <|> maildirFormat

  limit <- case limit' of
    Limit x -> return (Limit x)
    NoLimit -> Limit <$> subtract 2 <$> fromIntegral <$> liftIO terminalHeight

  filter <- makeStoreFilter filterExp

  result <- queryStore (filterBy filter limit)
  printMessageLines format result

cmdLsn :: FilterExp -> StoreM ()
cmdLsn filterExp = do
  filter <- makeStoreFilter filterExp
  result <- queryStore (filterBy filter NoLimit)
  printStoreNumbers result

cmdBrowse :: Limit -> FilterExp -> Maybe RichFormat -> StoreM ()
cmdBrowse limit filterExp argumentFormat = do
  maildirFormat <- getMaildirFormat
  let format = fromMaybe defaultMessageFormat $ argumentFormat <|> maildirFormat

  filter <- makeStoreFilter filterExp
  result <- queryStore (filterBy filter limit)
  liftIO $ browseStoredMessages format (reverse $ resultRows result)

cmdTrash :: MessageNumber -> StoreM ()
cmdTrash = modifyMessage (liftMaildir . setFlag 'T') "Trashed message."

cmdRecover :: MessageNumber -> StoreM ()
cmdRecover = modifyMessage (liftMaildir . unsetFlag 'T') "Recovered message."

cmdPurge :: StoreM ()
cmdPurge = do
  n <- liftMaildir purgeMaildir
  printStatusMessage $ T.pack $ printf "Purged %d messages.\n" n

cmdUnread :: MessageNumber -> StoreM ()
cmdUnread = modifyMessage (liftMaildir . unsetFlag 'S') "Marked message as unread."

cmdFlag :: MessageNumber -> StoreM ()
cmdFlag = modifyMessage (liftMaildir . setFlag 'F') "Flagged message."

cmdUnflag :: MessageNumber -> StoreM ()
cmdUnflag = modifyMessage (liftMaildir . unsetFlag 'F') "Unflagged message."

cmdFilename :: MessageNumber -> StoreM ()
cmdFilename mn' = do
  mn <- getMessageNumber mn'
  msg <- queryStore' (lookupStoreNumber mn)
  case msg ^. body ^. storedMid of
    Nothing -> throwError "This message has no filename"
    Just mid -> do
      filename <- liftMaildir $ absoluteMaildirFile mid
      printPlain (T.pack filename)

cmdOutline :: MessageRef -> StoreM ()
cmdOutline mref = do
  msg <- getMessage mref
  printOutline msg

cmdTar :: MessageRef -> StoreM ()
cmdTar mref = do
  msg <- getMessage mref

  let atts = msg ^.. body . storedAttachments . withIndex
  let mkTarEntry (partnum, (filename', ct)) = case filename' of
        Nothing -> return Nothing
        Just filename -> case Tar.toTarPath False (T.unpack filename) of
          Left err -> do
            logWarnN $ T.pack $ printf "Invalid filename '%s': %s" filename err
            return Nothing
          Right tp -> do
            msg' <- getMIMEMessage msg
            part <- getPartByIndex partnum msg'
            return $ Just $ Tar.fileEntry tp (BL.fromStrict (part ^. body))

  tarentries <- catMaybes <$> mapM mkTarEntry atts
  liftIO $ BL.putStr (Tar.write tarentries)

---

modifyMessage :: (MID -> StoreM ()) -> T.Text -> MessageNumber -> StoreM ()
modifyMessage f notice mn' = do
  mn <- getMessageNumber mn'
  msg <- queryStore' (lookupStoreNumber mn)
  mapM_ f (msg ^. body . storedMid)
  printStatusMessage notice
  printMessageLine msg

getSignature :: IO T.Text
getSignature = do
  signaturePath <- (</> ".config/mailsh/signature") <$> getHomeDirectory
  signatureExists <- doesFileExist signaturePath
  if signatureExists then T.readFile signaturePath else return ""

getSender :: StoreM Mailbox
getSender = do
  value <- joinMaybe "Need to set MAILFROM environment variable" $ liftIO $ lookupEnv "MAILFROM"
  mailbox <- joinEither "MAILFROM parse error" $ return $ reparse mailboxParser (T.pack value)
  return mailbox

askAboutMessage :: Bool -> TextEntity -> StoreM () -> StoreM ()
askAboutMessage True msg _ = do
  msg' <- liftIO $ composedToMime msg
  liftIO $ BL8.putStrLn (renderMessage msg')
askAboutMessage False msg action = do
  sendIt <- liftIO $ askYesNo "Send this message?"
  if sendIt
    then do
      action
      printStatusMessage "Ok, mail sent."
    else do
      printStatusMessage "Ok, mail discarded."

askYesNo :: String -> IO Bool
askYesNo prompt = do
  putStr (prompt ++ " (y/n) ")
  hFlush stdout
  r <- getChar
  if r == 'y' then return True else return False

dos2unix :: BL.ByteString -> BL.ByteString
dos2unix = BL8.filter ((/=) '\r')

composeMessage :: TextEntity -> StoreM TextEntity
composeMessage template = do
  bs <- liftIO $ composeWithEditor (dos2unix $ renderMessage template)
  msg <- joinEither "Error parsing template message" $ return $ parse (messageUtf8 composed) bs
  currentTime <- liftIO $ getZonedTime

  let messageWithDate = msg & headerDate .~ Just currentTime

  if T.null (T.strip (msg ^. body))
     then throwError "Empty message"
     else return messageWithDate

quotedMessage :: StoredMessage -> T.Text
quotedMessage msg =
  let decoded = either (T.pack . show) (view body) $
        msg ^. body . storedMainBody . charsetDecoded' defaultCharsets
      rendered = renderType (msg ^. body . storedMainBody . contentType) decoded
      quoted = T.unlines $ map ("> " <>) $ T.lines rendered
  in quoted

editFile :: FilePath -> IO ()
editFile fp = do
  editor <- fromMaybe "vim" <$> lookupEnv "EDITOR"
  callCommand (editor ++ " " ++ fp)

composeWithEditor :: BL.ByteString -> IO BS.ByteString
composeWithEditor msg = do
  tempdir <- getTemporaryDirectory
  (tempf, temph) <- openTempFile tempdir "message"
  BL8.hPutStr temph msg
  hClose temph
  editFile tempf
  BS8.readFile tempf

addSentMessage :: MIMEMessage -> StoreM ()
addSentMessage msg = do
  let bs = renderMessage msg
  maildirpath <- liftMaildir getMaildirPath
  result <- liftIO $ runExceptT $ withMaildirPath (writeMaildirFile bs) (maildirpath </> ".sent")
  case result of
    Left err -> printError "Could not write sent message"
    Right path -> printStatusMessage $ T.pack $ printf "Written sent message to .sent/new/%s" path

---

getPart :: PartRef -> StoreM ByteEntity
getPart pr@(PartRef msgref partnum) = do
  storedMsg <- getMessage msgref
  msg <- getMIMEMessage storedMsg
  joinMaybe ("Part " ++ T.unpack (reprint partRefParser pr) ++ " not found") $
    return (msg ^? ientities . transferDecodedEmbedError . index (fromIntegral partnum))

getPartByIndex :: Int -> MIMEMessage -> StoreM ByteEntity
getPartByIndex partnum msg = do
  joinMaybe ("Part " ++ show partnum ++ " not found") $
    return (msg ^? ientities . transferDecodedEmbedError . index (fromIntegral partnum))

getMessage :: MessageRef -> StoreM StoredMessage
getMessage mref = case mref of
  MessageRefNumber n' -> do
    n <- getMessageNumber n'
    queryStore' (lookupStoreNumber n)
  MessageRefPath path -> do
    let reader = BS.readFile (T.unpack path)
    joinEither ("Error parsing " ++ T.unpack path) (liftIO $ readStoredMessage Nothing Nothing "" reader)
  MessageRefStdin -> do
    let reader = BS.getContents
    joinEither "Error parsing stdin" (liftIO $ readStoredMessage Nothing Nothing "" reader)

getMessageFilename :: MessageRef -> StoreM T.Text
getMessageFilename mref = case mref of
  MessageRefNumber n' -> do
    n <- getMessageNumber n'
    msg <- queryStore' (lookupStoreNumber n)
    case msg ^. body ^. storedMid of
      Nothing -> liftIO $ (msg ^. body . storedSource) >>= temporaryFile
      Just mid -> T.pack <$> liftMaildir (absoluteMaildirFile mid)
  MessageRefPath path -> return path
  MessageRefStdin -> liftIO $ BS.getContents >>= temporaryFile

temporaryFile :: BS.ByteString -> IO T.Text
temporaryFile bs = do
  tempdir <- getTemporaryDirectory
  (tempf, temph) <- openTempFile tempdir "mime-message"
  BS.hPutStr temph bs
  hClose temph
  return (T.pack tempf)

getMIMEMessage :: StoredMessage -> StoreM MIMEMessage
getMIMEMessage msg = do
  bs <- liftIO $ msg ^. body . storedSource
  joinEither "Error parsing message" $ return $ parse (message mime) bs

getCurrentMessage :: StoreM Natural
getCurrentMessage = do
  path <- liftMaildir (getOtherMaildirFile ".recentmessage")
  read <$> liftIO (readFile path)

getNextMessage :: StoreM Natural
getNextMessage = (+1) <$> getCurrentMessage

getNextMessageRef :: StoreM MessageRef
getNextMessageRef = MessageRefNumber <$> MessageNumber <$> getNextMessage

setCurrentMessage :: Natural -> StoreM ()
setCurrentMessage n = do
  path <- liftMaildir (getOtherMaildirFile ".recentmessage")
  liftIO $ writeFile path (show n)

setCurrentMessageRef :: MessageRef -> StoreM ()
setCurrentMessageRef mref = case mref of
  MessageRefNumber (MessageNumber n) -> setCurrentMessage n
  _ -> return ()

getMessageNumber :: MessageNumber -> StoreM StoreNumber
getMessageNumber n' = case n' of
  MessageNumberDefault -> storeNumber <$> getCurrentMessage
  MessageNumber n -> return (storeNumber n)

getMaildirFormat :: StoreM (Maybe RichFormat)
getMaildirFormat = do
  formatPath <- liftMaildir (getOtherMaildirFile ".format")
  formatString <- liftIO $ readFileIfExists formatPath
  case formatString of
    Nothing -> return Nothing
    Just "" -> return Nothing
    Just formatString' -> case parseRichFormat (head (lines formatString')) of
      Left err -> throwError ("Error parsing format string: " ++ err)
      Right format -> return (Just format)
