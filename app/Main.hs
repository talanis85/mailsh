{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Control.Lens hiding (argument)
import Control.Monad.Except
import Control.Monad.Logger
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import Development.GitRev
import Options.Applicative
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.Printf

import Mailsh.Compose
import Mailsh.Fields
import Mailsh.Filter
import Mailsh.Types
import Mailsh.Maildir
import Mailsh.Message
import Mailsh.MimeRender
import Mailsh.MimeType
import Mailsh.Parse
import Mailsh.Store

import Compose
import MessageRef
import Render

-----------------------------------------------------------------------------

data Options = Options
  { optCommand :: StoreM ()
  , optDebug :: Bool
  }

options :: ParserInfo Options
options = info (helper <*> (Options <$> commandP <*> debugOption))
  (  fullDesc
  <> progDesc "Manage maildirs from the shell"
  <> header "mailsh - A console maildir tool"
  <> footer ("Version: " ++ version)
  )

debugOption = flag False True (short 'd' <> long "debug")

version :: String
version = $(gitBranch) ++ "@" ++ $(gitHash)

commandP :: Parser (StoreM ())
commandP = hsubparser
  (  command "read"     (info (cmdRead    <$> msgArgument
                                          <*> rendererOption noquoteRenderer)
                              (progDesc "Read a message."))
  <> command "cat"      (info (cmdCat     <$> msgArgument)
                              (progDesc "Output raw message data."))
  <> command "view"     (info (cmdView    <$> msgArgument)
                              (progDesc "View a message or part with mailcap."))
  <> command "visual"   (info (cmdVisual  <$> msgArgument)
                              (progDesc "View a message in the browser"))
  <> command "save"     (info (cmdSave    <$> msgArgument
                                          <*> option str (short 'd' <> metavar "DIR" <> help "Where to save the attachment."))
                              (progDesc "Save a part to disk using its given filename."))
  <> command "next"     (info (cmdRead    <$> pure (MessageRefNumber getNextMessageNumber)
                                          <*> rendererOption previewRenderer)
                              (progDesc "Read the next unread message."))
  <> command "compose"  (info (cmdCompose <$> dryFlag
                                          <*> many (option str (short 'a' <> long "attachment" <> metavar "FILE" <> help "Attach a file (can occur multiple times)"))
                                          <*> argument str (metavar "RECIPIENT" <> help "The recipient's address"))
                              (progDesc "Compose a new message using your EDITOR"))
  <> command "reply"    (info (cmdReply   <$> dryFlag
                                          <*> flag SingleReply GroupReply (short 'g' <> long "group" <> help "Group reply")
                                          <*> many (option str (short 'a' <> long "attachment" <> metavar "FILE" <> help "Attach a file (can occur multiple times)"))
                                          <*> msgArgument)
                              (progDesc "Reply to a message using your EDITOR."))
  <> command "forward"  (info (cmdForward <$> dryFlag
                                          <*> argument str (metavar "RECIPIENT" <> help "The recipient's address")
                                          <*> msgArgument)
                              (progDesc "Forward a message."))
  <> command "headers"  (info (cmdHeaders <$> limitOption Nothing
                                          <*> filterArgument (return filterUnseen))
                              (headersHelp <> progDesc "List all headers given a filter expression."))
  <> command "ls"       (info (cmdLs      <$> filterArgument (return filterAll))
                              (headersHelp <> progDesc "List all message numbers given a filter expression."))
  <> command "trash"    (info (cmdTrash   <$> mnArgument)
                              (progDesc "Trash a message."))
  <> command "recover"  (info (cmdRecover <$> mnArgument)
                              (progDesc "Recover a trashed message."))
  <> command "purge"    (info (pure cmdPurge)
                              (progDesc "Permanently delete trashed messages."))
  <> command "unread"   (info (cmdUnread  <$> mnArgument)
                              (progDesc "Mark a message as unread."))
  <> command "flag"     (info (cmdFlag    <$> mnArgument)
                              (progDesc "Mark a message as flagged."))
  <> command "unflag"   (info (cmdUnflag  <$> mnArgument)
                              (progDesc "Mark a message as unflagged."))
  <> command "filename" (info (cmdFilename <$> mnArgument)
                              (progDesc "Get the filename of a message."))
  <> command "outline"  (info (cmdOutline <$> msgArgument)
                              (progDesc "Display an outline of a message."))
  <> command "tar"      (info (cmdTar     <$> msgArgument)
                              (progDesc "Output all attachments of a message to stdout as a tar archive."))
  ) <|> (cmdHeaders <$> limitOption Nothing
                    <*> filterArgument (return filterUnseen))

headersHelp :: InfoMod a
headersHelp = footerDoc $ Just $ PP.vcat $ map PP.string
  [ "Valid filter expressions are:"
  , ""
  , "  /string/       'Subject' or 'From' contains 'string'"
  , "  #keyword#      'Keywords' contain the given keyword"
  , "  a              Matches all messages"
  , "  d              Matches draft messages"
  , "  r              Matches replied messages"
  , "  s              Matches seen messages"
  , "  t              Matches trashed messages"
  , "  f              Matches flagged messages"
  , "  all            Equivalent to '~t' (all non-trashed messages)"
  , "  new            Equivalent to '~s' (all unseen messages)"
  , ""
  , "All of these expressions can be combined with the logical operators '&' (and), '|' (or) and '~' (not)."
  , ""
  , "Examples:"
  , "  f&~d                All flagged messages that are not a draft"
  , "  r&/hello/           All replied messages that contain the string 'hello'"
  , "  (t&/foo/)|(d&/bar/) All trashed messages containing 'foo' and all drafts containing 'bar'"
  ]

dryFlag :: Parser Bool
dryFlag = flag False True (long "dry" <> help "Dont actually send the message")

filterArgument :: StoreM FilterExp -> Parser (StoreM FilterExp)
filterArgument def = argument (eitherReader parseFilterExp) $
     metavar "FILTER"
  <> help "A filter expression"
  <> value def

limitOption :: Maybe Limit -> Parser (Maybe Limit)
limitOption def = option (eitherReader parseLimit) $
     short 'l'
  <> long "limit"
  <> metavar "LIMIT"
  <> help "How many headers to display"
  <> value def

rendererOption :: Renderer -> Parser Renderer
rendererOption def = option rendererReader $
     short 'r'
  <> long "render"
  <> metavar "RENDERER"
  <> help "Available renderers are: full, outline, preview, noquote"
  <> value def
  where
    rendererReader = eitherReader $ \s -> case s of
      "full"    -> Right fullRenderer
      "outline" -> Right outlineRenderer
      "preview" -> Right previewRenderer
      "noquote" -> Right noquoteRenderer
      _         -> Left "Invalid renderer"

maybeOption :: ReadM a -> Mod OptionFields (Maybe a) -> Parser (Maybe a)
maybeOption r m = option (Just <$> r) (m <> value Nothing)

msgArgument :: Parser MessageRef
msgArgument = argument messageRefReader $
     metavar "MESSAGE"
  <> help "Either a message number (e.g. 123), a part reference (e.g. 2#123) or '-' for stdin. Default is the last accessed message."
  <> value (MessageRefNumber getRecentMessageNumber)

mnArgument :: Parser MessageNumber'
mnArgument = argument (setRecentMessageNumber <$> auto) $
     metavar "MESSAGE"
  <> help "Message number. Default is the last accessed message."
  <> value getRecentMessageNumber

queryStore' q = do
  x <- queryStore q
  case x of
    Nothing -> throwError "No such message"
    Just x  -> return x

throwEither :: (MonadError String m) => String -> m (Either String a) -> m a
throwEither s m = do
  r <- m
  case r of
    Left e -> throwError (s ++ ": " ++ show e)
    Right v -> return v

-----------------------------------------------------------------------------

getPartNumber n body
  | n < 1 = throwError "No such part"
  | n > length (body ^.. allParts) = throwError "No such part"
  | otherwise = return ((body ^.. allParts) !! (n-1))

getMessage :: MessageRef -> StoreM (Maybe MID, DigestMessage, BL.ByteString)
getMessage mref = case mref of
  MessageRefNumber mn' -> do
    mn <- mn'
    msg <- queryStore' (lookupMessageNumber mn)
    fp <- liftMaildir $ absoluteMaildirFile (messageMid msg)
    bs <- liftIO $ BL.readFile fp
    return (Just (messageMid msg), messageDigest msg, bs)
  MessageRefPath path -> do
    bs <- liftIO $ BS.readFile path
    case parseOnly messageP bs of
      Left err -> throwError ("Could not parse message:\n" ++ err)
      Right msg -> return (Nothing, digestMessage msg, BL.fromStrict bs)
  MessageRefStdin -> do
    bs <- liftIO $ BS.getContents
    case parseOnly messageP bs of
      Left err -> throwError ("Could not parse message:\n" ++ err)
      Right msg -> return (Nothing, digestMessage msg, BL.fromStrict bs)
  MessageRefPart n mref' -> do
    (_, msg', bs) <- getMessage mref'
    Message headers body <- throwEither "Could not parse Message" $ pure $ parseOnly messageP (BL.toStrict bs)
    part <- getPartNumber n body
    case part of
      Part _ (PartBinary t s) ->
        if isSimpleMimeType "message/rfc822" t
        then do
          case parseOnly messageP s of
            Left err -> throwError ("Could not parse message:\n" ++ err)
            Right msg -> return (Nothing, digestMessage msg, BL.fromStrict s)
        else throwError "Part must be of type message/rfc822"
      _ -> throwError "Part must be of type message/rfc822"

getRawMessage :: MessageRef -> StoreM BL.ByteString
getRawMessage mref = do
  (_, _, bs) <- getMessage mref
  return bs

getPart :: MessageRef -> StoreM Part
getPart mref = case mref of
  MessageRefNumber mn' -> do
    mn <- mn'
    msg <- queryStore' (lookupMessageNumber mn)
    fp <- liftMaildir $ absoluteMaildirFile (messageMid msg)
    bs <- liftIO $ BS.readFile fp
    return (Part DispositionInline (PartBinary (mkMimeType "message" "rfc822") bs))
  MessageRefPath path -> do
    bs <- liftIO $ BS.readFile path
    return (Part DispositionInline (PartBinary (mkMimeType "message" "rfc822") bs))
  MessageRefStdin -> do
    bs <- liftIO $ BS.getContents
    return (Part DispositionInline (PartBinary (mkMimeType "message" "rfc822") bs))
  MessageRefPart n mref' -> do
    part' <- getPart mref'
    case part' of
      Part _ (PartBinary t bs) -> do
        if isSimpleMimeType "message/rfc822" t
        then do
          Message headers body <- throwEither "Could not parse Message" $ pure $ parseOnly messageP bs
          getPartNumber n body
        else throwError "Expected type message/rfc822"
      _ -> throwError "Expected type message/rfc822"

cmdRead :: MessageRef -> Renderer -> StoreM ()
cmdRead mref renderer = do
  (mid, msg, _) <- getMessage mref
  renderer msg
  maybe (return ()) (liftMaildir . setFlag 'S') mid

cmdCat :: MessageRef -> StoreM ()
cmdCat mref = do
  part <- getPart mref
  case part ^. partBody of
    PartText t s -> liftIO $ T.putStrLn s
    PartBinary t s -> liftIO $ BSC.putStrLn s

cmdView :: MessageRef -> StoreM ()
cmdView mref = do
  part <- getPart mref
  case part ^? partFilename of
    Nothing -> throwError "Part has no file name"
    Just filename -> case part ^. partBody of
      PartText _ s   -> liftIO $ runXdgOpen filename (T.encodeUtf8 s)
      PartBinary _ s -> liftIO $ runXdgOpen filename s

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

cmdSave :: MessageRef -> FilePath -> StoreM ()
cmdSave mref path = do
  part <- getPart mref
  case part ^. partBody of
    PartBinary t s -> case part ^? partFilename of
      Nothing -> throwError "Part has no filename"
      Just fn -> liftIO $ BS.writeFile (path </> T.unpack fn) s
    PartText t s -> throwError "Cannot save text parts"

getMailfromEnv :: IO (Maybe Mailbox)
getMailfromEnv = do
  maybeValue <- lookupEnv "MAILFROM"
  return $ do
    value <- maybeValue
    either (const Nothing) Just (parseOnly mailboxP (BSC.pack value))

getSignature :: IO T.Text
getSignature = do
  signaturePath <- (</> ".config/mailsh/signature") <$> getHomeDirectory
  signatureExists <- doesFileExist signaturePath
  if signatureExists then T.readFile signaturePath else return ""

composeMessage :: [Field] -> T.Text -> StoreM ComposedMessage
composeMessage headers text = do
  from <- liftIO getMailfromEnv

  let headersBefore = catMaybes
        [ mkField fFrom <$> return <$> from
        ] ++ headers

      messageBefore = emptyComposedMessage
        { cmessageFields = headersBefore
        , cmessageText = text
        }

  messageAfter <- throwEither "Invalid message" $ liftIO $ composeWith messageBefore

  currentTime <- liftIO $ getCurrentTime
  let messageWithDate = messageAfter {
        cmessageFields = mkField fDate currentTime : cmessageFields messageAfter
        }

  if T.null (T.strip (cmessageText messageWithDate))
     then throwError "Empty message"
     else return messageWithDate

ifSend :: (MonadIO m) => Bool -> m () -> m ()
ifSend dry sendAction = do
  when (not dry) $ do
    sendIt <- liftIO $ askYesNo "Send this message?"
    if sendIt
       then do
         sendAction
         liftIO $ putStrLn "Ok, mail sent."
        else do
         liftIO $ putStrLn "Ok, mail discarded."

cmdCompose :: Bool -> [FilePath] -> Recipient -> StoreM ()
cmdCompose dry attachments rcpt = do
  let headers = catMaybes
        [ mkField fTo   <$> either (const Nothing) Just (parseOnly mailboxListP (BSC.pack rcpt))
        ]
        ++ map (mkField (fOptionalField "Attachment")) (map T.pack attachments)

  text <- liftIO getSignature

  msg <- composeMessage headers text
  liftIO $ T.putStr $ renderComposedMessage msg

  ifSend dry $ do
    sendMessage msg
    addSentMessage msg

quotedMessage :: DigestMessage -> T.Text
quotedMessage msg =
  let rendered = renderType (messageBodyType msg) (messageBody msg)
      quoted = T.unlines $ map ("> " <>) $ T.lines rendered
  in quoted

cmdReply :: Bool -> ReplyStrategy -> [FilePath] -> MessageRef -> StoreM ()
cmdReply dry strat attachments mref = do
  (mid, msg, _) <- getMessage mref
  let headers = replyHeaders strat msg
        ++ map (mkField (fOptionalField "Attachment")) (map T.pack attachments)
      text = quotedMessage msg

  msg <- composeMessage headers text
  liftIO $ T.putStr $ renderComposedMessage msg

  ifSend dry $ do
    sendMessage msg
    addSentMessage msg
    maybe (return ()) (liftMaildir . setFlag 'R') mid

cmdForward :: Bool -> Recipient -> MessageRef -> StoreM ()
cmdForward dry rcpt mref = do
  let headers = catMaybes
        [ mkField fTo   <$> either (const Nothing) Just (parseOnly mailboxListP (BSC.pack rcpt))
        ]
      text = ""

  msg <- composeMessage headers text
  part <- getPart mref
  let original = case part ^. partBody of
        PartBinary t s -> Attachment
          { attachmentContentType = t
          , attachmentFilename = "original"
          , attachmentData = BL.fromStrict s
          }
        PartText t s -> Attachment
          { attachmentContentType = withMimeParam "charset" "utf-8" (mkMimeType "text" t)
          , attachmentFilename = "original"
          , attachmentData = BL.fromStrict (T.encodeUtf8 s)
          }
  let msg' = msg { cmessageAttachments = original : cmessageAttachments msg }

  liftIO $ T.putStr $ renderComposedMessage msg'

  ifSend dry $ do
    sendMessage msg'
    addSentMessage msg'

cmdHeaders :: Maybe Limit -> StoreM FilterExp -> StoreM ()
cmdHeaders limit' filter' = do
  filter <- filter'
  limit <- case limit' of
    Just x -> return x
    Nothing -> Just . subtract 2 <$> liftIO terminalHeight
  result <- queryStore (filterBy filter limit)
  let messages = resultRows result
  liftIO $ mapM_ (uncurry printMessageSingle) messages
  liftIO $ printResultCount result

cmdLs :: StoreM FilterExp -> StoreM ()
cmdLs filter' = do
  filter <- filter'
  result <- queryStore (filterBy filter Nothing)
  let messages = resultRows result
  liftIO $ putStrLn $ intercalate " " $ map (show . fst) messages

cmdTrash :: MessageNumber' -> StoreM ()
cmdTrash = modifyMessage (liftMaildir . setFlag 'T') "Trashed message."

cmdRecover :: MessageNumber' -> StoreM ()
cmdRecover = modifyMessage (liftMaildir . unsetFlag 'T') "Recovered message."

cmdPurge :: StoreM ()
cmdPurge = do
  n <- liftMaildir purgeMaildir
  liftIO $ printf "Purged %d messages.\n" n

cmdUnread :: MessageNumber' -> StoreM ()
cmdUnread = modifyMessage (liftMaildir . unsetFlag 'S') "Marked message as unread."

cmdFlag :: MessageNumber' -> StoreM ()
cmdFlag = modifyMessage (liftMaildir . setFlag 'F') "Flagged message."

cmdUnflag :: MessageNumber' -> StoreM ()
cmdUnflag = modifyMessage (liftMaildir . unsetFlag 'F') "Unflagged message."

cmdFilename :: MessageNumber' -> StoreM ()
cmdFilename mn' = do
  mn <- mn'
  msg <- queryStore' (lookupMessageNumber mn)
  let mid = messageMid msg
  filename <- liftMaildir $ absoluteMaildirFile mid
  liftIO $ putStrLn filename

cmdOutline :: MessageRef -> StoreM ()
cmdOutline mref = do
  (_, msg, bs) <- getMessage mref
  Message headers body <- throwEither "Could not parse Message" $ pure $ parseOnly messageP (BL.toStrict bs)
  liftIO $ putStrLn $ outline body

cmdTar :: MessageRef -> StoreM ()
cmdTar mref = do
  bs <- getRawMessage mref
  Message headers parts <- throwEither "Could not parse Message" $ pure $ parseOnly messageP (BL.toStrict bs)
  let mkAttachmentEntry (filename, body) = case filename of
        Nothing -> return Nothing
        Just fn -> case Tar.toTarPath False (T.unpack fn) of
          Left err -> throwError $ printf "Invalid path '%s': %s" fn err
          Right tp -> case body of
            PartText _ text -> return $ Just $ Tar.fileEntry tp (BL.fromStrict (T.encodeUtf8 text))
            PartBinary _ bs -> return $ Just $ Tar.fileEntry tp (BL.fromStrict bs)
  attachments <- catMaybes <$> mapM mkAttachmentEntry (parts ^.. attachmentParts)
  liftIO $ BL.putStr (Tar.write attachments)

modifyMessage :: (MID -> StoreM ()) -> String -> MessageNumber' -> StoreM ()
modifyMessage f notice mn' = do
  mn <- mn'
  msg <- queryStore' (lookupMessageNumber mn)
  f (messageMid msg)
  liftIO $ putStrLn notice
  liftIO $ printMessageSingle mn msg

addSentMessage :: ComposedMessage -> StoreM ()
addSentMessage cmsg = do
  bs <- renderComposedMessageRaw cmsg
  maildirpath <- liftIO getCurrentDirectory
  result <- liftIO $ runExceptT $ withMaildirPath (writeMaildirFile bs) (maildirpath </> ".sent")
  case result of
    Left err -> liftIO $ printf "Error: %s\n" err
    Right path -> liftIO $ printf "Written sent message to .sent/new/%s\n" path

askYesNo :: String -> IO Bool
askYesNo prompt = do
  putStr (prompt ++ " (y/n) ")
  hFlush stdout
  r <- getChar
  if r == 'y' then return True else return False

main :: IO ()
main = do
  opts <- execParser options
  maildirpath <- getCurrentDirectory
  result <- runExceptT $ runStderrLoggingT $ filterLogger (logFilter (optDebug opts)) $ withStorePath (optCommand opts) maildirpath
  case result of
    Left err -> printf "Error: %s\n" err
    Right () -> return ()
  where
    logFilter True _ _ = True
    logFilter False _ LevelDebug = False
    logFilter False _ LevelInfo = False
    logFilter False _ _ = True
