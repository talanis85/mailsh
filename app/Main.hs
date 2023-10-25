{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import Control.Monad.Except
import Control.Monad.Logger
import qualified Data.Text as T
import Development.GitRev
import Options.Applicative
import System.Directory
import System.Environment
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.Printf

import Data.Reparser
import Mailsh.Compose
import Mailsh.Maildir
import Mailsh.Types
import Mailsh.Store

import Commands
import Format
import Render

-----------------------------------------------------------------------------

data Options = Options
  { optCommand :: StoreM ()
  , optDebug :: Bool
  , optNoUpdate :: Bool
  , optMaildirPath :: Maybe FilePath
  }

options :: ParserInfo Options
options = info (helper <*> (Options <$> commandP <*> debugOption <*> noUpdateOption <*> maildirPathOption))
  (  fullDesc
  <> progDesc "Manage maildirs from the shell"
  <> header "mailsh - A console maildir tool"
  <> footer ("Version: " ++ version)
  )

debugOption = flag False True (short 'd' <> long "debug")
noUpdateOption = flag False True (short 'u' <> long "noupdate")
maildirPathOption = maybeOption str (short 'm' <> long "maildir")

version :: String
version = $(gitBranch) ++ "@" ++ $(gitHash)

commandP :: Parser (StoreM ())
commandP = hsubparser
  (  command "read"     (info (cmdRead    <$> messageOrPartRefArgument
                                          <*> rendererOption noquoteRenderer
                                          <*> quietFlag)
                              (progDesc "Read a message."))
  <> command "cat"      (info (cmdCat     <$> messageOrPartRefArgument)
                              (progDesc "Output raw message data."))
  <> command "view"     (info (cmdView    <$> messageOrPartRefArgument)
                              (progDesc "View a message or part with mailcap."))
  <> command "headers"  (info (cmdHeaders <$> messageOrPartRefArgument)
                              (progDesc "Print all headers of a message or part"))
--  <> command "visual"   (info (cmdVisual  <$> messageOrPartRefArgument)
--                              (progDesc "View a message in the browser"))
  <> command "save"     (info (cmdSave    <$> messageOrPartRefArgument
                                          <*> maybeOption str (short 'd' <> metavar "DIR" <> help "Where to save the attachment(s)."))
                              (progDesc "Save one or all attachments to disk."))
  <> command "next"     (info (cmdNext    <$> rendererOption previewRenderer)
                              (progDesc "Read the next unread message."))
  <> command "compose"  (info (cmdCompose <$> dryFlag
                                          <*> attachmentOptions
                                          <*> recipientArgument)
                              (progDesc "Compose a new message using your EDITOR"))
  <> command "reply"    (info (cmdReply   <$> dryFlag
                                          <*> replyStrategyFlag
                                          <*> attachmentOptions
                                          <*> messageRefArgument)
                              (progDesc "Reply to a message using your EDITOR."))
  <> command "forward"  (info (cmdForward <$> dryFlag
                                          <*> recipientArgument
                                          <*> messageRefArgument)
                              (progDesc "Forward a message."))
  <> command "ls"       (info (cmdLs      <$> limitOption NoLimit
                                          <*> filterArgument FilterUnseen
                                          <*> formatOption)
                              (filterHelp <> progDesc "List all headers given a filter expression."))
  <> command "lsn"      (info (cmdLsn     <$> filterArgument FilterAll)
                              (filterHelp <> progDesc "List all message numbers given a filter expression."))
  <> command "browse"   (info (cmdBrowse  <$> limitOption (Limit 1000)
                                          <*> filterArgument FilterAll
                                          <*> formatOption)
                              (filterHelp <> progDesc "Browse messages interactively."))
  <> command "trash"    (info (cmdTrash   <$> messageNumberArgument)
                              (progDesc "Trash a message."))
  <> command "recover"  (info (cmdRecover <$> messageNumberArgument)
                              (progDesc "Recover a trashed message."))
  <> command "purge"    (info (pure cmdPurge)
                              (progDesc "Permanently delete trashed messages."))
  <> command "unread"   (info (cmdUnread  <$> messageNumberArgument)
                              (progDesc "Mark a message as unread."))
  <> command "flag"     (info (cmdFlag    <$> messageNumberArgument)
                              (progDesc "Mark a message as flagged."))
  <> command "unflag"   (info (cmdUnflag  <$> messageNumberArgument)
                              (progDesc "Mark a message as unflagged."))
  <> command "filename" (info (cmdFilename <$> messageNumberArgument)
                              (progDesc "Get the filename of a message."))
  <> command "outline"  (info (cmdOutline <$> messageRefArgument)
                              (progDesc "Display an outline of a message."))
  <> command "tar"      (info (cmdTar     <$> messageRefArgument)
                              (progDesc "Output all attachments of a message to stdout as a tar archive."))
  ) <|> (cmdLs <$> limitOption NoLimit <*> filterArgument FilterUnseen <*> formatOption)

filterHelp :: InfoMod a
filterHelp = footerDoc $ Just $ PP.vcat $ map PP.string
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

quietFlag :: Parser Bool
quietFlag = flag False True (short 'q' <> long "quiet" <> help "Dont display anything")

reparserReader :: Reparser String T.Text b -> ReadM b
reparserReader r = eitherReader (reparse r . T.pack)

filterArgument :: FilterExp -> Parser FilterExp
filterArgument def = argument (reparserReader filterExpParser) $
     metavar "FILTER"
  <> help "A filter expression"
  <> value def

limitOption :: Limit -> Parser Limit
limitOption def = option (reparserReader limitParser) $
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
  <> help "Available renderers are: full, outline, preview, noquote, plaintext"
  <> value def
  where
    rendererReader = eitherReader $ \s -> case s of
      "full"      -> Right fullRenderer
      "outline"   -> Right outlineRenderer
      "preview"   -> Right previewRenderer
      "noquote"   -> Right noquoteRenderer
      "plaintext" -> Right plaintextRenderer
      _           -> Left "Invalid renderer"

formatOption :: Parser (Maybe RichFormat)
formatOption = maybeOption (eitherReader parseRichFormat) $
     short 'f'
  <> long "format"
  <> metavar "FORMAT"
  <> help "Printf-style formatting for header lines"

maybeOption :: ReadM a -> Mod OptionFields (Maybe a) -> Parser (Maybe a)
maybeOption r m = option (Just <$> r) (m <> value Nothing)

replyStrategyFlag :: Parser ReplyStrategy
replyStrategyFlag = flag SingleReply GroupReply (short 'g' <> long "group" <> help "Group reply")

recipientArgument :: Parser [T.Text]
recipientArgument = many $ argument str (metavar "RECIPIENT" <> help "The recipient's address")

attachmentOptions :: Parser [T.Text]
attachmentOptions = many $ option str $
     short 'a'
  <> long "attachment"
  <> metavar "FILE"
  <> help "Attach a file (can occur multiple times)"

messageRefArgument :: Parser MessageRef
messageRefArgument = argument (reparserReader messageRefParser) $
     metavar "MESSAGE"
  <> help "Either a message number (e.g. 123), a file path or '-' for stdin. Default is the last accessed message."
  <> value (MessageRefNumber MessageNumberDefault)

partRefArgument :: Parser PartRef
partRefArgument = argument (reparserReader partRefParser) $
     metavar "PART"
  <> help "Either <MESSAGE>#<PART> or #<PART> for a part of the last accessed message."

messageOrPartRefArgument :: Parser (Either MessageRef PartRef)
messageOrPartRefArgument = argument (reparserReader (reparserChoice messageRefParser partRefParser)) $
     metavar "MESSAGE/PART"
  <> help "A message or part."
  <> value (Left (MessageRefNumber MessageNumberDefault))

messageNumberArgument :: Parser MessageNumber
messageNumberArgument = argument (reparserReader messageNumberParser) $
     metavar "MESSAGE"
  <> help "Message number. Default is the last accessed message."
  <> value MessageNumberDefault

-----------------------------------------------------------------------------

{-
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
    Message headers body <- joinEither "Could not parse Message" $ pure $ parseOnly messageP (BL.toStrict bs)
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
          Message headers body <- joinEither "Could not parse Message" $ pure $ parseOnly messageP bs
          getPartNumber n body
        else throwError "Expected type message/rfc822"
      _ -> throwError "Expected type message/rfc822"

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

  messageAfter <- joinEither "Invalid message" $ liftIO $ composeWith messageBefore

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

quotedMessage :: DigestMessage -> T.Text
quotedMessage msg =
  let rendered = renderType (msg ^. contentType) (msg ^. body)
      quoted = T.unlines $ map ("> " <>) $ T.lines rendered
  in quoted

addSentMessage :: ComposedMessage -> StoreM ()
addSentMessage cmsg = do
  bs <- renderComposedMessageRaw cmsg
  maildirpath <- liftMaildir getMaildirPath
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
-}

main :: IO ()
main = do
  opts <- execParser options

  result <- runExceptT $ do
    maildirpath <- do
      cwdMaildir <- do
        x <- liftIO getCurrentDirectory
        valid <- liftIO $ isMaildir x
        if valid
           then return (Just x)
           else return Nothing

      envMaildir <- do
        x <- liftIO $ lookupEnv "MAILDIR"
        case x of
          Nothing -> return Nothing
          Just x' -> do
            valid <- liftIO $ isMaildir x'
            if valid
               then return (Just x')
               else return Nothing

      case optMaildirPath opts <|> cwdMaildir <|> envMaildir of
        Nothing -> throwError "Neither MAILDIR nor CWD contain a valid maildir"
        Just x -> return x

    let doUpdate = if optNoUpdate opts then return () else updateStore

    runStderrLoggingT $ filterLogger (logFilter (optDebug opts)) $
      withStorePath (doUpdate >> optCommand opts) maildirpath

  case result of
    Left err -> printf "Error: %s\n" err
    Right () -> return ()
  where
    logFilter True _ _ = True
    logFilter False _ LevelDebug = False
    logFilter False _ LevelInfo = False
    logFilter False _ _ = True
