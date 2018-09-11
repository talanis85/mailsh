{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Control.Monad.Except
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Development.GitRev
import Options.Applicative
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.Printf

import Mailsh.Compose
import Mailsh.Filter
import Mailsh.Types
import Mailsh.Maildir
import Mailsh.MimeRender
import Mailsh.Parse
import Mailsh.Store

import Network.Email

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
  <> command "save"     (info (cmdSave    <$> msgArgument
                                          <*> option str (short 'd' <> metavar "DIR" <> help "Where to save the attachment."))
                              (progDesc "Save a part to disk using its given filename."))
  <> command "next"     (info (cmdRead    <$> pure (MessageRefNumber getNextMessageNumber)
                                          <*> rendererOption previewRenderer)
                              (progDesc "Read the next unread message."))
  <> command "compose"  (info (cmdCompose <$> dryFlag
                                          <*> argument str (metavar "RECIPIENT" <> help "The recipient's address"))
                              (progDesc "Compose a new message using your EDITOR"))
  <> command "reply"    (info (cmdReply   <$> dryFlag
                                          <*> flag SingleReply GroupReply (short 'g' <> long "group" <> help "Group reply")
                                          <*> msgArgument)
                              (progDesc "Reply to a message using your EDITOR."))
  <> command "forward"  (info (cmdForward <$> dryFlag
                                          <*> argument str (metavar "RECIPIENT" <> help "The recipient's address")
                                          <*> msgArgument)
                              (progDesc "Forward a message."))
  <> command "headers"  (info (cmdHeaders <$> limitOption Nothing
                                          <*> filterArgument (return filterUnseen))
                              (headersHelp <> progDesc "List all headers given a filter expression."))
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
  | n > length (partList body) = throwError "No such part"
  | otherwise = return (partList body !! (n-1))

getMessage :: MessageRef -> StoreM (Message, BL.ByteString)
getMessage mref = case mref of
  MessageRefNumber mn' -> do
    mn <- mn'
    msg <- queryStore' (lookupMessageNumber mn)
    fp <- liftMaildir $ absoluteMaildirFile (messageMid msg)
    bs <- liftIO $ BL.readFile fp
    return (msg, bs)
  MessageRefPath path -> do
    bs <- liftIO $ BL.readFile path
    msg' <- liftIO $ parseMessageString "" "" bs
    case msg' of
      Left err -> throwError ("Could not parse message:\n" ++ err)
      Right msg -> return (msg, bs)
  MessageRefStdin -> do
    bs <- liftIO $ BL.getContents
    msg' <- liftIO $ parseMessageString "" "" bs
    case msg' of
      Left err -> throwError ("Could not parse message:\n" ++ err)
      Right msg -> return (msg, bs)
  MessageRefPart n mref' -> do
    (msg', bs) <- getMessage mref'
    (headers, body) <- liftMaildir $ parseMailstring bs $ messageP (mimeTextPlain "utf8")
    part <- getPartNumber n body
    case part of
      PartBinary t s ->
        if isSimpleMimeType "message/rfc822" t
        then do
          let bs = BL.fromStrict s
          msg'' <- liftIO $ parseMessageString "" "" bs
          case msg'' of
            Left err -> throwError ("Could not parse message:\n" ++ err)
            Right msg -> return (msg, bs)
        else throwError "Part must be of type message/rfc822"
      _ -> throwError "Part must be of type message/rfc822"

getRawMessage :: MessageRef -> StoreM BL.ByteString
getRawMessage mref = snd <$> getMessage mref

getPart :: MessageRef -> StoreM Part
getPart mref = case mref of
  MessageRefNumber mn' -> do
    mn <- mn'
    msg <- queryStore' (lookupMessageNumber mn)
    fp <- liftMaildir $ absoluteMaildirFile (messageMid msg)
    bs <- liftIO $ BS.readFile fp
    return (PartBinary (mkMimeType "message" "rfc822") bs)
  MessageRefPath path -> do
    bs <- liftIO $ BS.readFile path
    return (PartBinary (mkMimeType "message" "rfc822") bs)
  MessageRefStdin -> do
    bs <- liftIO $ BS.getContents
    return (PartBinary (mkMimeType "message" "rfc822") bs)
  MessageRefPart n mref' -> do
    part' <- getPart mref'
    case part' of
      PartBinary t bs -> do
        if isSimpleMimeType "message/rfc822" t
        then do
          (headers, body) <- liftMaildir $ parseMailstring (BL.fromStrict bs) $ messageP (mimeTextPlain "utf8")
          getPartNumber n body
        else throwError "Expected type message/rfc822"
      _ -> throwError "Expected type message/rfc822"

cmdRead :: MessageRef -> Renderer -> StoreM ()
cmdRead mref renderer = do
  (msg, _) <- getMessage mref
  renderer msg
  liftMaildir $ setFlag 'S' (messageMid msg)

cmdCat :: MessageRef -> StoreM ()
cmdCat mref = do
  part <- getPart mref
  case part of
    PartText t s -> liftIO $ T.putStrLn s
    PartBinary t s -> liftIO $ BSC.putStrLn s

cmdView :: MessageRef -> StoreM ()
cmdView mref = do
  part <- getPart mref
  case part of
    PartText t s   -> liftIO $ runMailcap (mkMimeType "text" t) (BSC.pack (T.unpack s))
    PartBinary t s -> liftIO $ runMailcap t s

cmdSave :: MessageRef -> FilePath -> StoreM ()
cmdSave mref path = do
  part <- getPart mref
  case part of
    PartBinary t s -> case lookupMimeParam "name" t of
      Nothing -> throwError "Part has no filename"
      Just fn -> liftIO $ BS.writeFile (path </> fn) s
    PartText t s -> throwError "Cannot save text parts"

cmdCompose :: Bool -> Recipient -> StoreM ()
cmdCompose dry rcpt = do
  from <- do
    x <- liftIO (lookupEnv "MAILFROM")
    return (x >>= parseStringMaybe nameAddrP)
  let initialHeaders = catMaybes
        [ mkField fFrom <$> return <$> from
        , mkField fTo   <$> parseStringMaybe nameAddrsP rcpt
        ]

  initialMessage <- liftIO $ do
    signaturePath <- (</> ".config/mailsh/signature") <$> getHomeDirectory
    signatureExists <- doesFileExist signaturePath
    if signatureExists then readFile signaturePath else return ""

  (headers, body) <- throwEither "Invalid message" $ liftIO $ composeWith initialHeaders initialMessage
  if dry
  then do
    msg <- renderMessageS headers body
    liftIO $ putStrLn msg
  else do
    preview <- renderCompose headers (T.unpack body)
    liftIO $ putStrLn preview
    sendIt <- liftIO $ askYesNo "Send this message?"
    if sendIt
    then do
      mail <- generateMessage headers body
      liftIO $ sendMessage mail
      liftIO $ putStrLn "Ok, mail sent."
    else liftIO $ putStrLn "Ok, mail discarded."

cmdReply :: Bool -> ReplyStrategy -> MessageRef -> StoreM ()
cmdReply dry strat mref = do
  (msg, _) <- getMessage mref
  let mid = messageMid msg
  from <- do
    x <- liftIO (lookupEnv "MAILFROM")
    liftIO $ putStrLn $ show x
    return (x >>= parseStringMaybe nameAddrP)
  liftIO $ putStrLn $ show from
  let headers = replyHeaders from strat msg
  let rendered = renderType (messageBodyType msg) (messageBody msg)
  let quoted = unlines $ map ("> " ++) $ lines rendered
  (headers, body) <- throwEither "Invalid message" $ liftIO $ composeWith headers quoted
  if dry
  then do
    msg <- renderMessageS headers body
    liftIO $ putStrLn msg
  else do
    preview <- renderCompose headers (T.unpack body)
    liftIO $ putStrLn preview
    sendIt <- liftIO $ askYesNo "Send this message?"
    if sendIt
    then do
      mail <- generateMessage headers body
      liftIO $ sendMessage mail
      liftMaildir $ setFlag 'R' mid
      liftIO $ putStrLn "Ok, mail sent."
    else liftIO $ putStrLn "Ok, mail discarded."

cmdForward :: Bool -> Recipient -> MessageRef -> StoreM ()
cmdForward dry rcpt mref = do
  from <- do
    x <- liftIO (lookupEnv "MAILFROM")
    return (x >>= parseStringMaybe nameAddrP)
  let initialHeaders = catMaybes
        [ mkField fFrom <$> return <$> from
        , mkField fTo   <$> parseStringMaybe nameAddrsP rcpt
        ]
  (headers, body) <- throwEither "Invalid message" $ liftIO $ composeWith initialHeaders ""
  mail <- generateMessage headers body
  part <- getPart mref
  let mail' = case part of
        PartBinary t s ->
          addAttachmentBS (T.pack (simpleMimeType t))
                          (T.pack "original")
                          (BL.fromStrict s)
                          mail
        PartText t s ->
          addAttachmentBS (T.pack ("text/" ++ t ++ "; charset=utf-8"))
                          (T.pack "original")
                          (BL.fromStrict (T.encodeUtf8 s))
                          mail
  if dry
  then do
    msg <- liftIO $ renderMail' mail'
    liftIO $ BL.putStr msg
  else do
    liftIO $ sendMessage mail'

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
  (msg, bs) <- getMessage mref
  (headers, body) <- liftMaildir $ parseMailstring bs $ messageP (mimeTextPlain "utf8")
  liftIO $ putStrLn $ outline body

cmdTar :: MessageRef -> StoreM ()
cmdTar mref = do
  bs <- getRawMessage mref
  (headers, parts) <- liftMaildir $ parseMailstring bs $ messageP (mimeTextPlain "utf8")
  let mkAttachmentEntry (PartText _ _) = return Nothing
      mkAttachmentEntry (PartBinary mt bs) = case lookupMimeParam "name" mt of
        Nothing -> return Nothing
        Just fn -> case Tar.toTarPath False fn of
          Left err -> throwError $ printf "Invalid path '%s': %s" fn err
          Right tp -> return $ Just $ Tar.fileEntry tp (BL.fromStrict bs)
  attachments <- catMaybes <$> mapM mkAttachmentEntry (partList parts)
  liftIO $ BL.putStr (Tar.write attachments)

modifyMessage :: (MID -> StoreM ()) -> String -> MessageNumber' -> StoreM ()
modifyMessage f notice mn' = do
  mn <- mn'
  msg <- queryStore' (lookupMessageNumber mn)
  f (messageMid msg)
  liftIO $ putStrLn notice
  liftIO $ printMessageSingle mn msg

parseMaildirFile :: MID -> Attoparsec a -> MaildirM a
parseMaildirFile mid p = do
  fp <- absoluteMaildirFile mid
  r <- liftIO $ parseCrlfFile fp p
  case r of
    Left err -> throwError ("Cannot parse message " ++ err)
    Right v  -> return v

parseMailfile :: (MonadError String m, MonadIO m) => FilePath -> Attoparsec a -> m a
parseMailfile fp p = do
  r <- liftIO $ parseCrlfFile fp p
  case r of
    Left err -> throwError ("Cannot parse message " ++ err)
    Right v  -> return v

parseMailstring :: (MonadError String m) => BL.ByteString -> Attoparsec a -> m a
parseMailstring s p = do
  let r = parseCrlfByteString s p
  case r of
    Left err -> throwError ("Cannot parse message " ++ err)
    Right v  -> return v

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
