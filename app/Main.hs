{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Development.GitRev
import Options.Applicative
import System.Directory
import System.Environment
import System.FilePath
import Text.Printf

import Mailsh.Filter
import Mailsh.Types
import Mailsh.Maildir
import Mailsh.MimeRender
import Mailsh.Parse
import Mailsh.Store

import Network.Email

import Browse
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
commandP = subparser
  (  command "read"     (info (cmdRead    <$> msgArgument
                                          <*> rendererOption noquoteRenderer) idm)
  <> command "cat"      (info (cmdCat     <$> msgArgument
                                          <*> maybeOption auto (short 'p' <> metavar "PART")) idm)
  <> command "view"     (info (cmdView    <$> msgArgument
                                          <*> option auto (short 'p' <> metavar "PART")) idm)
  <> command "save"     (info (cmdSave    <$> msgArgument
                                          <*> option auto (short 'p' <> metavar "PART")
                                          <*> option str (short 'd' <> metavar "DIR")) idm)
  <> command "next"     (info (cmdRead    <$> pure (MessageRefNumber getNextMessageNumber)
                                          <*> rendererOption previewRenderer) idm)
  <> command "compose"  (info (cmdCompose <$> flag False True (long "nosend")
                                          <*> argument str (metavar "RECIPIENT")) idm)
  <> command "reply"    (info (cmdReply   <$> flag False True (long "nosend")
                                          <*> flag SingleReply GroupReply (long "group")
                                          <*> msgArgument) idm)
  <> command "forward"  (info (cmdForward <$> msgArgument
                                          <*> argument str (metavar "RECIPIENT")) idm)
  <> command "headers"  (info (cmdHeaders <$> limitOption Nothing
                                          <*> argument (eitherReader parseFilterExp)
                                                       (metavar "FILTER" <> value (return filterUnseen)))
                              idm)
  <> command "browse"   (info (cmdBrowse  <$> limitOption Nothing
                                          <*> argument (eitherReader parseFilterExp)
                                                       (metavar "FILTER" <> value (return filterUnseen)))
                              idm)
  <> command "trash"    (info (cmdTrash   <$> mnArgument) idm)
  <> command "recover"  (info (cmdRecover <$> mnArgument) idm)
  <> command "purge"    (info (pure cmdPurge) idm)
  <> command "unread"   (info (cmdUnread  <$> mnArgument) idm)
  <> command "flag"     (info (cmdFlag    <$> mnArgument) idm)
  <> command "unflag"   (info (cmdUnflag  <$> mnArgument) idm)
  <> command "filename" (info (cmdFilename <$> mnArgument) idm)
  <> command "outline"  (info (cmdOutline <$> msgArgument) idm)
  ) <|> (cmdHeaders <$> limitOption Nothing
                    <*> argument (eitherReader parseFilterExp)
                                 (metavar "FILTER" <> value (return filterUnseen)))
    where
      limitOption def = option (eitherReader parseLimit) (short 'l' <> long "limit" <> metavar "LIMIT" <> value def)
      rendererOption def = option rendererReader (   short 'r'
                                                  <> long "render"
                                                  <> metavar "RENDERER"
                                                  <> value def
                                                 )
      rendererReader = eitherReader $ \s -> case s of
        "full"    -> Right fullRenderer
        "outline" -> Right outlineRenderer
        "preview" -> Right previewRenderer
        "noquote" -> Right noquoteRenderer
        _         -> Left "Invalid renderer"

maybeOption :: ReadM a -> Mod OptionFields (Maybe a) -> Parser (Maybe a)
maybeOption r m = option (Just <$> r) (m <> value Nothing)

msgArgument :: Parser MessageRef
msgArgument = argument messageRefReader
                       (metavar "MESSAGE" <> value (MessageRefNumber getRecentMessageNumber))

mnArgument :: Parser MessageNumber'
mnArgument = argument (setRecentMessageNumber <$> auto)
                      (metavar "MESSAGE" <> value getRecentMessageNumber)

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
    body <- liftMaildir $ parseMailstring bs $ do
      h <- parseHeaders
      b <- parseMessage (mimeTextPlain "utf8") h
      return b
    case partList body !! (n-1) of
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

cmdRead :: MessageRef -> Renderer -> StoreM ()
cmdRead mref renderer = do
  (msg, _) <- getMessage mref
  renderer msg
  liftMaildir $ setFlag 'S' (messageMid msg)

cmdCat :: MessageRef -> Maybe Int -> StoreM ()
cmdCat mref part = do
  (msg, bs) <- getMessage mref
  case part of
    Nothing -> liftIO $ BLC.putStrLn bs
    Just part -> do
      (headers, body) <- liftMaildir $ parseMailstring bs $ do
        h <- parseHeaders
        b <- parseMessage (mimeTextPlain "utf8") h
        return (h, b)
      liftIO $ outputPart part body

cmdView :: MessageRef -> Int -> StoreM ()
cmdView mref part = do
  (msg, bs) <- getMessage mref
  (headers, body) <- liftMaildir $ parseMailstring bs $ do
    h <- parseHeaders
    b <- parseMessage (mimeTextPlain "utf8") h
    return (h, b)
  case partList body !! (part-1) of
    PartText t s   -> liftIO $ runMailcap (mkMimeType "text" t) (BSC.pack (T.unpack s))
    PartBinary t s -> liftIO $ runMailcap t s

cmdSave :: MessageRef -> Int -> FilePath -> StoreM ()
cmdSave mref part path = do
  (msg, bs) <- getMessage mref
  (headers, body) <- liftMaildir $ parseMailstring bs $ do
    h <- parseHeaders
    b <- parseMessage (mimeTextPlain "utf8") h
    return (h, b)
  case partList body !! (part-1) of
    PartBinary t s -> case lookupMimeParam "name" t of
      Nothing -> throwError "Part has no filename"
      Just fn -> liftIO $ BS.writeFile (path </> fn) s
    PartText t s -> throwError "Cannot save text parts"

cmdCompose :: Bool -> Recipient -> StoreM ()
cmdCompose nosend rcpt = do
  from <- do
    x <- liftIO (lookupEnv "MAILFROM")
    return (x >>= parseStringMaybe parseNameAddr)
  let initialHeaders = catMaybes
        [ mkField fFrom <$> return <$> from
        , mkField fTo   <$> parseStringMaybe parseNameAddrs rcpt
        ]
  (headers, body) <- throwEither "Invalid message" $ liftIO $ composeWith initialHeaders ""
  unless nosend $ do
    mail <- generateMessage headers body
    liftIO $ sendMessage mail
  msg <- renderMessageS headers body
  liftIO $ putStrLn msg

cmdReply :: Bool -> ReplyStrategy -> MessageRef -> StoreM ()
cmdReply nosend strat mref = do
  (msg, _) <- getMessage mref
  let mid = messageMid msg
  from <- do
    x <- liftIO (lookupEnv "MAILFROM")
    liftIO $ putStrLn $ show x
    return (x >>= parseStringMaybe parseNameAddr)
  liftIO $ putStrLn $ show from
  let headers = replyHeaders from strat msg
  let rendered = renderType (messageBodyType msg) (messageBody msg)
  let quoted = unlines $ map ("> " ++) $ lines rendered
  (headers, body) <- throwEither "Invalid message" $ liftIO $ composeWith headers quoted
  unless nosend $ do
    mail <- generateMessage headers body
    liftIO $ sendMessage mail
    liftMaildir $ setFlag 'R' mid
  msg <- renderMessageS headers body
  liftIO $ putStrLn msg

cmdForward :: MessageRef -> Recipient -> StoreM ()
cmdForward mref rcpt = do
  from <- do
    x <- liftIO (lookupEnv "MAILFROM")
    return (x >>= parseStringMaybe parseNameAddr)
  let initialHeaders = catMaybes
        [ mkField fFrom <$> return <$> from
        , mkField fTo   <$> parseStringMaybe parseNameAddrs rcpt
        ]
  (headers, body) <- throwEither "Invalid message" $ liftIO $ composeWith initialHeaders ""
  mail <- generateMessage headers body
  attachment <- getRawMessage mref
  let mail' = addAttachmentBS (T.pack "message/rfc822") (T.pack "original") attachment mail
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

cmdBrowse :: Maybe Limit -> StoreM FilterExp -> StoreM ()
cmdBrowse limit' filter' = do
  filter <- filter'
  limit <- case limit' of
    Just x -> return x
    Nothing -> Just . subtract 2 <$> liftIO terminalHeight
  result <- queryStore (filterBy filter limit)
  let messages = resultRows result
  liftIO $ browseMessages messages

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
  (headers, body) <- liftMaildir $ parseMailstring bs $ do
    h <- parseHeaders
    b <- parseMessage (mimeTextPlain "utf8") h
    return (h, b)
  liftIO $ putStrLn $ outline body

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
