{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe
import qualified Data.Text as T
import Development.GitRev
import Options.Applicative
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import Text.Printf

import Mailsh.Compose
import Mailsh.Filter
import Mailsh.Types
import Mailsh.Maildir
import Mailsh.Parse
import Mailsh.Store

import Network.Email

import Render

-----------------------------------------------------------------------------

type MessageNumber' = StoreM MessageNumber

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
  <> command "next"     (info (cmdRead    <$> pure getNextMessageNumber
                                          <*> rendererOption previewRenderer) idm)
  <> command "compose"  (info (cmdCompose <$> flag False True (long "nosend")
                                          <*> argument str (metavar "RECIPIENT")) idm)
  <> command "reply"    (info (cmdReply   <$> flag False True (long "nosend")
                                          <*> flag SingleReply GroupReply (long "group")
                                          <*> msgArgument) idm)
  <> command "headers"  (info (cmdHeaders <$> maybeOption auto (short 'l' <> metavar "LIMIT")
                                          <*> argument (eitherReader parseFilterExp)
                                                       (metavar "FILTER" <> value (return filterUnseen)))
                              idm)
  <> command "trash"    (info (cmdTrash   <$> msgArgument) idm)
  <> command "recover"  (info (cmdRecover <$> msgArgument) idm)
  <> command "purge"    (info (pure cmdPurge) idm)
  <> command "unread"   (info (cmdUnread  <$> msgArgument) idm)
  <> command "flag"     (info (cmdFlag    <$> msgArgument) idm)
  <> command "unflag"   (info (cmdUnflag  <$> msgArgument) idm)
  ) <|> (cmdHeaders <$> maybeOption auto (short 'l' <> metavar "LIMIT")
                    <*> argument (eitherReader parseFilterExp)
                                 (metavar "FILTER" <> value (return filterUnseen)))
    where
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

msgArgument :: Parser MessageNumber'
msgArgument = argument (setRecentMessageNumber <$> auto)
                       (metavar "MESSAGE" <> value getRecentMessageNumber)

maildirFile :: String -> MaildirM FilePath
maildirFile f = do
  p <- getMaildirPath
  return (p </> f)

getRecentMessageNumber :: MessageNumber'
getRecentMessageNumber = read <$> (liftMaildir (maildirFile ".recentmessage") >>= liftIO . readFile)

setRecentMessageNumber :: MessageNumber -> MessageNumber'
setRecentMessageNumber n = (liftMaildir (maildirFile ".recentmessage") >>= liftIO . flip writeFile (show n))
                           >> return n

getNextMessageNumber :: MessageNumber'
getNextMessageNumber = do
  messages <- queryStore (filterBy filterUnseen Nothing)
  setRecentMessageNumber $ fromMaybe (messageNumber 0) $ listToMaybe $ map fst messages

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

cmdRead :: MessageNumber' -> Renderer -> StoreM ()
cmdRead mn' renderer = do
  mn <- mn'
  msg <- queryStore' (lookupMessageNumber mn)
  renderer msg
  liftMaildir $ setFlag 'S' (messageMid msg)

cmdCat :: MessageNumber' -> Maybe Int -> StoreM ()
cmdCat mn' part = do
  mn <- mn'
  msg <- queryStore' (lookupMessageNumber mn)
  case part of
    Nothing -> do
      fp <- liftMaildir $ absoluteMaildirFile (messageMid msg)
      liftIO $ BL.readFile fp >>= BLC.putStrLn
    Just part -> do
      (headers, body) <- liftMaildir $ parseMaildirFile (messageMid msg) $ do
        h <- parseHeaders
        b <- parseMessage (mimeTextPlain "utf8") h
        return (h, b)
      liftIO $ outputPart part body

cmdCompose :: Bool -> Recipient -> StoreM ()
cmdCompose nosend rcpt = do
  from <- do
    x <- liftIO (lookupEnv "MAILFROM")
    return (x >>= parseString parseNameAddr)
  let initialHeaders = catMaybes
        [ mkField fFrom <$> return <$> from
        , mkField fTo   <$> parseString parseNameAddrs rcpt
        ]
  (headers, body) <- throwEither "Invalid message" $ liftIO $ composeWith initialHeaders ""
  unless nosend $ sendMessage headers body
  msg <- renderMessageS headers body
  liftIO $ putStrLn msg

cmdReply :: Bool -> ReplyStrategy -> MessageNumber' -> StoreM ()
cmdReply nosend strat mn' = return ()
  {-
  mn <- mn'
  msg <- queryStore' (lookupMessageNumber mn)
  let mid = messageMid msg
  myaddr <- liftIO $ lookupEnv "MAILADDR"
  myname <- liftIO $ lookupEnv "MAILNAME"
  let mynameaddr = NameAddr <$> pure myname <*> myaddr
  (headers, body) <- liftMaildir $ parseMaildirFile mid $ do
    h <- parseHeaders
    m <- parseMessage (mimeTextPlain "utf8") h
    return (h, m)
  from <- do
    x <- liftIO (lookupEnv "MAILFROM")
    return (x >>= parseString parseNameAddr)
  let initialHeaders = catMaybes
        [ mkField fFrom <$> return <$> from
        ]
        ++ replyHeaders mynameaddr strat headers
  rendered <- liftIO $ renderMainPart body
  let quoted = unlines $ map ("> " ++) $ lines $ wordwrap 80 rendered
  (headers, body) <- throwEither "Invalid message" $ liftIO $ composeWith initialHeaders quoted
  liftMaildir $ unless nosend $ do
    sendMessage headers body
    setFlag 'R' mid
  msg <- renderMessageS headers body
  liftIO $ putStrLn msg
  -}

replyHeaders :: Maybe NameAddr -> ReplyStrategy -> [Field] -> [Field]
replyHeaders myaddr strat headers =
  let from    = mconcat (lookupField fFrom headers)
      replyto = mconcat (lookupField fReplyTo headers)
      to      = mconcat (lookupField fTo headers)
      cc      = mconcat (lookupField fCc headers)
      subject = listToMaybe (lookupField fSubject headers)
      msgids  = lookupField fMessageID headers
      refs     = mconcat (lookupField fReferences headers)
      from'   = case replyto of
                  [] -> from
                  xs -> xs
      to'     = case strat of
                  GroupReply -> from' ++ to
                  SingleReply -> from'
      cc'     = case strat of
                  GroupReply -> Just cc
                  SingleReply -> Nothing
      subject' = fromMaybe "Re:" $ ("Re: " ++) <$> subject
  in catMaybes [ Just (mkField fTo to')
               , mkField fCc <$> cc'
               , Just (mkField fInReplyTo msgids)
               , Just (mkField fSubject subject')
               , Just (mkField fReferences (msgids ++ refs))
               , mkField fFrom <$> pure <$> myaddr
               ]

composeWith :: [Field] -> String -> IO (Either String ([Field], T.Text))
composeWith headers text = do
  tempdir <- getTemporaryDirectory
  (tempf, temph) <- openTempFile tempdir "message"
  msg <- runExceptT $ renderCompose headers text
  case msg of
    Left err -> return (Left err)
    Right msg -> do
      hPutStrLn temph msg
      hClose temph
      editFile tempf
      parseFile tempf $ do
        headers <- parseComposedHeaders
        body <- parseComposedMessage
        if T.null body
           then fail "Empty message"
           else return (headers, body)

cmdHeaders :: Limit -> StoreM FilterExp -> StoreM ()
cmdHeaders limit filter' = do
  filter <- filter'
  messages <- queryStore (filterBy filter limit)
  liftIO $ case limit of
    Nothing -> mapM_ (uncurry printMessageSingle) messages
    Just l  -> mapM_ (uncurry printMessageSingle) (drop (length messages - l) messages)

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

editFile :: FilePath -> IO ()
editFile fp = do
  editor <- fromMaybe "vim" <$> lookupEnv "EDITOR"
  callCommand (editor ++ " " ++ fp)

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
