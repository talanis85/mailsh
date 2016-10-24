{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Data.Maybe
import Data.String.WordWrap
import qualified Data.Text as T
import Development.GitRev
import Options.Applicative
import System.Console.Terminal.Size
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Time
import System.Locale
import System.Process
import Text.Printf

import Mailsh.Compose
import Mailsh.Types
import Mailsh.Filter
import Mailsh.Maildir
import Mailsh.MessageNumber
import Mailsh.Parse
import Mailsh.Render

import Network.Email

type Limit = Int
type MessageNumber' = MaildirM MessageNumber

data Options = Options
  { optCommand :: MaildirM ()
  }

options :: ParserInfo Options
options = Options <$> info (helper <*> commandP)
  (  fullDesc
  <> progDesc "Manage maildirs from the shell"
  <> header "mailsh - A console maildir tool"
  <> footer ("Version: " ++ version)
  )

version :: String
version = $(gitBranch) ++ "@" ++ $(gitHash)

commandP :: Parser (MaildirM ())
commandP = subparser
  (  command "read"     (info (cmdRead    <$> msgArgument
                                          <*> rendererOption) idm)
  <> command "cat"      (info (cmdCat     <$> msgArgument
                                          <*> maybeOption auto (short 'p' <> metavar "PART")) idm)
  <> command "next"     (info (cmdRead    <$> pure getNextMessageNumber
                                          <*> rendererOption) idm)
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
  <> command "recache"  (info (pure cmdRecache) idm)
  ) <|> (cmdHeaders <$> maybeOption auto (short 'l' <> metavar "LIMIT")
                    <*> argument (eitherReader parseFilterExp)
                                 (metavar "FILTER" <> value (return filterUnseen)))
    where
      rendererOption = option rendererReader (   short 'r'
                                              <> long "render"
                                              <> metavar "RENDERER"
                                              <> value renderDefault
                                             )
      rendererReader = eitherReader $ \s -> case s of
        "default" -> Right renderDefault
        "outline" -> Right renderOutline
        _         -> Left "Invalid renderer"
      {-
      printerOption def =
        option printerReader (   short 'p'
                              <> long "printer"
                              <> metavar "PRINTER"
                              <> value def
                             )
      printerReader = eitherReader $ \s -> case s of
        "headers"      -> Right (Printer headersOnlyPrinter)
        "raw"          -> Right (Printer utf8Printer)
        "default"      -> Right (Printer simplePrinter)
        _              -> Left "Invalid printer"
      -}

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
getRecentMessageNumber = read <$> (maildirFile ".recentmessage" >>= liftIO . readFile)

setRecentMessageNumber :: MessageNumber -> MessageNumber'
setRecentMessageNumber n = (maildirFile ".recentmessage" >>= liftIO . flip writeFile (show n))
                           >> return n

getNextMessageNumber :: MessageNumber'
getNextMessageNumber = do
  messages <- checksumListing <$> listMaildir
  filtered <- filterM (runFilter filterUnseen . snd) messages
  setRecentMessageNumber $ fromMaybe invalidMessageNumber $ listToMaybe $ map fst filtered

defaultHeaders :: [IsField]
defaultHeaders =
  [ IsField fDate, IsField fTo, IsField fFrom
  , IsField fCc, IsField fBcc, IsField fReplyTo
  , IsField fSubject
  ]

throwEither :: (MonadError String m) => String -> m (Either String a) -> m a
throwEither s m = do
  r <- m
  case r of
    Left e -> throwError (s ++ ": " ++ show e)
    Right v -> return v

cmdRead :: MessageNumber' -> Renderer -> MaildirM ()
cmdRead msg' renderer = do
  msg <- msg'
  mid <- getMID msg
  (headers, body) <- parseMaildirFile mid $ do
    h <- parseHeaders
    b <- parseMessage (mimeTextPlain "utf8") h
    return (h, b)

  let refs = concat $ lookupField fReferences headers
      refFilter = foldr (filterOr . filterMessageID) filterNone refs
  refmsgs <- (checksumListing <$> listMaildir) >>= filterM (runFilter refFilter . snd)

  liftIO $ putStrLn $ formatHeaders defaultHeaders headers
  liftIO $ renderer body >>= putStrLn

  when (not (null refmsgs)) $ do
    liftIO $ printf "References:\n"
    mapM_ (uncurry printMessageSingle) refmsgs

  setFlag 'S' mid

cmdCat :: MessageNumber' -> Maybe Int -> MaildirM ()
cmdCat msg' part = do
  msg <- msg'
  mid <- getMID msg
  case part of
    Nothing -> do
      fp <- absoluteMaildirFile mid
      liftIO $ readFile fp >>= putStrLn
    Just part -> do
      (headers, body) <- parseMaildirFile mid $ do
        h <- parseHeaders
        b <- parseMessage (mimeTextPlain "utf8") h
        return (h, b)
      liftIO $ outputPart part body

cmdCompose :: Bool -> Recipient -> MaildirM ()
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

cmdReply :: Bool -> ReplyStrategy -> MessageNumber' -> MaildirM ()
cmdReply nosend strat msg' = do
  msg <- msg'
  mid <- getMID msg
  (headers, body) <- parseMaildirFile mid $ do
    h <- parseHeaders
    m <- parseMessage (mimeTextPlain "utf8") h
    return (h, m)
  from <- do
    x <- liftIO (lookupEnv "MAILFROM")
    return (x >>= parseString parseNameAddr)
  let initialHeaders = catMaybes
        [ mkField fFrom <$> return <$> from
        ]
        ++ replyHeaders strat headers
  rendered <- liftIO $ renderMainPart body
  let quoted = unlines $ map ("> " ++) $ lines $ wordwrap 80 rendered
  (headers, body) <- throwEither "Invalid message" $ liftIO $ composeWith initialHeaders quoted
  unless nosend $ do
    sendMessage headers body
    setFlag 'R' mid
  msg <- renderMessageS headers body
  liftIO $ putStrLn msg

replyHeaders :: ReplyStrategy -> [Field] -> [Field]
replyHeaders strat headers =
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

cmdHeaders :: Maybe Limit -> MaildirM FilterExp -> MaildirM ()
cmdHeaders limit filter' = do
  messages <- checksumListing <$> listMaildir
  filter <- filter'
  filtered <- filterM (runFilter filter . snd) messages
  case limit of
    Nothing -> mapM_ (uncurry printMessageSingle) filtered
    Just l  -> mapM_ (uncurry printMessageSingle) (drop (length filtered - l) filtered)

cmdTrash :: MessageNumber' -> MaildirM ()
cmdTrash = modifyMessage (setFlag 'T') "Trashed message."

cmdRecover :: MessageNumber' -> MaildirM ()
cmdRecover = modifyMessage (unsetFlag 'T') "Recovered message."

cmdPurge :: MaildirM ()
cmdPurge = do
  n <- purgeMaildir
  liftIO $ printf "Purged %d messages.\n" n

cmdUnread :: MessageNumber' -> MaildirM ()
cmdUnread = modifyMessage (unsetFlag 'S') "Marked message as unread."

cmdFlag :: MessageNumber' -> MaildirM ()
cmdFlag = modifyMessage (setFlag 'F') "Flagged message."

cmdUnflag :: MessageNumber' -> MaildirM ()
cmdUnflag = modifyMessage (unsetFlag 'F') "Unflagged message."

cmdRecache :: MaildirM ()
cmdRecache = do
  liftIO $ printf "Rebuilding cache.\n"
  rebuildMaildirCache
  liftIO $ printf "Done.\n"

modifyMessage :: (MID -> MaildirM ()) -> String -> MessageNumber' -> MaildirM ()
modifyMessage f notice msg' = do
  msg <- msg'
  mid <- getMID msg
  f mid
  liftIO $ putStrLn notice
  printMessageSingle msg mid

printMessageSingle :: MessageNumber -> MID -> MaildirM ()
printMessageSingle = printMessageWith $
  \msg flags hs -> printWithWidth (formatMessageSingle msg flags hs)

printMessageWith :: (MessageNumber -> String -> [Field] -> IO ())
                 -> MessageNumber -> MID -> MaildirM ()
printMessageWith f n mid = do
  headers <- parseMaildirFile mid parseHeaders
  flags <- getFlags mid
  liftIO $ f n flags headers

printWithWidth :: (Int -> String) -> IO ()
printWithWidth f = do
  (Window _ w) <- fromMaybe (Window 80 80) <$> size
  putStrLn (f w)

formatMessageSingle :: MessageNumber -> String -> [Field] -> Int -> String
formatMessageSingle msg flags hs width = printf "%c %5s %18s %16s %s"
                                         (flagSummary flags)
                                         (show msg)
                                         (take 18 from)
                                         (take 16 date)
                                         (take (width - (1 + 5 + 18 + 16 + 5)) subject)
  where
    from = fromMaybe "" (formatNameAddrShort <$> listToMaybe (mconcat (lookupField fFrom hs)))
    subject = fromMaybe "" (listToMaybe (lookupField fSubject hs))
    date = fromMaybe "" (formatCalendarTime defaultTimeLocale "%a %b %d %H:%M"
                         <$> listToMaybe (lookupField fDate hs))

flagSummary :: String -> Char
flagSummary flags
  | 'T' `elem` flags                        = 'T'
  | 'F' `elem` flags                        = 'F'
  | 'R' `elem` flags                        = 'R'
  | 'S' `elem` flags && 'T' `notElem` flags = 'O'
  | otherwise                               = 'N'

getHeaders :: MID -> MaildirM [Field]
getHeaders mid = parseMaildirFile mid parseHeaders

getMID :: MessageNumber -> MaildirM MID
getMID msg = do
  messages <- listMaildir
  case lookupMessage msg messages of
    Nothing -> throwError "No such message."
    Just mid -> return mid

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
  result <- withMaildirPath (optCommand opts) maildirpath
  case result of
    Left err -> printf "Error: %s\n" err
    Right () -> return ()
