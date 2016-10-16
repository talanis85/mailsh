{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Data.Maybe
import Data.String.WordWrap
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
import Mailsh.Printer
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
                                          <*> printerOption (Printer simplePrinter)
                                          <*> pure defaultPrinterOptions)
                              idm)
  <> command "cat"      (info (cmdCat     <$> msgArgument
                                          <*> printerOption (Printer utf8Printer)
                                          <*> pure defaultPrinterOptions)
                              idm)
  <> command "next"     (info (cmdRead    <$> pure getNextMessageNumber
                                          <*> printerOption (Printer headersOnlyPrinter)
                                          <*> pure defaultPrinterOptions)
                              idm)
  <> command "compose"  (info (cmdCompose <$> flag False True (long "nosend")
                                          <*> argument str (metavar "RECIPIENT")) idm)
  <> command "reply"    (info (cmdReply   <$> flag False True (long "nosend")
                                          <*> flag SingleReply GroupReply (long "group")
                                          <*> msgArgument) idm)
  <> command "headers"  (info (cmdHeaders <$> maybeOption auto (short 'l' <> metavar "LIMIT")
                                          <*> argument (eitherReader parseFilterExp)
                                                       (metavar "FILTER" <> value filterUnseen))
                              idm)
  <> command "trash"    (info (cmdTrash   <$> msgArgument) idm)
  <> command "recover"  (info (cmdRecover <$> msgArgument) idm)
  <> command "unread"   (info (cmdUnread  <$> msgArgument) idm)
  <> command "flag"     (info (cmdFlag    <$> msgArgument) idm)
  <> command "unflag"   (info (cmdUnflag  <$> msgArgument) idm)
  ) <|> (cmdHeaders <$> maybeOption auto (short 'l' <> metavar "LIMIT")
                    <*> argument (eitherReader parseFilterExp)
                                 (metavar "FILTER" <> value filterUnseen))
    where
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

throwEither :: (MonadError String m) => String -> m (Either String a) -> m a
throwEither s m = do
  r <- m
  case r of
    Left e -> throwError (s ++ ": " ++ show e)
    Right v -> return v

cmdRead :: MessageNumber' -> Printer -> PrinterOptions -> MaildirM ()
cmdRead msg' printer propts = do
  msg <- msg'
  mid <- getMID msg
  fp <- absoluteMaildirFile mid
  outputWithPrinter printer propts fp
  setFlag 'S' mid

cmdCat :: MessageNumber' -> Printer -> PrinterOptions -> MaildirM ()
cmdCat msg' printer propts = do
  msg <- msg'
  mid <- getMID msg
  fp <- absoluteMaildirFile mid
  outputWithPrinter printer propts fp

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
  fp <- absoluteMaildirFile mid
  (headers, body) <- throwEither "Invalid message" $ liftIO $ parseCrlfFile fp $ do
    h <- parseHeaders
    b <- parseMessage mimeTextPlain h
    return (h, b)
  from <- do
    x <- liftIO (lookupEnv "MAILFROM")
    return (x >>= parseString parseNameAddr)
  let initialHeaders = (catMaybes
        [ mkField fFrom <$> return <$> from
        ])
        ++ replyHeaders strat headers
  rendered <- liftIO $ uncurry renderSimple (bodiesOf ["text/plain", "text/html"] body !! 0)
  let quoted = unlines $ map ("> " ++) $ lines $ wordwrap 80 rendered
  (headers, body) <- throwEither "Invalid message" $ liftIO $ composeWith initialHeaders quoted
  unless nosend $ sendMessage headers body
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

composeWith :: [Field] -> String -> IO (Either String ([Field], Body))
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
        case body of
          BodyLeaf _ b -> case filter (`notElem` "\n\r\t ") b of
                            [] -> fail "Empty message"
                            _  -> return (headers, body)
          _            -> return (headers, body)

cmdHeaders :: Maybe Limit -> FilterExp -> MaildirM ()
cmdHeaders limit filter = do
  messages <- checksumListing <$> listMaildir
  filtered <- filterM (runFilter filter . snd) messages
  case limit of
    Nothing -> mapM_ (uncurry printMessageSingle) filtered
    Just l  -> mapM_ (uncurry printMessageSingle) (drop (length filtered - l) filtered)

cmdTrash :: MessageNumber' -> MaildirM ()
cmdTrash = modifyMessage (setFlag 'T') "Trashed message."

cmdRecover :: MessageNumber' -> MaildirM ()
cmdRecover = modifyMessage (unsetFlag 'T') "Recovered message."

cmdUnread :: MessageNumber' -> MaildirM ()
cmdUnread = modifyMessage (unsetFlag 'S') "Marked message as unread."

cmdFlag :: MessageNumber' -> MaildirM ()
cmdFlag = modifyMessage (setFlag 'F') "Flagged message."

cmdUnflag :: MessageNumber' -> MaildirM ()
cmdUnflag = modifyMessage (unsetFlag 'F') "Unflagged message."

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
  fp <- absoluteMaildirFile mid
  headers <- liftIO $ parseCrlfFile fp parseHeaders
  flags <- getFlags mid
  case headers of
    Left err -> liftIO $ printf "%6s: --- Error parsing headers ---" (show n)
    Right headers -> liftIO $ f n flags headers

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
getHeaders mid = do
  fp <- absoluteMaildirFile mid
  throwEither "Error parsing headers" $ liftIO $ parseCrlfFile fp parseHeaders

getMID :: MessageNumber -> MaildirM MID
getMID msg = do
  messages <- listMaildir
  case lookupMessage msg messages of
    Nothing -> throwError "No such message."
    Just mid -> return mid

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
