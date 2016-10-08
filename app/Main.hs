module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Data.Maybe
import Options.Applicative
import System.Console.Terminal.Size
import System.Directory
import System.Environment
import System.IO
import System.Time
import System.Locale
import Text.Printf

import Mailsh.Types
import Mailsh.Filter
import Mailsh.Maildir
import Mailsh.MessageNumber
import Mailsh.Printer
import Mailsh.Parse

import Network.Email

type Limit = Int
type MessageNumber' = MaildirM MessageNumber

data Options = Options
  { optCommand :: MaildirM ()
  }

options :: Parser Options
options = Options <$> subparser
  (  command "read"     (info (cmdRead    <$> msgArgument
                                          <*> printerOption
                                          <*> pure defaultPrinterOptions)
                              idm)
  <> command "compose"  (info (cmdCompose <$> argument str (metavar "RECIPIENT")) idm)
  <> command "reply"    (info (cmdReply   <$> flag SingleReply GroupReply (long "group")
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
  )
    where
      printerOption = option printerReader (   short 'p'
                                            <> long "printer"
                                            <> metavar "PRINTER"
                                            <> value (Printer simplePrinter)
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

getRecentMessageNumber :: MessageNumber'
getRecentMessageNumber = read <$> liftIO (readHomeFile ".recentmessage")
  where
    readHomeFile f = do
      d <- getHomeDirectory
      readFile (d ++ "/" ++ f)

setRecentMessageNumber :: MessageNumber -> MessageNumber'
setRecentMessageNumber n = liftIO (writeHomeFile ".recentmessage" (show n)) >> return n
  where
    writeHomeFile f s = do
      d <- getHomeDirectory
      writeFile (d ++ "/" ++ f) s

cmdRead :: MessageNumber' -> Printer -> PrinterOptions -> MaildirM ()
cmdRead msg' printer propts = do
  msg <- msg'
  mid <- getMID msg
  fp <- absoluteMaildirFile mid
  outputWithPrinter printer propts fp

cmdCompose :: Recipient -> MaildirM ()
cmdCompose rcpt = liftIO $ printf "TODO: Compose mail to %s\n" rcpt

cmdReply :: ReplyStrategy -> MessageNumber' -> MaildirM ()
cmdReply strat msg' = do
  msg <- msg'
  liftIO $ printf "TODO: Reply to message %s with %s\n" (show msg) (show strat)

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
  headers <- liftIO $ parseFile fp parseHeaders
  flags <- getFlags mid
  case headers of
    Nothing -> liftIO $ printf "%6s: --- Error parsing headers ---" (show n)
    Just headers -> liftIO $ f n flags headers

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
  headers <- liftIO $ parseFile fp parseHeaders
  case headers of
    Nothing -> throwError "Error parsing headers."
    Just headers -> return headers

getMID :: MessageNumber -> MaildirM MID
getMID msg = do
  messages <- listMaildir
  case lookupMessage msg messages of
    Nothing -> throwError "No such message."
    Just mid -> return mid

main :: IO ()
main = do
  opts <- execParser (info options idm)
  maildirpath <- lookupEnv "MAILDIR"
  case maildirpath of
    Nothing -> printf "Error: No maildir set\n"
    Just maildirpath -> do
      result <- withMaildirPath (optCommand opts) maildirpath
      case result of
        Left err -> printf "Error: %s\n" err
        Right () -> return ()
