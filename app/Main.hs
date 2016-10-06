module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Data.Maybe
import Options.Applicative
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

data Options = Options
  { optCommand :: MaildirM ()
  }

options :: Parser Options
options = Options <$> subparser
  (  command "read"     (info (cmdRead    <$> argument auto (metavar "MID")
                                          <*> printerOption
                                          <*> pure defaultPrinterOptions)
                              idm)
  <> command "compose"  (info (cmdCompose <$> argument str (metavar "RECIPIENT")) idm)
  <> command "reply"    (info (cmdReply   <$> flag SingleReply GroupReply (long "group")
                                          <*> argument auto (metavar "MID")) idm)
  <> command "headers"  (info (cmdHeaders <$> option (Just <$> auto)
                                                     (short 'l' <> metavar "LIMIT" <> value Nothing)
                                          <*> argument (eitherReader parseFilterExp)
                                                       (metavar "FILTER" <> value filterUnseen))
                              idm)
  <> command "trash"    (info (cmdTrash   <$> argument auto (metavar "MID")) idm)
  <> command "recover"  (info (cmdRecover <$> argument auto (metavar "MID")) idm)
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

cmdRead :: MessageNumber -> Printer -> PrinterOptions -> MaildirM ()
cmdRead msg printer propts = do
  mid <- getMID msg
  fp <- absoluteMaildirFile mid
  outputWithPrinter printer propts fp

cmdCompose :: Recipient -> MaildirM ()
cmdCompose rcpt = liftIO $ printf "TODO: Compose mail to %s\n" rcpt

cmdReply :: ReplyStrategy -> MessageNumber -> MaildirM ()
cmdReply strat msg = liftIO $ printf "TODO: Reply to message %s with %s\n" (show msg) (show strat)

cmdHeaders :: Maybe Limit -> FilterExp -> MaildirM ()
cmdHeaders limit filter = do
  messages <- checksumListing <$> listMaildir
  filtered <- filterM (runFilter filter . snd) messages
  case limit of
    Nothing -> mapM_ (uncurry printMessageOverview) filtered
    Just l  -> mapM_ (uncurry printMessageOverview) (drop (length filtered - l) filtered)

cmdTrash :: MessageNumber -> MaildirM ()
cmdTrash msg = do
  mid <- getMID msg
  setFlag 'T' mid
  headers <- getHeaders mid
  liftIO $ printf "Trashed message.\n"
  printMessageOverview msg mid

cmdRecover :: MessageNumber -> MaildirM ()
cmdRecover msg = do
  mid <- getMID msg
  unsetFlag 'T' mid
  liftIO $ printf "Recovered message.\n"
  printMessageOverview msg mid

printMessageOverview :: MessageNumber -> MID -> MaildirM ()
printMessageOverview n mid = do
  fp <- absoluteMaildirFile mid
  headers <- liftIO $ parseFile fp parseHeaders
  case headers of
    Nothing -> liftIO $ printf "%6s: --- Error parsing headers ---" (show n)
    Just headers -> liftIO $ printf "%6s: %s\n" (show n) (formatHeaderLine headers)
  where
    formatHeaderLine :: [Field] -> String
    formatHeaderLine hs =
      let date    = fromMaybe "" (formatCalendarTime defaultTimeLocale "%d.%m.%Y %H:%M"
                                 <$> listToMaybe (lookupField fDate hs))
          subject = fromMaybe "" (listToMaybe (lookupField fSubject hs))
          from    = fromMaybe "" (formatNameAddr
                                 <$> listToMaybe (mconcat (lookupField fFrom hs)))
      in printf "%s\n        %s\n        %s" date from (take 50 subject)

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
