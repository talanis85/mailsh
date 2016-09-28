module Main where

import Control.Monad
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
import Mailsh.Printer
import Mailsh.Message

data Options = Options
  { optCommand :: MaildirM ()
  }

options :: Parser Options
options = Options <$> subparser
  (  command "read"     (info (cmdRead    <$> argument auto (metavar "MID")
                                          <*> printerOption
                                          <*> pure defaultPrinterOptions) idm)
  <> command "compose"  (info (cmdCompose <$> argument str (metavar "RECIPIENT")) idm)
  <> command "reply"    (info (cmdReply   <$> flag SingleReply GroupReply (long "group")
                                          <*> argument auto (metavar "MID")) idm)
  <> command "headers"  (info (cmdHeaders <$> argument (eitherReader parseFilterExp)
                                                       (metavar "FILTER" <> value filterUnseen))
                                                       idm)
  )
    where
      printerOption = option printerReader (   short 'p'
                                            <> long "printer"
                                            <> metavar "PRINTER"
                                            <> value (Printer simplePrinter)
                                           )
      printerReader = eitherReader $ \s -> case s of
        "headers-only" -> Right (Printer headersOnlyPrinter)
        "passthrough"  -> Right (Printer utf8Printer)
        "simple"       -> Right (Printer simplePrinter)
        _              -> Left "Invalid printer"

cmdRead :: MessageNumber -> Printer -> PrinterOptions -> MaildirM ()
cmdRead msg printer propts = do
  messages <- listMaildir
  if length messages <= msg
     then liftIO $ printf "No such message.\n"
     else do
       fp <- absoluteMaildirFile (messages !! msg)
       outputWithPrinter printer propts fp

cmdCompose :: Recipient -> MaildirM ()
cmdCompose rcpt = liftIO $ printf "TODO: Compose mail to %s\n" rcpt

cmdReply :: ReplyStrategy -> MessageNumber -> MaildirM ()
cmdReply strat mid = liftIO $ printf "TODO: Reply to message %d with %s\n" mid (show strat)

cmdHeaders :: FilterExp -> MaildirM ()
cmdHeaders filter = do
  messages <- zip ([0..] :: [Int]) <$> listMaildir
  filtered <- filterM (runFilter filter . snd) messages
  mapM_ printMessageHeader filtered
    where
      printMessageHeader (n, mid) = do
        fp <- absoluteMaildirFile mid
        headers <- liftIO $ getMessageFileHeaders fp
        liftIO $ printf "%04d: %s\n" n (formatHeaderLine headers)
      formatHeaderLine :: [Field] -> String
      formatHeaderLine hs =
        let date    = fromMaybe "" (formatCalendarTime defaultTimeLocale "%d.%m.%Y %H:%M"
                                   <$> listToMaybe (lookupField fDate hs))
            subject = fromMaybe "" (listToMaybe (lookupField fSubject hs))
            from    = fromMaybe "" (formatNameAddr
                                   <$> listToMaybe (mconcat (lookupField fFrom hs)))
        in printf "%s\n      %s\n      %s" date from (take 50 subject)

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
