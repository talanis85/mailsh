module Main where

import Control.Monad
import Control.Monad.Trans
import Options.Applicative
import System.Environment
import System.IO
import Text.Printf

import Mailsh.Types
import Mailsh.Filter
import Mailsh.Maildir
import Mailsh.Printer

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
                                            <> value (Printer utf8Printer)
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
  messages <- zip [0..] <$> listMaildir
  filtered <- filterM (runFilter filter . snd) messages
  mapM_ (liftIO . putStrLn . show) filtered

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
