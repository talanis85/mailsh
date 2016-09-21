module Main where

import Control.Monad
import Control.Monad.Trans
import Options.Applicative
import System.Environment
import Text.Printf

import Mailsh.Types
import Mailsh.Filter
import Mailsh.Operations

data Options = Options
  { optCommand :: Mailsh ()
  }

options :: Parser Options
options = Options <$> subparser
  (  command "read"     (info (cmdRead    <$> argument auto (metavar "MID")) idm)
  <> command "compose"  (info (cmdCompose <$> argument str (metavar "RECIPIENT")) idm)
  <> command "reply"    (info (cmdReply   <$> flag SingleReply GroupReply (long "group")
                                          <*> argument auto (metavar "MID")) idm)
  <> command "headers"  (info (cmdHeaders <$> argument (eitherReader parseFilterExp)
                                                       (metavar "FILTER" <> value filterAll))
                                                       idm)
  )

cmdRead :: MID -> Mailsh ()
cmdRead mid = do
  messages <- mailshList
  if length messages <= mid
     then liftIO $ printf "No such message.\n"
     else mailshGetContents (messages !! mid) >>= liftIO . putStrLn

cmdCompose :: Recipient -> Mailsh ()
cmdCompose rcpt = liftIO $ printf "TODO: Compose mail to %s\n" rcpt

cmdReply :: ReplyStrategy -> MID -> Mailsh ()
cmdReply strat mid = liftIO $ printf "TODO: Reply to message %d with %s\n" mid (show strat)

cmdHeaders :: FilterExp -> Mailsh ()
cmdHeaders filter = liftIO $ printf "TODO: Show headers with filter '%s'\n" (show filter)

main :: IO ()
main = do
  opts <- execParser (info options idm)
  maildirpath <- lookupEnv "MAILDIR"
  case maildirpath of
    Nothing -> printf "Error: No maildir set\n"
    Just maildirpath -> do
      runMailshWithMaildir (optCommand opts) maildirpath
