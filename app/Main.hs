module Main where

import Control.Monad
import Options.Applicative
import Text.Printf

import Mailsh.Types
import Mailsh.Filter

data Options = Options
  { optCommand :: IO ()
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

cmdRead :: MID -> IO ()
cmdRead mid = printf "TODO: Read mail %d\n"

cmdCompose :: Recipient -> IO ()
cmdCompose rcpt = printf "TODO: Compose mail to %s\n"

cmdReply :: ReplyStrategy -> MID -> IO ()
cmdReply strat mid = printf "TODO: Reply to message %d with %s\n" mid (show strat)

cmdHeaders :: FilterExp -> IO ()
cmdHeaders filter = printf "TODO: Show headers with filter '%s'\n" (show filter)

main :: IO ()
main = do
  opts <- execParser (info options idm)
  optCommand opts
