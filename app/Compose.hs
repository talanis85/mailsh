{-# LANGUAGE OverloadedStrings #-}
module Compose
  ( ReplyStrategy (..)
  , composeWith
  , replyHeaders
  ) where

import Data.Attoparsec.ByteString
import Data.Maybe
import Data.Monoid ((<>))
import System.Directory
import System.Environment
import System.Process
import System.IO

import qualified Data.Text.IO as T

import qualified Data.ByteString as B

import Mailsh.Compose
import Mailsh.Fields
import Mailsh.Message

data ReplyStrategy = SingleReply | GroupReply
  deriving (Show)

editFile :: FilePath -> IO ()
editFile fp = do
  editor <- fromMaybe "vim" <$> lookupEnv "EDITOR"
  callCommand (editor ++ " " ++ fp)

composeWith :: ComposedMessage -> IO (Either String ComposedMessage)
composeWith cmsg = do
  tempdir <- getTemporaryDirectory
  (tempf, temph) <- openTempFile tempdir "message"
  let msg = renderComposedMessage cmsg
  T.hPutStr temph msg
  hClose temph
  editFile tempf
  msg' <- B.readFile tempf
  return $ parseOnly composedMessageP msg'

listToMaybeList :: [a] -> Maybe [a]
listToMaybeList [] = Nothing
listToMaybeList xs = Just xs

replyHeaders :: ReplyStrategy -> DigestMessage -> [Field]
replyHeaders strat msg =
  let from'   = case messageReplyTo msg of
                  [] -> messageFrom msg
                  xs -> xs
      to      = case strat of
                  GroupReply -> from' ++ messageTo msg
                  SingleReply -> from'
      cc      = case strat of
                  GroupReply -> listToMaybeList (messageCc msg)
                  SingleReply -> Nothing
      subject  = "Re: " <> messageSubject msg
  in catMaybes [ Just (mkField fTo to)
               , mkField fCc <$> cc
               , Just (mkField fInReplyTo [messageMessageId msg])
               , Just (mkField fSubject subject)
               , Just (mkField fReferences (messageMessageId msg : messageReferences msg))
               ]
