module Compose
  ( ReplyStrategy (..)
  , composeWith
  , replyHeaders
  ) where

import Control.Monad.Except
import Data.Maybe
import System.Directory
import System.Environment
import System.Process
import System.IO

import Data.Text (Text)
import qualified Data.Text as T

import Mailsh.Compose
import Mailsh.Parse
import Mailsh.Store
import Network.Email

data ReplyStrategy = SingleReply | GroupReply
  deriving (Show)

editFile :: FilePath -> IO ()
editFile fp = do
  editor <- fromMaybe "vim" <$> lookupEnv "EDITOR"
  callCommand (editor ++ " " ++ fp)

composeWith :: [Field] -> String -> IO (Either String ([Field], Text))
composeWith headers text = do
  tempdir <- getTemporaryDirectory
  (tempf, temph) <- openTempFile tempdir "message"
  msg <- runExceptT $ renderCompose headers text
  case msg of
    Left err -> return (Left err)
    Right msg -> do
      hPutStr temph msg
      hClose temph
      editFile tempf
      msg' <- readFile tempf
      if msg == msg'
      then return (Left "Unchanged message")
      else return $ flip parseString msg' $ do
        headers <- parseComposedHeaders
        body <- parseComposedMessage
        if T.null body
           then fail "Empty message"
           else return (headers, body)

replyHeaders :: Maybe NameAddr -> ReplyStrategy -> Message -> [Field]
replyHeaders myaddr strat msg =
  let from'   = case messageReplyTo msg of
                  [] -> messageFrom msg
                  xs -> xs
      to      = case strat of
                  GroupReply -> from' ++ messageTo msg
                  SingleReply -> from'
      cc      = case strat of
                  GroupReply -> Just (messageCc msg)
                  SingleReply -> Nothing
      subject  = "Re: " ++ messageSubject msg
  in catMaybes [ Just (mkField fTo to)
               , mkField fCc <$> cc
               , Just (mkField fInReplyTo [messageMessageId msg])
               , Just (mkField fSubject subject)
               , Just (mkField fReferences (messageMessageId msg : messageReferences msg))
               , mkField fFrom <$> pure <$> myaddr
               ]
