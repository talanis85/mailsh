module MessageRef
  ( MessageNumber'
  , MessageRef (..)
  , messageRefReader
  , getRecentMessageNumber
  , setRecentMessageNumber
  , getNextMessageNumber
  ) where

import Control.Monad.Trans
import Data.Maybe
import Options.Applicative hiding ((<|>))
import Text.Parsec
import System.FilePath

import Mailsh.Maildir
import Mailsh.Store

type MessageNumber' = StoreM MessageNumber
data MessageRef = MessageRefNumber MessageNumber'
                | MessageRefPath FilePath
                | MessageRefStdin
                | MessageRefPart Int MessageRef

maildirFile :: String -> MaildirM FilePath
maildirFile f = do
  p <- getMaildirPath
  return (p </> f)

getRecentMessageNumber :: StoreM MessageNumber
getRecentMessageNumber = read <$> (liftMaildir (maildirFile ".recentmessage") >>= liftIO . readFile)

setRecentMessageNumber :: MessageNumber -> StoreM MessageNumber
setRecentMessageNumber n = (liftMaildir (maildirFile ".recentmessage") >>= liftIO . flip writeFile (show n))
                           >> return n

getNextMessageNumber :: StoreM MessageNumber
getNextMessageNumber = do
  messages <- resultRows <$> queryStore (filterBy filterUnseen Nothing)
  setRecentMessageNumber $ fromMaybe (messageNumber 0) $ listToMaybe $ map fst messages

messageRefReader :: ReadM MessageRef
messageRefReader = eitherReader $ \x -> case parse messageRefParser "" x of
  Left err -> Left (show err)
  Right v  -> Right v

messageRefParser :: Parsec String u MessageRef
messageRefParser =
      (MessageRefPart <$> partNumber <*> messageRefParser)
  <|> (stdinToken >> return MessageRefStdin)
  <|> (currentToken >> return (MessageRefNumber getRecentMessageNumber))
  <|> (MessageRefNumber . setRecentMessageNumber <$> anyNumber)
  <|> (MessageRefPath <$> anyString)
  where
    anyNumber = read <$> many1 digit
    anyString = many1 anyChar
    stdinToken = char '-'
    currentToken = char '_'
    partNumber = do
      n <- read <$> many1 digit
      char '#'
      return n
