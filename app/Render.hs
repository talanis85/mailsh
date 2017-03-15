{-# LANGUAGE FlexibleContexts #-}
module Render
  ( Renderer
  , fullRenderer
  , previewRenderer
  , noquoteRenderer
  , outlineRenderer
  , printMessageSingle
  , outputPart
  ) where

import Control.Monad.Trans
import Data.List
import Data.Maybe
import Data.String.WordWrap
import Data.Time.Format
import Network.Email
import System.Console.Terminal.Size
import Text.Parsec
import Text.Printf
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS

import Mailsh.MimeRender
import Mailsh.Store

type Renderer = Message -> StoreM ()

flagSummary :: String -> Char
flagSummary flags
  | 'T' `elem` flags                        = 'T'
  | 'F' `elem` flags                        = 'F'
  | 'R' `elem` flags                        = 'R'
  | 'S' `elem` flags && 'T' `notElem` flags = 'O'
  | otherwise                               = 'N'

outputPart :: Int -> PartTree -> IO ()
outputPart n b =
  let typeFilter = const True
      parts      = partList b
  in case parts !! (n-1) of
       PartText t s   -> T.putStrLn s
       PartBinary t s -> BS.putStr s

removeBlankLines :: String -> String
removeBlankLines = unlines . fst . foldr removeBlankLines' ([], 0) . lines
  where
    removeBlankLines' x (xs, n) = case x of
                                    "" -> if n >= 2
                                             then (xs, n + 1)
                                             else ("" : xs, n + 1)
                                    _  -> (x : xs, 0)

fullRenderer :: Renderer
fullRenderer = messageRenderer (removeBlankLines . wordwrap 80)

previewRenderer :: Renderer
previewRenderer = messageRenderer (wordwrap 80 . unlines . take 10 . lines)

noquoteRenderer :: Renderer
noquoteRenderer = messageRenderer (removeBlankLines . wordwrap 80 . parseFilter noquoteParser)

parseFilter p s = case parse p "<message>" s of
                    Left _   -> s
                    Right s' -> s'

noquoteParser :: Parsec String u String
noquoteParser = unlines <$> many (quotes <|> line)
  where
    quotes = do
      char '>'
      many (noneOf "\n")
      newline
      quotes <|> return "<snip>"
    line = many (noneOf "\n") <* newline

messageRenderer :: (String -> String) -> Renderer
messageRenderer flt msg = do
  liftIO $ withWidth (displayMessage msg)
  refs <- concat <$> mapM (queryStore . flip filterBy Nothing . filterMessageId) (messageReferences msg)
  liftIO $ displayReferences refs
  where
    displayMessage :: Message -> Int -> IO ()
    displayMessage msg width = do
      putStrLn (replicate width '-')
      headerName "From" >> addressHeader (messageFrom msg)
      headerName "To" >> addressHeader (messageTo msg)
      headerName "Subject" >> textHeader (messageSubject msg)
      headerName "Date" >> dateHeader (messageDate msg)
      putStrLn (replicate width '-')
      putStrLn ""
      renderType (messageBodyType msg) (messageBody msg) >>= putStrLn . flt
        where
          headerName s = printf "%-15s" (s ++ ": ")
          addressHeader as = putStrLn (intercalate ", " (map formatNameAddr as))
          textHeader s = putStrLn s
          dateHeader d = putStrLn (formatTime Data.Time.Format.defaultTimeLocale "%a %b %d %H:%M" d)
    displayReferences :: [(MessageNumber, Message)] -> IO ()
    displayReferences = mapM_ (uncurry printMessageSingle)

outlineRenderer :: Renderer
outlineRenderer msg = return ()

printMessageSingle :: MessageNumber -> Message -> IO ()
printMessageSingle mn msg = withWidth $ putStrLn . formatMessageSingle mn msg

withWidth :: (Int -> IO a) -> IO a
withWidth f = do
  (Window _ w) <- fromMaybe (Window 80 80) <$> size
  f w

formatMessageSingle :: MessageNumber -> Message -> Int -> String
formatMessageSingle mn msg width = printf "%c %5s %18s %16s %s"
                                        (flagSummary (messageFlags msg))
                                        (show mn)
                                        (take 18 from)
                                        (take 16 date)
                                        (take (width - (1 + 5 + 18 + 16 + 5)) subject)
  where
    from = fromMaybe "" $ formatNameAddrShort <$> listToMaybe (messageFrom msg)
    subject = messageSubject msg
    date = formatTime Data.Time.Format.defaultTimeLocale "%a %b %d %H:%M" (messageDate msg)


