{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Render
  ( Renderer
  , fullRenderer
  , previewRenderer
  , noquoteRenderer
  , outlineRenderer
  , formatMessageSingle
  , printMessageSingle
  , printResultCount
  , terminalHeight
  , runMailcap
  , runXdgOpen
  ) where

import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Format
import System.Console.Terminal.Size
import Text.Parsec
import Text.Printf
import qualified Data.ByteString.Char8 as BSC
import System.Console.ANSI
import System.Directory
import System.IO
import System.Process

import Mailsh.Fields
import Mailsh.Message
import Mailsh.MimeRender
import Mailsh.MimeType
import Mailsh.Store

type Renderer = DigestMessage -> StoreM ()

flagSummary :: String -> Char
flagSummary flags
  | 'T' `elem` flags                        = 'T'
  | 'F' `elem` flags                        = 'F'
  | 'R' `elem` flags                        = 'R'
  | 'S' `elem` flags && 'T' `notElem` flags = 'O'
  | otherwise                               = 'N'

removeBlankLines :: String -> String
removeBlankLines = unlines . fst . foldr removeBlankLines' ([], 0) . lines
  where
    removeBlankLines' x (xs, n) = case x of
                                    "" -> if n >= 2
                                             then (xs, n + 1)
                                             else ("" : xs, n + 1)
                                    _  -> (x : xs, 0)

fullRenderer :: Renderer
fullRenderer = messageRenderer (removeBlankLines)

previewRenderer :: Renderer
previewRenderer = messageRenderer (unlines . take 10 . lines)

noquoteRenderer :: Renderer
noquoteRenderer = messageRenderer (removeEmojis . removeBlankLines . parseFilter noquoteParser)

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
  refs <- concat <$> mapM (fmap resultRows . queryStore . flip filterBy Nothing . filterMessageId) (messageReferences msg)
  refby <- resultRows <$> queryStore (filterBy (filterReferencedBy (messageMessageId msg)) Nothing)
  liftIO $ displayReferences refs
  liftIO $ displayReferencedBy refby
  liftIO $ displayParts
  where
    displayMessage :: DigestMessage -> Int -> IO ()
    displayMessage msg width = do
      setSGR [SetConsoleIntensity BoldIntensity]
      headerName "From" >> addressHeader (messageFrom msg)
      headerName "To" >> addressHeader (messageTo msg)
      headerName "Subject" >> textHeader (messageSubject msg)
      headerName "Date" >> dateHeader (messageDate msg)
      setSGR [Reset]
      putStrLn ""
      let rendered = T.unpack $ renderType (messageBodyType msg) (messageBody msg)
      putStrLn $ flt rendered
        where
          headerName s = printf "%-15s" (s ++ ": ")
          addressHeader as = T.putStrLn (T.intercalate ", " (map formatMailbox as))
          textHeader s = T.putStrLn s
          dateHeader d = putStrLn (formatTime Data.Time.Format.defaultTimeLocale "%a %b %d %H:%M (%z)" d)
    displayReferences :: [(MessageNumber, StoreMessage)] -> IO ()
    displayReferences [] = return ()
    displayReferences refs = do
      setSGR [SetConsoleIntensity BoldIntensity]
      putStrLn "Referenced messages:"
      setSGR [Reset]
      mapM_ (uncurry printMessageSingle) refs
    displayReferencedBy :: [(MessageNumber, StoreMessage)] -> IO ()
    displayReferencedBy [] = return ()
    displayReferencedBy refbys = do
      setSGR [SetConsoleIntensity BoldIntensity]
      putStrLn "Referenced by:"
      setSGR [Reset]
      mapM_ (uncurry printMessageSingle) refbys
    displayParts = do
      when (not (null (messageAttachments msg))) $ do
        setSGR [SetConsoleIntensity BoldIntensity]
        putStrLn "Attachments:"
        setSGR [Reset]
        forM_ (zip ([1..] :: [Int]) (messageParts msg)) $ \(i, part) -> case isAttachment part of
          Nothing -> return ()
          Just (fn, t) -> printf "%d: %s (%s)\n" i fn (formatMimeTypeShort t)

outlineRenderer :: Renderer
outlineRenderer msg = return ()

printMessageSingle :: MessageNumber -> StoreMessage -> IO ()
printMessageSingle mn msg = withWidth $ putStrLn . formatMessageSingle mn msg

printResultCount :: FilterResult a -> IO ()
printResultCount r = do
  setSGR [SetConsoleIntensity BoldIntensity]
  printf "%d/%d messages\n" (length (resultRows r)) (resultTotal r)
  setSGR [Reset]

withWidth :: (Int -> IO a) -> IO a
withWidth f = do
  (Window _ w) <- fromMaybe (Window 80 80) <$> size
  f w

terminalHeight :: IO Int
terminalHeight = do
  (Window h _) <- fromMaybe (Window 80 80) <$> size
  return h

formatMessageSingle :: MessageNumber -> StoreMessage -> Int -> String
formatMessageSingle mn msg width = printf "%c %c %5s %18s %24s %s"
                                        (flagSummary (messageFlags msg))
                                        (if null (messageAttachments (messageDigest msg)) then ' ' else '§')
                                        (show mn)
                                        (take 18 from)
                                        (take 24 date)
                                        (take (width - (2 + 2 + 6 + 19 + 17)) subject)
  where
    from = T.unpack $ fromMaybe "" $ formatMailboxShort <$> listToMaybe (messageFrom $ messageDigest msg)
    subject = T.unpack $ messageSubject $ messageDigest msg
    date = formatTime Data.Time.Format.defaultTimeLocale "%a %b %d %H:%M (%z)" (messageDate $ messageDigest msg)

runMailcap :: MimeType -> BSC.ByteString -> IO ()
runMailcap t s = do
  let cmd = printf "run-mailcap %s:-" (formatMimeTypeShort t)
  hFlush stdout
  (Just inH, _, _, procH) <-
    createProcess_ "see" (shell cmd) { std_in = CreatePipe }
  BSC.hPutStrLn inH s
  waitForProcess procH
  return ()

runXdgOpen :: T.Text -> BSC.ByteString -> IO ()
runXdgOpen filename bs = do
  tempdir <- getTemporaryDirectory
  (tempf, temph) <- openTempFile tempdir (T.unpack filename)
  BSC.hPutStr temph bs
  hClose temph
  let cmd = printf "xdg-open \"%s\"" tempf
  (_, _, _, procH) <- createProcess_ "xdg-open" (shell cmd)
  waitForProcess procH
  removeFile tempf
  return ()

removeEmojis :: String -> String
removeEmojis = foldr f mempty
  where f char text
          | ord char >= 0x1f600 && ord char <= 0x1f64f = "*emoji*" ++ text
          | ord char >= 0x1f300 && ord char <= 0x1f5ff = "*emoji*" ++ text
          | otherwise = [char] ++ text
