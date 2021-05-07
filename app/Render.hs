{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Render
  ( Renderer

  , fullRenderer
  , previewRenderer
  , noquoteRenderer
  , outlineRenderer

  , printMessage
  , printPlain
  , printStatusMessage
  , printError
  , printMessageLine
  , printMessageLines
  , printStoreNumbers
  , printHeaders
  , printOutline

  , terminalHeight

  , runMailcap
  , runXdgOpen
  ) where

import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.Trans
import qualified Data.CaseInsensitive as CI
import Data.Char
import Data.Char.WCWidth
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import System.Console.Terminal.Size
import Text.Parsec
import Text.Printf
import Text.Wrap
import qualified Data.ByteString.Char8 as BSC
import System.Directory
import System.IO
import System.Process

import ANSI
import Format
import Data.Reparser
import Mailsh.Maildir
import Mailsh.Message hiding (parse)
import Mailsh.MimeRender
import Mailsh.Store

type Renderer = StoredMessage -> StoreM ()

printMessage :: Renderer -> StoredMessage -> StoreM ()
printMessage = id

printPlain :: T.Text -> StoreM ()
printPlain = liftIO . T.putStrLn

printStatusMessage :: T.Text -> StoreM ()
printStatusMessage x = liftIO $ runANSI $
  withForegroundColor Vivid Green $ liftIO $ T.putStrLn x

printMessageLine :: StoredMessage -> StoreM ()
printMessageLine = liftIO . printMessageSingle defaultMessageFormat

printError :: T.Text -> StoreM ()
printError x = liftIO $ do
  runANSI $ withForegroundColor Vivid Red $ liftIO $ T.putStrLn x

printMessageLines :: ConsoleFormat -> FilterResult StoredMessage -> StoreM ()
printMessageLines format r = do
  mapM_ (liftIO . printMessageSingle format) (resultRows r)
  liftIO $ printResultCount r

printStoreNumbers :: FilterResult StoredMessage -> StoreM ()
printStoreNumbers r = do
  let printStoreNumber msg =
        case msg ^. body . storedNumber of
          Nothing -> return ()
          Just n -> putStrLn (show n)
  liftIO $ mapM_ printStoreNumber (resultRows r)

printHeaders :: Message a b -> StoreM ()
printHeaders msg = do
  mapM_ (liftIO . printHeader) (msg ^. headerList)
  where
    printHeader (key, value) = runANSI $ do
      withIntensity BoldIntensity $ liftIO $ do
        BSC.putStr (CI.foldedCase key)
        BSC.putStr ": "
      liftIO $ do
        BSC.putStr value
        BSC.putStrLn ""

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
noquoteRenderer = messageRenderer (removeBlankLines . parseFilter noquoteParser)

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
  refs <- getReferencedMessages msg
  refby <- getReferencingMessages msg

  liftIO $ do
    (Window windowHeight windowWidth) <- fromMaybe (Window 80 80) <$> liftIO size

    localDate <- mapM utcToLocalZonedTime (zonedTimeToUTC <$> msg ^. headerDate)

    let printAttachment i (fn, ct) = printf "%02d: %s (%s)\n" i (attachmentName fn) (reprint contentTypeParser ct)
        attachmentName = T.unpack . fromMaybe "(no filename)"
        headerName s = printf "%-15s" (s ++ ": ")
        addressHeader = T.putStr
          . alignHeaderValue
          . wrapTextToLines defaultWrapSettings (windowWidth - 15)
          . reprint addressesParser
        subjectHeader = T.putStr
          . alignHeaderValue
          . wrapTextToLines defaultWrapSettings (windowWidth - 15)
          . fromMaybe "(no subject)"
        alignHeaderValue x = case x of
          [] -> ""
          (x:xs) -> T.unlines (x : map (T.pack (replicate 15 ' ') <>) xs)
        dateHeader = putStrLn
          . fromMaybe "(no date)"
          . fmap (formatTime defaultTimeLocale dateTimeFormat)

    runANSI $ do
      withIntensity BoldIntensity $ liftIO $ do
        headerName "From" >> addressHeader (msg ^. headerFrom defaultCharsets)
        headerName "To" >> addressHeader (msg ^. headerTo defaultCharsets)
        headerName "Subject" >> subjectHeader (msg ^. headerSubject defaultCharsets)
        headerName "Date" >> dateHeader localDate

      liftIO $ putStrLn ""

      liftIO $ case msg ^. body . storedMainBody . charsetDecoded' defaultCharsets of
        Left err -> putStrLn (show err)
        Right msg' -> putStrLn $ flt $ T.unpack $ wrapText defaultWrapSettings windowWidth $
          renderType (msg ^. contentType) (msg' ^. body)

      when (not $ null refs) $ do
        withIntensity BoldIntensity $ liftIO $ putStrLn "Referenced messages:"
        liftIO $ mapM_ (printMessageSingle defaultMessageFormat) refs

      when (not $ null refby) $ do
        withIntensity BoldIntensity $ liftIO $ putStrLn "Referenced by:"
        liftIO $ mapM_ (printMessageSingle defaultMessageFormat) refby

      when (not $ null (msg ^.. body . storedAttachments)) $ do
        withIntensity BoldIntensity $ liftIO $ putStrLn "Attachments:"
        liftIO $ imapMOf_ (body . storedAttachments) printAttachment msg

printOutline :: StoredMessage -> StoreM ()
printOutline msg = do
  printMessageLine msg
  liftIO $ imapMOf_ (body . storedParts . traversed) printPartInfo msg

printPartInfo :: Int -> (Maybe ContentDisposition, ContentType) -> IO ()
printPartInfo i (cd, ct) = runANSI $ do
  let h = printf "#%2d: " i
      indent = replicate (length h) ' '
      printHeader s = liftIO (putStr indent) >> printHeader' s
      printHeader' s = withIntensity BoldIntensity $
        liftIO $ putStr (printf "%-20s " (s :: String))

  liftIO $ putStrLn ""

  withForegroundColor Vivid Yellow $ liftIO $ putStr h

  printHeader' "Content-Type:"
  liftIO $ T.putStrLn (reprint contentTypeParser ct)

  printHeader "Disposition:"
  liftIO $ T.putStrLn (fromMaybe "none" (reprint contentDispositionParser <$> cd))

  printHeader "Filename:"
  liftIO $ T.putStrLn (fromMaybe "none" (cd ^? traversed . filename defaultCharsets))

outlineRenderer :: Renderer
outlineRenderer msg = return ()

trimUniString :: Int -> String -> String
trimUniString n [] = []
trimUniString n (x:xs)
  | n - wcwidth x <= 0 = ""
  | otherwise = x : trimUniString (n - wcwidth x) xs

uniStringWidth :: String -> Int
uniStringWidth = sum . map wcwidth

padUniStringLeft :: Int -> Char -> String -> String
padUniStringLeft n c s
  | uniStringWidth s >= n = s
  | otherwise = replicate (n - uniStringWidth s) c ++ s

padUniStringRight :: Int -> Char -> String -> String
padUniStringRight n c s
  | uniStringWidth s >= n = s
  | otherwise = s ++ replicate (n - uniStringWidth s) c

printMessageSingle :: ConsoleFormat -> StoredMessage -> IO ()
printMessageSingle format msg = do
  (Window windowHeight windowWidth) <- fromMaybe (Window 80 80) <$> size
  runANSI $ formatStoredMessage format msg windowWidth
  putStr "\n"

dateTimeFormat :: String
dateTimeFormat = "%a %b %d %Y %H:%M"

printResultCount :: FilterResult a -> IO ()
printResultCount r = runANSI $
  withIntensity BoldIntensity $
    liftIO $ printf "%d/%d messages\n" (length (resultRows r)) (resultTotal r)

withWidth :: (Int -> IO a) -> IO a
withWidth f = do
  (Window _ w) <- fromMaybe (Window 80 80) <$> size
  f w

terminalHeight :: IO Int
terminalHeight = do
  (Window h _) <- fromMaybe (Window 80 80) <$> size
  return h

runMailcap :: ContentType -> BSC.ByteString -> IO ()
runMailcap t s = do
  let cmd = printf "run-mailcap %s/%s:-"
        (BSC.unpack $ CI.foldedCase $ t ^. ctType)
        (BSC.unpack $ CI.foldedCase $ t ^. ctSubtype)
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
