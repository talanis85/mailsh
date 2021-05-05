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
import System.Console.ANSI
import System.Directory
import System.IO
import System.Process

import Data.Reparser
import Mailsh.Message hiding (parse)
import Mailsh.MimeRender
import Mailsh.Store

type Renderer = StoredMessage -> StoreM ()

printMessage :: Renderer -> StoredMessage -> StoreM ()
printMessage = id

printPlain :: T.Text -> StoreM ()
printPlain = liftIO . T.putStrLn

printStatusMessage :: T.Text -> StoreM ()
printStatusMessage x = liftIO $ do
  setSGR [SetColor Foreground Vivid Green]
  T.putStrLn x
  setSGR [Reset]

printMessageLine :: StoredMessage -> StoreM ()
printMessageLine = liftIO . printMessageSingle False

printError :: T.Text -> StoreM ()
printError x = liftIO $ do
  setSGR [SetColor Foreground Vivid Red]
  T.putStrLn x
  setSGR [Reset]

printMessageLines :: FilterResult StoredMessage -> StoreM ()
printMessageLines r = do
  mapM_ (liftIO . printMessageSingle False) (resultRows r)
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
    printHeader (key, value) = do
      setSGR [SetConsoleIntensity BoldIntensity]
      BSC.putStr (CI.foldedCase key)
      BSC.putStr ": "
      setSGR [Reset]
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

    setSGR [SetConsoleIntensity BoldIntensity]

    headerName "From" >> addressHeader (msg ^. headerFrom defaultCharsets)
    headerName "To" >> addressHeader (msg ^. headerTo defaultCharsets)
    headerName "Subject" >> subjectHeader (msg ^. headerSubject defaultCharsets)
    headerName "Date" >> dateHeader localDate

    setSGR [Reset]

    putStrLn ""

    case msg ^. body . storedMainBody . charsetDecoded' defaultCharsets of
      Left err -> putStrLn (show err)
      Right msg' -> putStrLn $ flt $ T.unpack $ wrapText defaultWrapSettings windowWidth $
        renderType (msg ^. contentType) (msg' ^. body)

    when (not $ null refs) $ do
      setSGR [SetConsoleIntensity BoldIntensity]
      putStrLn "Referenced messages:"
      setSGR [Reset]
      mapM_ (printMessageSingle False) refs

    when (not $ null refby) $ do
      setSGR [SetConsoleIntensity BoldIntensity]
      putStrLn "Referenced by:"
      setSGR [Reset]
      mapM_ (printMessageSingle False) refby

    when (not $ null (msg ^.. body . storedAttachments)) $ do
      setSGR [SetConsoleIntensity BoldIntensity]
      putStrLn "Attachments:"
      setSGR [Reset]
      imapMOf_ (body . storedAttachments) printAttachment msg

printOutline :: StoredMessage -> StoreM ()
printOutline msg = do
  printMessageLine msg
  liftIO $ imapMOf_ (body . storedParts . traversed) printPartInfo msg

printPartInfo :: Int -> (Maybe ContentDisposition, ContentType) -> IO ()
printPartInfo i (cd, ct) = do
  let h = printf "#%2d: " i
      indent = replicate (length h) ' '
      printHeader s = putStr indent >> printHeader' s
      printHeader' s = do
        setSGR [SetConsoleIntensity BoldIntensity]
        putStr (printf "%-20s " (s :: String))
        setSGR [Reset]

  putStrLn ""

  setSGR [SetColor Foreground Vivid Yellow]
  putStr h
  setSGR [Reset]

  printHeader' "Content-Type:"
  T.putStrLn (reprint contentTypeParser ct)

  printHeader "Disposition:"
  T.putStrLn (fromMaybe "none" (reprint contentDispositionParser <$> cd))

  printHeader "Filename:"
  T.putStrLn (fromMaybe "none" (cd ^? traversed . filename defaultCharsets))

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

printMessageSingle :: Bool -> StoredMessage -> IO ()
printMessageSingle underline msg = do
  (Window windowHeight windowWidth) <- fromMaybe (Window 80 80) <$> size

  localDate <- mapM utcToLocalZonedTime (zonedTimeToUTC <$> msg ^. headerDate)
  let date = fromMaybe "(no date)" $ formatTime defaultTimeLocale dateTimeFormat <$> localDate

  let flagsAndNumber = printf "%c %c %5s" flags attachmentSymbol number
      nameInfo = printf " %s" (padUniStringRight 26 ' ' (trimUniString 26 from))
      dateInfo = printf " %21s " (take 21 date)

  let subjectWidth = windowWidth
        - length (flagsAndNumber)
        - length (nameInfo)
        - length (dateInfo)

  when underline $ setSGR [SetUnderlining SingleUnderline]

  setSGR [SetColor Foreground Vivid Yellow]

  putStr flagsAndNumber

  setSGR [SetColor Foreground Vivid White]
  setSGR [SetConsoleIntensity BoldIntensity]

  putStr nameInfo

  setSGR [SetConsoleIntensity NormalIntensity]
  setSGR [SetConsoleIntensity FaintIntensity]

  putStr dateInfo

  setSGR [SetConsoleIntensity NormalIntensity]

  putStr (trimUniString subjectWidth subject)
  putStr "\n"

  setSGR [Reset]

  where
    flags = flagSummary (msg ^. body . storedFlags)
    attachmentSymbol = if null (msg ^.. body . storedAttachments) then ' ' else '§'
    number = fromMaybe "" (show <$> msg ^. body . storedNumber)
    from = T.unpack $ fromMaybe "(nobody)" $ formatAddressShort <$> msg ^? headerFrom defaultCharsets . traversed
    subject = T.unpack $ fromMaybe "(no subject)" $ msg ^. headerSubject defaultCharsets

dateTimeFormat :: String
dateTimeFormat = "%a %b %d %Y %H:%M"

formatAddressShort :: Address -> T.Text
formatAddressShort (Single (Mailbox (Just name) _)) = name
formatAddressShort (Single (Mailbox Nothing addr)) = reprint addrSpecParser addr
formatAddressShort (Group name _) = name

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
