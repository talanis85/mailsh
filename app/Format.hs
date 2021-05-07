module Format
  ( ConsoleFormat (..)
  , parseConsoleFormat

  , formatConsole
  , formatStoredMessage
  , defaultMessageFormat
  ) where

import ANSI
import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.Trans
import Data.Bifunctor
import Data.Char.WCWidth
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import qualified Data.Text as T
import Data.Time
import Text.Parsec
import Text.Parsec.String

import Data.Reparser
import Mailsh.Message hiding (parse)
import Mailsh.Store.Message

data FormatFieldSize = FormatFieldSize Bool (Maybe Int)
  deriving (Eq, Show)

data ConsoleFormat
  = ConsoleFormatEmpty
  | ConsoleFormatField Char FormatFieldSize
  | ConsoleFormatForegroundColor ColorIntensity Color ConsoleFormat
  | ConsoleFormatBackgroundColor ColorIntensity Color ConsoleFormat
  | ConsoleFormatIntensity ConsoleIntensity ConsoleFormat
  | ConsoleFormatCat ConsoleFormat ConsoleFormat
  | ConsoleFormatString String
  deriving (Eq, Show)

instance Semigroup ConsoleFormat where
  (<>) = ConsoleFormatCat

instance Monoid ConsoleFormat where
  mempty = ConsoleFormatEmpty

instance IsString ConsoleFormat where
  fromString = ConsoleFormatString

parseConsoleFormat :: String -> Either String ConsoleFormat
parseConsoleFormat s = first (("Format string parse error: " ++) . show) $
  parse (consoleFormatP <* eof) "<formatstring>" s

consoleFormatP :: Parser ConsoleFormat
consoleFormatP = foldr ConsoleFormatCat ConsoleFormatEmpty <$> many consoleFormatTokenP

consoleFormatTokenP :: Parser ConsoleFormat
consoleFormatTokenP = choice $ map try
  [ ConsoleFormatString <$> many1 (noneOf "%}")
  , do
      string "%{("
      modifiers <- modifierP `sepBy1` char ','
      char ')'
      rest <- consoleFormatP
      char '}'
      return (foldr ($) rest modifiers)
  , do
      char '%'
      rightAlign <- option False (char '-' >> return True)
      size <- optionMaybe (read <$> many1 digit)
      c <- letter
      return (ConsoleFormatField c (FormatFieldSize rightAlign size))
  ]

modifierP :: Parser (ConsoleFormat -> ConsoleFormat)
modifierP = choice $ map try
  [ string "bold" >> return (ConsoleFormatIntensity BoldIntensity)
  , string "faint" >> return (ConsoleFormatIntensity FaintIntensity)
  , string "black" >> return (ConsoleFormatForegroundColor Vivid Black)
  , string "red" >> return (ConsoleFormatForegroundColor Vivid Red)
  , string "green" >> return (ConsoleFormatForegroundColor Vivid Green)
  , string "yellow" >> return (ConsoleFormatForegroundColor Vivid Yellow)
  , string "blue" >> return (ConsoleFormatForegroundColor Vivid Blue)
  , string "magenta" >> return (ConsoleFormatForegroundColor Vivid Magenta)
  , string "cyan" >> return (ConsoleFormatForegroundColor Vivid Cyan)
  , string "white" >> return (ConsoleFormatForegroundColor Vivid White)
  ]

formatConsole :: (Char -> FormatFieldSize -> a -> Int -> ANSI Int) -> ConsoleFormat -> a -> Int -> ANSI Int
formatConsole formatField ConsoleFormatEmpty x cols = return cols
formatConsole formatField (ConsoleFormatForegroundColor intensity color format) x cols =
  withForegroundColor intensity color (formatConsole formatField format x cols)
formatConsole formatField (ConsoleFormatBackgroundColor intensity color format) x cols =
  withBackgroundColor intensity color (formatConsole formatField format x cols)
formatConsole formatField (ConsoleFormatIntensity intensity format) x cols =
  withIntensity intensity (formatConsole formatField format x cols)
formatConsole formatField (ConsoleFormatField f fieldSize) x cols = formatField f fieldSize x cols
formatConsole formatField (ConsoleFormatCat a b) x cols
  | cols <= 0 = return cols
  | otherwise = do
      cols' <- formatConsole formatField a x cols
      formatConsole formatField b x cols'
formatConsole formatField (ConsoleFormatString s) x cols = do
  putStrWithSize (FormatFieldSize False Nothing) cols s

formatStoredMessage :: ConsoleFormat -> StoredMessage -> Int -> ANSI ()
formatStoredMessage format msg cols = void $ formatConsole formatter format msg cols
  where
    formatter 'd' size msg cols = do
      localDate <- liftIO $ mapM utcToLocalZonedTime (zonedTimeToUTC <$> msg ^. headerDate)
      putStrWithSize size cols $
        fromMaybe "(no date)" $ formatTime defaultTimeLocale dateTimeFormat <$> localDate
    formatter c size msg cols = putStrWithSize size cols (formatter' c msg)

    formatter' 'x' msg = pure (flagSummary (msg ^. body . storedFlags))
    formatter' 'a' msg = if null (msg ^.. body . storedAttachments) then " " else "§"
    formatter' 'n' msg = fromMaybe "" (show <$> msg ^. body . storedNumber)
    formatter' 'f' msg = T.unpack $ fromMaybe "(nobody)" $
      formatAddressShort <$> msg ^? headerFrom defaultCharsets . traversed
    formatter' 't' msg = T.unpack $ fromMaybe "(nobody)" $
      formatAddressShort <$> msg ^? headerTo defaultCharsets . traversed
    formatter' 's' msg = T.unpack $ fromMaybe "(no subject)" $ msg ^. headerSubject defaultCharsets
    formatter' _ msg = ""

putStrWithSize :: FormatFieldSize -> Int -> String -> ANSI Int
putStrWithSize size cols s = liftIO $ do
  let s' = trimUniString cols $ padToFieldSize size s
  putStr s'
  return (cols - length s')

padToFieldSize (FormatFieldSize _ Nothing) = id
padToFieldSize (FormatFieldSize False (Just n)) = padUniStringRight n ' ' . trimUniString n
padToFieldSize (FormatFieldSize True (Just n)) = padUniStringLeft n ' ' . trimUniString n

defaultMessageFormat :: ConsoleFormat
defaultMessageFormat = fromRight (error "error in Format.defaultMessageFormat") $
  parseConsoleFormat "%{(yellow)%x %a} %{(blue)%n} %{(faint)%d} %{(bold)%30f} %s"

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

trimUniString :: Int -> String -> String
trimUniString n [] = []
trimUniString n (x:xs)
  | n - wcwidth x <= 0 = ""
  | otherwise = x : trimUniString (n - wcwidth x) xs

flagSummary :: String -> Char
flagSummary flags
  | 'T' `elem` flags                        = 'T'
  | 'F' `elem` flags                        = 'F'
  | 'R' `elem` flags                        = 'R'
  | 'S' `elem` flags && 'T' `notElem` flags = 'O'
  | otherwise                               = 'N'

formatAddressShort :: Address -> T.Text
formatAddressShort (Single (Mailbox (Just name) _)) = name
formatAddressShort (Single (Mailbox Nothing addr)) = reprint addrSpecParser addr
formatAddressShort (Group name _) = name

dateTimeFormat :: String
dateTimeFormat = "%a %b %d %Y %H:%M"
