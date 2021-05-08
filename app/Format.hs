module Format
  ( RichFormat (..)
  , parseRichFormat

  , formatStoredMessage
  , defaultMessageFormat
  ) where

import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Bifunctor
import Data.Char.WCWidth
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import qualified Data.Text as T
import Data.Time
import Text.Parsec hiding (State)
import Text.Parsec.String

import Data.Reparser
import Mailsh.Message hiding (parse)
import Mailsh.Store.Message

import ANSI
import RichString

data FormatFieldSize = FormatFieldSize Bool (Maybe Int)
  deriving (Eq, Show)

data RichFormat
  = RichFormatEmpty
  | RichFormatField Char FormatFieldSize
  | RichFormatForegroundColor ColorIntensity Color RichFormat
  | RichFormatBackgroundColor ColorIntensity Color RichFormat
  | RichFormatIntensity ConsoleIntensity RichFormat
  | RichFormatCat RichFormat RichFormat
  | RichFormatString String
  deriving (Eq, Show)

instance Semigroup RichFormat where
  (<>) = RichFormatCat

instance Monoid RichFormat where
  mempty = RichFormatEmpty

instance IsString RichFormat where
  fromString = RichFormatString

parseRichFormat :: String -> Either String RichFormat
parseRichFormat s = first (("Format string parse error: " ++) . show) $
  parse (richFormatP <* eof) "<formatstring>" s

richFormatP :: Parser RichFormat
richFormatP = foldr RichFormatCat RichFormatEmpty <$> many richFormatTokenP

richFormatTokenP :: Parser RichFormat
richFormatTokenP = choice $ map try
  [ RichFormatString <$> many1 (noneOf "%}")
  , do
      string "%{("
      modifiers <- modifierP `sepBy1` char ','
      char ')'
      rest <- richFormatP
      char '}'
      return (foldr ($) rest modifiers)
  , do
      char '%'
      rightAlign <- option False (char '-' >> return True)
      size <- optionMaybe (read <$> many1 digit)
      c <- letter
      return (RichFormatField c (FormatFieldSize rightAlign size))
  ]

modifierP :: Parser (RichFormat -> RichFormat)
modifierP = choice $ map try
  [ string "bold" >> return (RichFormatIntensity BoldIntensity)
  , string "faint" >> return (RichFormatIntensity FaintIntensity)
  , string "black" >> return (RichFormatForegroundColor Vivid Black)
  , string "red" >> return (RichFormatForegroundColor Vivid Red)
  , string "green" >> return (RichFormatForegroundColor Vivid Green)
  , string "yellow" >> return (RichFormatForegroundColor Vivid Yellow)
  , string "blue" >> return (RichFormatForegroundColor Vivid Blue)
  , string "magenta" >> return (RichFormatForegroundColor Vivid Magenta)
  , string "cyan" >> return (RichFormatForegroundColor Vivid Cyan)
  , string "white" >> return (RichFormatForegroundColor Vivid White)
  ]

formatRichString
  :: (Char -> FormatFieldSize -> a -> State Int RichString)
  -> RichFormat
  -> a
  -> State Int RichString
formatRichString formatField format x = case format of
  RichFormatEmpty -> return ""
  RichFormatForegroundColor intensity color format' ->
    mapAttributes (attrForeground .~ Just (intensity, color)) <$> formatRichString formatField format' x
  RichFormatBackgroundColor intensity color format' ->
    mapAttributes (attrBackground .~ Just (intensity, color)) <$> formatRichString formatField format' x
  RichFormatIntensity intensity format' ->
    mapAttributes (attrIntensity .~ intensity) <$> formatRichString formatField format' x
  RichFormatField f fieldSize ->
    formatField f fieldSize x
  RichFormatCat a b -> do
    cols <- get
    if cols <= 0
       then return ""
       else (<>) <$> formatRichString formatField a x <*> formatRichString formatField b x
  RichFormatString s ->
    richStringWithSize (FormatFieldSize False Nothing) s

richStringWithSize :: FormatFieldSize -> String -> State Int RichString
richStringWithSize size s = do
  cols <- get
  let s' = trimUniString cols $ padToFieldSize size s
  modify $ subtract (length s')
  return (richString s')

formatStoredMessage :: TimeZone -> RichFormat -> Int -> StoredMessage  -> RichString
formatStoredMessage tz format cols msg = evalState (formatRichString f format msg) cols
  where
    localDate = utcToZonedTime tz <$> zonedTimeToUTC <$> msg ^. headerDate

    f 'd' size msg = richStringWithSize size $
      fromMaybe "(no date)" $ formatTime defaultTimeLocale dateTimeFormat <$> localDate
    f c size msg = richStringWithSize size (f' c msg)

    f' 'x' msg = pure (flagSummary (msg ^. body . storedFlags))
    f' 'a' msg = if null (msg ^.. body . storedAttachments) then " " else "§"
    f' 'n' msg = fromMaybe "" (show <$> msg ^. body . storedNumber)
    f' 'f' msg = T.unpack $ fromMaybe "(nobody)" $
      formatAddressShort <$> msg ^? headerFrom defaultCharsets . traversed
    f' 't' msg = T.unpack $ fromMaybe "(nobody)" $
      formatAddressShort <$> msg ^? headerTo defaultCharsets . traversed
    f' 's' msg = T.unpack $ fromMaybe "(no subject)" $ msg ^. headerSubject defaultCharsets
    f' _ msg = ""

{-
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
-}

padToFieldSize (FormatFieldSize _ Nothing) = id
padToFieldSize (FormatFieldSize False (Just n)) = padUniStringRight n ' ' . trimUniString n
padToFieldSize (FormatFieldSize True (Just n)) = padUniStringLeft n ' ' . trimUniString n

defaultMessageFormat :: RichFormat
defaultMessageFormat = fromRight (error "error in Format.defaultMessageFormat") $
  parseRichFormat "%{(yellow)%x %a} %{(blue)%5n} %{(faint)%d} %{(bold)%30f} %s"

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
