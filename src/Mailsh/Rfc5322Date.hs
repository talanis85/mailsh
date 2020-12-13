module Mailsh.Rfc5322Date
  ( parseRfc5322Date
  , renderRfc5322Date
  ) where

import           Control.Applicative ((<|>), many)
import           Control.Monad (replicateM, liftM2)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B8
import           Data.Char (toUpper)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime

parseRfc5322Date :: String -> Maybe UTCTime
parseRfc5322Date s = case parseOnly date_time (B8.pack s) of
                       Left err -> Nothing
                       Right v -> Just v

rfc5322DateTimeFormat :: String
rfc5322DateTimeFormat = "%a, %d %b %Y %T %z"

renderRfc5322Date :: UTCTime -> String
renderRfc5322Date t = formatTime defaultTimeLocale rfc5322DateTimeFormat t

optional :: Parser a -> Parser ()
optional p = option () (p >> return ())

between :: Parser a -> Parser b -> Parser c -> Parser c
between a b c = a *> c <* b

oneOf :: String -> Parser Char
oneOf = satisfy . inClass

noneOf :: String -> Parser Char
noneOf = satisfy . notInClass

caseChar        :: Char -> Parser Char
caseChar c       = satisfy (\x -> toUpper x == toUpper c)

caseString      :: String -> Parser ()
caseString cs    = mapM_ caseChar cs <?> cs

manyN :: Int -> Parser a -> Parser [a]
manyN n p
    | n <= 0     = return []
    | otherwise  = liftM2 (++) (replicateM n p) (many p)

manyNtoM        :: Int -> Int -> Parser a -> Parser [a]
manyNtoM n m p
    | n < 0      = return []
    | n > m      = return []
    | n == m     = replicateM n p
    | n == 0     = foldr (<|>) (return []) (map (\x -> try $ replicateM x p) (reverse [1..m]))
    | otherwise  = liftM2 (++) (replicateM n p) (manyNtoM 0 (m-n) p)

spaces :: Parser String
spaces = many1 (oneOf " \t")

date_time       :: Parser UTCTime
date_time       = do -- wd <- option () (try (do wd <- day_of_week
                     --                          _ <- char ','
                     --                          return wd))
                     optional $ do
                       _ <- day_of_week
                       _ <- char ','
                       _ <- spaces
                       return ()
                     (y,m,d) <- date
                     _ <- spaces
                     (td,z) <- time
                     optional spaces
                     return $ zonedTimeToUTC $ ZonedTime
                       { zonedTimeToLocalTime = LocalTime
                         { localDay = fromGregorian (fromIntegral y) m d
                         , localTimeOfDay = td
                         }
                       , zonedTimeZone = z
                       }
                  <?> "date/time specification"

-- |This parser matches a 'day_name' or an 'obs_day_of_week' (optionally
-- wrapped in folding whitespace) and return its 'Day' value.

day_of_week :: Parser ()
day_of_week = day_name <?> "name of a day-of-the-week"

-- |This parser will the abbreviated weekday names (\"@Mon@\", \"@Tue@\", ...)
-- and return the appropriate 'Day' value.

day_name        :: Parser ()
day_name        =     do { caseString "Mon"; return () }
                  <|> do { try (caseString "Tue"); return () }
                  <|> do { caseString "Wed"; return () }
                  <|> do { caseString "Thu"; return () }
                  <|> do { caseString "Fri"; return () }
                  <|> do { try (caseString "Sat"); return () }
                  <|> do { caseString "Sun"; return () }
                  <?> "name of a day-of-the-week"

-- |This parser will match a date of the form \"@dd:mm:yyyy@\" and return
-- a tripple of the form (Int,Month,Int) - corresponding to
-- (year,month,day).

date            :: Parser (Int, Int, Int)
date            = do d <- day
                     _ <- spaces
                     m <- month
                     _ <- spaces
                     y <- year
                     return (y,m,d)
                  <?> "date specification"

-- |This parser will match a four digit number and return its integer
-- value. No range checking is performed.

year            :: Parser Int
year            = do y <- manyN 4 digit
                     return (read y :: Int)
                  <?> "year"

-- |This parser will match a 'month_name', optionally wrapped in
-- folding whitespace, or an 'obs_month' and return its 'Month'
-- value.

month           :: Parser Int
month           = month_name <?> "month name"


-- |This parser will the abbreviated month names (\"@Jan@\", \"@Feb@\", ...)
-- and return the appropriate 'Month' value.

month_name      :: Parser Int
month_name      =     do { try (caseString "Jan"); return 1 }
                  <|> do { caseString "Feb"; return 2 }
                  <|> do { try (caseString "Mar"); return 3 }
                  <|> do { try (caseString "Apr"); return 4 }
                  <|> do { caseString "May"; return 5 }
                  <|> do { try (caseString "Jun"); return 6 }
                  <|> do { caseString "Jul"; return 7 }
                  <|> do { caseString "Aug"; return 8 }
                  <|> do { caseString "Sep"; return 9 }
                  <|> do { caseString "Oct"; return 10 }
                  <|> do { caseString "Nov"; return 11 }
                  <|> do { caseString "Dec"; return 12 }
                  <?> "month name"

-- Internal helper function: match a 1 or 2-digit number (day of month).

day_of_month    :: Parser Int
day_of_month    = fmap read (manyNtoM 1 2 digit)

-- |Match a 1 or 2-digit number (day of month), recognizing both
-- standard and obsolete folding syntax.

day             :: Parser Int
day             = day_of_month <?> "day"

-- |This parser will match a 'time_of_day' specification followed by a
-- 'zone'. It returns the tuple (TimeDiff,Int) corresponding to the
-- return values of either parser.

time            :: Parser (TimeOfDay, TimeZone)
time            = do t <- time_of_day
                     _ <- spaces
                     z <- zone
                     return (t,z)
                  <?> "time and zone specification"

-- |This parser will match a time-of-day specification of \"@hh:mm@\" or
-- \"@hh:mm:ss@\" and return the corrsponding time as a 'TimeDiff'.

time_of_day     :: Parser TimeOfDay
time_of_day     = do h <- hour
                     _ <- char ':'
                     m <- minute
                     s <- option 0 (do { _ <- char ':'; second } )
                     return $ TimeOfDay
                       { todHour = h
                       , todMin = m
                       , todSec = fromIntegral s
                       }
                  <?> "time specification"

-- |This parser will match a two-digit number and return its integer
-- value. No range checking is performed.

hour            :: Parser Int
hour            = do r <- replicateM 2 digit
                     return (read r :: Int)
                  <?> "hour"

-- |This parser will match a two-digit number and return its integer
-- value. No range checking is performed.

minute          :: Parser Int
minute          = do r <- replicateM 2 digit
                     return (read r :: Int)
                  <?> "minute"

-- |This parser will match a two-digit number and return its integer
-- value. No range checking takes place.

second          :: Parser Int
second          = do r <- replicateM 2 digit
                     return (read r :: Int)
                  <?> "second"

-- |This parser will match a timezone specification of the form
-- \"@+hhmm@\" or \"@-hhmm@\" and return the zone's offset to UTC in
-- seconds as an integer. 'obs_zone' is matched as well.

zone            :: Parser TimeZone
zone            = (    do _ <- char '+'
                          h <- hour
                          m <- minute
                          return (minutesToTimeZone ((h*60)+m))
                   <|> do _ <- char '-'
                          h <- hour
                          m <- minute
                          return (minutesToTimeZone (-((h*60)+m)))
                   <|> do c1 <- letter_ascii
                          c2 <- letter_ascii
                          c3 <- letter_ascii
                          parseTimeM False defaultTimeLocale "%Z" [c1,c2,c3]
                   <?> "time zone"
                  )
