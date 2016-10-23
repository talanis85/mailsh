module Mailsh.MessageNumber
  ( MessageNumber
  , invalidMessageNumber
  , unsafeMessageNumber
  , checksumMessages
  , checksumListing
  , lookupMessage
  , lookupMessageUnsafe
  ) where

import Data.Char
import GHC.Read
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)

import Mailsh.Types
import Mailsh.Maildir

data MessageNumber = MessageNumber Int Char

instance Show MessageNumber where
  show (MessageNumber n c) = show n ++ [c]

instance Read MessageNumber where
  readPrec = lift messageNumberP

invalidMessageNumber :: MessageNumber
invalidMessageNumber = MessageNumber 0 '_'

unsafeMessageNumber :: Int -> MessageNumber
unsafeMessageNumber n = MessageNumber n '_'

messageNumberP :: ReadP MessageNumber
messageNumberP = MessageNumber <$> numberP <*> checksumP
  where
    numberP = read <$> many1 (satisfy (`elem` ['0'..'9']))
    checksumP = satisfy (`elem` ['a'..'z'])

genChecksum :: String -> String -> Char
genChecksum charset s = charset !! (sum (map ord s) `mod` length charset)

genChecksum' :: String -> Char
genChecksum' = genChecksum ['a'..'z']

checksumMessage :: Int -> MID -> MessageNumber
checksumMessage n mid = MessageNumber n (genChecksum' mid)

checksumMessages :: [MID] -> [MessageNumber]
checksumMessages = zipWith checksumMessage [1..]

checksumListing :: [MID] -> [(MessageNumber, MID)]
checksumListing mids = zip (checksumMessages mids) mids

lookupMessage :: MessageNumber -> [MID] -> Maybe MID
lookupMessage mn mids = do
  mid <- lookupMessageUnsafe mn mids
  if checkMessageNumber mn mid
     then Just mid
     else Nothing

lookupMessageUnsafe :: MessageNumber -> [MID] -> Maybe MID
lookupMessageUnsafe (MessageNumber n _) mids =
  if n < 1 || length mids < n
     then Nothing
     else Just $ mids !! (n - 1)

checkMessageNumber :: MessageNumber -> MID -> Bool
checkMessageNumber (MessageNumber n c) mid = c == genChecksum' mid
