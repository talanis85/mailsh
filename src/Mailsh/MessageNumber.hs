module Mailsh.MessageNumber
  ( MessageNumber
  , invalidMessageNumber
  , checksumMessages
  , checksumListing
  , lookupMessage
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
lookupMessage (MessageNumber n c) mids =
  if n < 1 || length mids < n
     then Nothing
     else let mid = mids !! (n - 1)
              c'  = genChecksum' mid
          in if c' /= c
                then Nothing
                else Just mid
