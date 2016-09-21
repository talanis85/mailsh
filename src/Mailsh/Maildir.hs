module Mailsh.Maildir
  ( Maildir
  , openMaildir
  , listMaildir
  , setFlag
  , unsetFlag
  , hasFlag
  ) where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath

data Maildir = Maildir { getMaildir :: FilePath }
  deriving (Show)

type MID = String

curOf = (</> "cur") . getMaildir
tmpOf = (</> "tmp") . getMaildir
newOf = (</> "new") . getMaildir

openMaildir :: FilePath -> IO Maildir
openMaildir fp = do
  let md = Maildir fp
  check md
  return md
  -- TODO check if maildir exists
  -- TODO remove old files from tmp

check :: Maildir -> IO ()
check md = return () -- TODO move messages from 'new' to 'cur'

listMaildir :: Maildir -> IO [MID]
listMaildir maildir =
  filter (\x -> (x /= ".") && (x /= ".."))<$> getDirectoryContents (curOf maildir)

setFlag :: Char -> Maildir -> MID -> IO ()
setFlag f maildir mid = do
  let (basename, flags') = break (== ',') mid
      flags              = tail flags'
      newflags           = nub (insert f flags)
      newmid             = basename ++ "," ++ newflags
  renameFile (curOf maildir </> mid) (curOf maildir </> newmid)

unsetFlag :: Char -> Maildir -> MID -> IO ()
unsetFlag f maildir mid = do
  let (basename, flags') = break (== ',') mid
      flags              = tail flags'
      newflags           = delete f flags
      newmid             = basename ++ "," ++ newflags
  renameFile (curOf maildir </> mid) (curOf maildir </> newmid)

hasFlag :: Char -> Maildir -> MID -> Bool
hasFlag f maildir mid =
  let (basename, flags') = break (== ',') mid
      flags              = tail flags'
  in f `elem` flags
