{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mailsh.Maildir
  ( Maildir
  , MID
  , MaildirM
  , runMaildirM
  , withMaildirPath
  , listMaildir
  , absoluteMaildirFile
  , setFlag
  , unsetFlag
  , hasFlag
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.List
import System.Directory
import System.FilePath

data Maildir = Maildir { getMaildir :: FilePath }
  deriving (Show)

newtype MaildirM a = MaildirM { _runMaildirM :: ExceptT String (ReaderT Maildir IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Maildir, MonadError String, MonadIO)

runMaildirM :: MaildirM a -> Maildir -> IO (Either String a)
runMaildirM m = runReaderT (runExceptT (_runMaildirM m))

withMaildirPath :: MaildirM a -> FilePath -> IO (Either String a)
withMaildirPath m p = do
  maildir <- openMaildir p
  runMaildirM m maildir

-- | A unique message id in a maildir. This is the file name without flags.
type MID = String
-- | The physical file name of a message.
type MaildirFile = String

curOf = (</> "cur") . getMaildir
tmpOf = (</> "tmp") . getMaildir
newOf = (</> "new") . getMaildir

-- | Split a 'MaildirFile' into a 'MID' and its flags.
breakFlags :: MaildirFile -> (MID, String)
breakFlags f =
  let (basename, flags') = break (== ',') f
  in (basename, tail flags')

-- | Get a 'MID' from a 'MaildirFile'
midOf :: MaildirFile -> MID
midOf = fst . breakFlags

-- | Get a 'MaildirFile' from a 'MID'
getMaildirFile :: MID -> MaildirM MaildirFile
getMaildirFile mid = do
  maildir <- ask
  fileList <- liftIO $ filter (\x -> (x /= ".") && (x /= ".."))
    <$> getDirectoryContents (curOf maildir)
  let match = filter (isPrefixOf mid) fileList
  case match of
    [] -> throwError ("No such message: " ++ mid)
    (x:_) -> return x

-- | Open a maildir and does all the garbage collection the maildir
--   spec requires.
openMaildir :: FilePath -> IO Maildir
openMaildir fp = do
  let md = Maildir fp
  return md
  -- TODO check if maildir exists
  -- TODO remove old files from tmp
  -- TODO move messages from 'new' to 'cur'

-- | Get all messages in the maildir in ascending order.
listMaildir :: MaildirM [MID]
listMaildir = do
  maildir <- ask
  liftIO $ sort
    <$> map midOf
    <$> filter (\x -> (x /= ".") && (x /= ".."))
    <$> getDirectoryContents (curOf maildir)

-- | Get the absolute 'FilePath' of a message.
--   Users should be aware that the returned path might be invalid
--   in concurrent situations.
absoluteMaildirFile :: MID -> MaildirM FilePath
absoluteMaildirFile mid = do
  file <- getMaildirFile mid
  maildir <- ask
  return (curOf maildir </> file)

setFlag :: Char -> MID -> MaildirM ()
setFlag f mid = do
  file <- getMaildirFile mid
  maildir <- ask
  let (basename, flags) = breakFlags file
      newflags          = nub (insert f flags)
      newfile           = basename ++ "," ++ newflags
  liftIO $ renameFile (curOf maildir </> file) (curOf maildir </> newfile)

unsetFlag :: Char -> MID -> MaildirM ()
unsetFlag f mid = do
  file <- getMaildirFile mid
  maildir <- ask
  let (basename, flags) = breakFlags file
      newflags          = delete f flags
      newfile           = basename ++ "," ++ newflags
  liftIO $ renameFile (curOf maildir </> file) (curOf maildir </> newfile)

hasFlag :: Char -> MID -> MaildirM Bool
hasFlag f mid = do
  file <- getMaildirFile mid
  let (basename, flags) = breakFlags file
  return (f `elem` flags)
