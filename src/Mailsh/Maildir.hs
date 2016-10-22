{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mailsh.Maildir
  ( Maildir
  , MID
  , MaildirM
  , runMaildirM
  , withMaildirPath
  , getMaildirPath
  , listMaildir
  , absoluteMaildirFile
  , purgeMaildir
  , setFlag
  , unsetFlag
  , hasFlag
  , getFlags
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import System.Directory
import System.FilePath

-- | A unique message id in a maildir. This is the file name without flags.
type MID = String
-- | The physical file name of a message.
type MaildirFile = String

data Maildir = Maildir { getMaildir :: FilePath }
  deriving (Show)

data MaildirCache = MaildirCache
  { mdcList :: Maybe (Map.Map MID MaildirFile)
  }

mkMaildirCache :: MaildirCache
mkMaildirCache = MaildirCache
  { mdcList = Nothing
  }

newtype MaildirM a = MaildirM { _runMaildirM :: ExceptT String (StateT MaildirCache (ReaderT Maildir IO)) a }
  deriving (Functor, Applicative, Monad, MonadReader Maildir, MonadError String, MonadState MaildirCache, MonadIO)

runMaildirM :: MaildirM a -> Maildir -> IO (Either String a)
runMaildirM m = runReaderT (evalStateT (runExceptT (_runMaildirM m)) mkMaildirCache)

withMaildirPath :: MaildirM a -> FilePath -> IO (Either String a)
withMaildirPath m p = do
  maildir <- openMaildir p
  case maildir of
    Left err -> return (Left err)
    Right maildir -> runMaildirM m maildir

getMaildirPath :: MaildirM FilePath
getMaildirPath = getMaildir <$> ask

curOf = (</> "cur") . getMaildir
tmpOf = (</> "tmp") . getMaildir
newOf = (</> "new") . getMaildir

splitLast :: (Eq a) => a -> [a] -> ([a], [a])
splitLast c s = let (l, r) = foldl f ([], []) s
                in if null l then (r, [])
                             else (l, r)
  where
    f (l, r) x | x == c && null l = (r, [])
               | x == c           = (l ++ [c] ++ r, [])
               | x /= c = (l, r ++ [x])

-- | Split a 'MaildirFile' into a 'MID' and its flags.
breakFlags :: MaildirFile -> (MID, String)
breakFlags = splitLast ','

-- | Split a 'MaildirFile' into basename and version+flags
breakVersion :: MaildirFile -> (String, String)
breakVersion = splitLast ':'

-- | Get a 'MID' from a 'MaildirFile'
midOf :: MaildirFile -> MID
midOf = fst . breakFlags

clearCache :: MaildirM ()
clearCache = modify (\s -> s { mdcList = Nothing })

updateCache :: MaildirM ()
updateCache = do
  maildir <- ask
  fileList <- liftIO $ filter (\x -> (x /= ".") && (x /= ".."))
    <$> getDirectoryContents (curOf maildir)
  let listMap = Map.fromList (map (\x -> (midOf x, x)) fileList)
  modify (\s -> s { mdcList = Just listMap })

cachedMap :: MaildirM (Map.Map MID MaildirFile)
cachedMap = do
  l <- mdcList <$> get
  case l of
    Nothing -> updateCache >> cachedMap
    Just l  -> return l

cachedList :: MaildirM [(MID, MaildirFile)]
cachedList = Map.toAscList <$> cachedMap

cachedMIDs :: MaildirM [MID]
cachedMIDs = map fst <$> cachedList

cachedFiles :: MaildirM [MaildirFile]
cachedFiles = map snd <$> cachedList

-- | Get a 'MaildirFile' from a 'MID'
getMaildirFile :: MID -> MaildirM MaildirFile
getMaildirFile mid = do
  m <- cachedMap
  case Map.lookup mid m of
    Nothing -> throwError ("No such message: " ++ mid)
    Just x  -> return x

-- | Open a maildir and does all the garbage collection the maildir
--   spec requires.
openMaildir :: FilePath -> IO (Either String Maildir)
openMaildir fp = do
  valid <- and <$> mapM (doesDirectoryExist . (fp </>)) ["cur", "new", "tmp"]
  if not valid
     then return (Left "This is not a valid maildir")
     else do
       let md = Maildir fp
       -- TODO remove old files from tmp
       updateNew md
       return (Right md)

-- | Move messages from 'new' to 'cur'
updateNew :: Maildir -> IO ()
updateNew maildir = do
  newFiles <- liftIO $ filter (\x -> (x /= ".") && (x /= ".."))
    <$> getDirectoryContents (newOf maildir)
  mapM_ moveNewFile newFiles
    where
      moveNewFile filename = renameFile (newOf maildir </> filename)
                                        (curOf maildir </> (fst (breakVersion filename) ++ ":2,"))

-- | Get all messages in the maildir in ascending order.
listMaildir :: MaildirM [MID]
listMaildir = cachedMIDs

-- | Get the absolute 'FilePath' of a message.
--   Users should be aware that the returned path might be invalid
--   in concurrent situations.
absoluteMaildirFile :: MID -> MaildirM FilePath
absoluteMaildirFile mid = do
  file <- getMaildirFile mid
  maildir <- ask
  return (curOf maildir </> file)

-- | Delete all trashed messages.
purgeMaildir :: MaildirM Int
purgeMaildir = do
  mids <- cachedMIDs
  sum <$> mapM purgeFile mids
    where
      purgeFile mid = do
        trashed <- hasFlag 'T' mid
        if trashed
           then do
             f <- absoluteMaildirFile mid
             liftIO $ removeFile f
             return 1
           else return 0

setFlag :: Char -> MID -> MaildirM ()
setFlag f mid = do
  file <- getMaildirFile mid
  maildir <- ask
  let (basename, flags) = breakFlags file
      newflags          = nub (insert f flags)
      newfile           = basename ++ "," ++ newflags
  liftIO $ renameFile (curOf maildir </> file) (curOf maildir </> newfile)
  clearCache

unsetFlag :: Char -> MID -> MaildirM ()
unsetFlag f mid = do
  file <- getMaildirFile mid
  maildir <- ask
  let (basename, flags) = breakFlags file
      newflags          = delete f flags
      newfile           = basename ++ "," ++ newflags
  liftIO $ renameFile (curOf maildir </> file) (curOf maildir </> newfile)
  clearCache

hasFlag :: Char -> MID -> MaildirM Bool
hasFlag f mid = do
  file <- getMaildirFile mid
  let (basename, flags) = breakFlags file
  return (f `elem` flags)

getFlags :: MID -> MaildirM String
getFlags mid = do
  file <- getMaildirFile mid
  return (snd (breakFlags file))
