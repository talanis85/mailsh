{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Mailsh.Maildir
  ( MID
  , MaildirM
  , isMaildir
  , openMaildir
  , runMaildirM
  , withMaildirPath
  , getMaildirPath
  , getOtherMaildirFile
  , listMaildir
  , absoluteMaildirFile
  , purgeMaildir
  , midOf
  , setFlag
  , unsetFlag
  , hasFlag
  , getFlags
  , writeMaildirFile
  , moveIntoMaildirNew
  , moveIntoMaildirCur
  ) where

import Control.Lens

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString.Lazy as BL

import Data.List
import qualified Data.Map as Map

import Data.Time.Clock
import Data.Time.Format

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import Network.HostName

import System.Directory
import System.FilePath

import Text.Printf

------------------------------------------------------------------------------

-- | The Maildir Monad
newtype MaildirM a = MaildirM
  { runMaildirM_ :: ExceptT String (StateT MaildirState (ReaderT Maildir IO)) a }
  deriving ( Functor, Applicative, Monad, MonadReader Maildir
           , MonadState MaildirState, MonadError String, MonadIO)

runMaildirM :: Maildir -> MaildirM a -> ExceptT String IO a
runMaildirM md m = do
  r <- lift $ runReaderT (evalStateT (runExceptT (runMaildirM_ m)) mkMaildirState) md
  case r of
    Left err -> throwError err
    Right x  -> return x

------------------------------------------------------------------------------

-- | A unique message id in a maildir. This is the file name without flags.
type MID = String
-- | The physical file name of a message.
type MaildirFile = String

data MaildirState = MaildirState
  { _maildirStateMIDCache :: Maybe (Map.Map MID MaildirFile)
  }

mkMaildirState :: MaildirState
mkMaildirState = MaildirState
  { _maildirStateMIDCache = Nothing
  }

------------------------------------------------------------------------------

data Maildir = Maildir
  { _maildirPath :: FilePath
  }

------------------------------------------------------------------------------

makeLenses ''MaildirState
makeLenses ''Maildir

------------------------------------------------------------------------------

invalidateMIDCache :: MaildirM ()
invalidateMIDCache = maildirStateMIDCache .= Nothing

updateMIDCache :: MaildirM ()
updateMIDCache = do
  curPath <- curOf <$> view maildirPath
  fileList <- liftIO $ filter (\x -> (x /= ".") && (x /= "..") && (not ("." `isPrefixOf` x)))
    <$> getDirectoryContents curPath 
  let listMap = Map.fromList (map (\x -> (midOf x, x)) fileList)
  maildirStateMIDCache .= Just listMap

getMIDCache :: MaildirM (Map.Map MID MaildirFile)
getMIDCache = do
  l <- use maildirStateMIDCache
  case l of
    Nothing -> updateMIDCache >> getMIDCache
    Just l  -> return l

getMIDCacheList :: MaildirM [(MID, MaildirFile)]
getMIDCacheList = Map.toAscList <$> getMIDCache

getMIDCacheMIDs :: MaildirM [MID]
getMIDCacheMIDs = map fst <$> getMIDCacheList

getMIDCacheFiles :: MaildirM [MaildirFile]
getMIDCacheFiles = map snd <$> getMIDCacheList

------------------------------------------------------------------------------

curOf = (</> "cur")
tmpOf = (</> "tmp")
newOf = (</> "new")

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

------------------------------------------------------------------------------

-- | Open a maildir and does all the garbage collection the maildir
--   spec requires.
openMaildir :: FilePath -> IO (Either String Maildir)
openMaildir fp = do
  valid <- isMaildir fp
  if not valid
     then return (Left "This is not a valid maildir")
     else do
       updateNew fp
       return $ Right Maildir
         { _maildirPath = fp
         }
       -- TODO remove old files from tmp

isMaildir :: FilePath -> IO Bool
isMaildir fp = and <$> mapM (doesDirectoryExist . (fp </>)) ["cur", "new", "tmp"]

-- | Move messages from 'new' to 'cur'
updateNew :: FilePath -> IO ()
updateNew maildir = do
  let moveNewFile filename = renameFile (newOf maildir </> filename)
                                        (curOf maildir </> (fst (breakVersion filename) ++ ":2,"))
  newFiles <- filter (\x -> (x /= ".") && (x /= "..")) <$> getDirectoryContents (newOf maildir)
  mapM_ moveNewFile newFiles

-- | Get a 'MaildirFile' from a 'MID'
getMaildirFile :: MID -> MaildirM MaildirFile
getMaildirFile mid = do
  m <- getMIDCache
  case Map.lookup mid m of
    Nothing -> throwError ("No such message: " ++ mid)
    Just x  -> return x

getOtherMaildirFile :: FilePath -> MaildirM FilePath
getOtherMaildirFile p = (</> p) <$> getMaildirPath

------------------------------------------------------------------------------

withMaildirPath :: MaildirM a -> FilePath -> ExceptT String IO a
withMaildirPath m p = do
  maildir <- lift $ openMaildir p
  case maildir of
    Left err -> throwError err
    Right maildir -> runMaildirM maildir m

getMaildirPath :: MaildirM FilePath
getMaildirPath = view maildirPath

-- | Get all messages in the maildir in ascending order.
listMaildir :: MaildirM [MID]
listMaildir = getMIDCacheMIDs

-- | Get the absolute 'FilePath' of a message.
--   Users should be aware that the returned path might be invalid
--   in concurrent situations.
absoluteMaildirFile :: MID -> MaildirM FilePath
absoluteMaildirFile mid = do
  file <- getMaildirFile mid
  curPath <- curOf <$> view maildirPath
  return (curPath </> file)

-- | Delete all trashed messages.
purgeMaildir :: MaildirM Int
purgeMaildir = do
  mids <- getMIDCacheMIDs
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
  curPath <- curOf <$> view maildirPath
  let (basename, flags) = breakFlags file
      newflags          = nub (insert f flags)
      newfile           = basename ++ "," ++ newflags
  liftIO $ renameFile (curPath </> file) (curPath </> newfile)
  invalidateMIDCache

unsetFlag :: Char -> MID -> MaildirM ()
unsetFlag f mid = do
  file <- getMaildirFile mid
  curPath <- curOf <$> view maildirPath
  let (basename, flags) = breakFlags file
      newflags          = delete f flags
      newfile           = basename ++ "," ++ newflags
  liftIO $ renameFile (curPath </> file) (curPath </> newfile)
  invalidateMIDCache

hasFlag :: Char -> MID -> MaildirM Bool
hasFlag f mid = do
  file <- getMaildirFile mid
  let (basename, flags) = breakFlags file
  return (f `elem` flags)

getFlags :: MID -> MaildirM String
getFlags mid = do
  file <- getMaildirFile mid
  return (snd (breakFlags file))

generateMaildirFile :: IO String
generateMaildirFile = do
  currentTime <- getCurrentTime
  let currentTimeNumber = formatTime defaultTimeLocale "%s" currentTime
  hostName <- getHostName
  uuid <- UUID.nextRandom
  return $ printf "%s.%s.%s" currentTimeNumber (UUID.toString uuid) hostName

writeMaildirFile :: BL.ByteString -> MaildirM MaildirFile
writeMaildirFile bs = do
  name <- liftIO generateMaildirFile
  newPath <- newOf <$> view maildirPath
  liftIO $ BL.writeFile (newPath </> name) bs
  invalidateMIDCache
  return name

moveIntoMaildirNew :: FilePath -> MaildirM String
moveIntoMaildirNew fp = do
  newFilename <- liftIO generateMaildirFile
  newPath <- newOf <$> view maildirPath
  liftIO $ renameFile fp (newPath </> newFilename)
  invalidateMIDCache
  return newFilename

moveIntoMaildirCur :: FilePath -> String -> MaildirM String
moveIntoMaildirCur fp flags = do
  newFilename <- liftIO generateMaildirFile
  curPath <- curOf <$> view maildirPath
  liftIO $ renameFile fp (curPath </> (newFilename ++ "," ++ flags))
  invalidateMIDCache
  return newFilename
