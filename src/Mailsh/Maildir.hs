{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Mailsh.Maildir
  ( MID
  , MaildirM
  , CachedMessage (..)
  , withMaildirPath
  , getMaildirPath
  , listMaildir
  , absoluteMaildirFile
  , purgeMaildir
  , setFlag
  , unsetFlag
  , hasFlag
  , getFlags
  , getMessage
  ) where

import Control.Lens

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe
import Data.List
import Data.Typeable
import Data.SafeCopy
import qualified Data.Map as Map

import System.Directory
import System.FilePath

import Data.CachedDir
import Mailsh.Parse
import Network.Email

------------------------------------------------------------------------------

-- | The Maildir Monad
newtype MaildirM a = MaildirM
  { runMaildirM_ :: ExceptT String (StateT MaildirState (ReaderT Maildir IO)) a }
  deriving ( Functor, Applicative, Monad, MonadReader Maildir
           , MonadState MaildirState, MonadError String, MonadIO)

runMaildirM :: MaildirM a -> Maildir -> IO (Either String a)
runMaildirM m = runReaderT (evalStateT (runExceptT (runMaildirM_ m)) mkMaildirState)

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

data CachedMessage = CachedMessage
  { messageHeaders :: [Field]
  -- , messageMainContentType :: MimeType
  -- , messageMainBody :: T.Text
  }

data Maildir = Maildir
  { _maildirPath :: FilePath
  , _maildirCache :: CachedDir MID CachedMessage
  }

------------------------------------------------------------------------------

makeLenses ''MaildirState
makeLenses ''Maildir

------------------------------------------------------------------------------

$(deriveSafeCopy 0 'base ''EncodingType)
$(deriveSafeCopy 0 'base ''MimeType)
$(deriveSafeCopy 0 'base ''MsgID)
$(deriveSafeCopy 0 'base ''NameAddr)
$(deriveSafeCopy 0 'base ''Field)
$(deriveSafeCopy 0 'base ''CachedMessage)

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
  valid <- and <$> mapM (doesDirectoryExist . (fp </>)) ["cur", "new", "tmp"]
  if not valid
     then return (Left "This is not a valid maildir")
     else do
       updateNew fp
       cachedDir <- openCachedDir (fp </> "cur") "../.mailsh.cache" midOf (readCachedMessage (fp </> "cur"))
       return $ Right Maildir
         { _maildirPath = fp
         , _maildirCache = cachedDir
         }
       -- TODO remove old files from tmp

-- | Move messages from 'new' to 'cur'
updateNew :: FilePath -> IO ()
updateNew maildir = do
  let moveNewFile filename = renameFile (newOf maildir </> filename)
                                        (curOf maildir </> (fst (breakVersion filename) ++ ":2,"))
  newFiles <- filter (\x -> (x /= ".") && (x /= "..")) <$> getDirectoryContents (newOf maildir)
  mapM_ moveNewFile newFiles

readCachedMessage :: FilePath -> MID -> IO (Maybe CachedMessage)
readCachedMessage fp mid = do
  fileList <- liftIO $ filter (\x -> (x /= ".") && (x /= ".."))
    <$> getDirectoryContents fp
  case listToMaybe (filter (isPrefixOf (mid ++ ",")) fileList) of
    Nothing -> return Nothing
    Just file -> do
      msg <- parseCrlfFile (fp </> file) parseCachedMessage
      case msg of
        Left err -> return Nothing
        Right msg -> return (Just msg)

parseCachedMessage :: Attoparsec CachedMessage
parseCachedMessage = do
  headers <- parseHeaders
  message <- parseMessage (mimeTextPlain "utf8") headers
  return CachedMessage
    { messageHeaders = headers
    }

-- | Get a 'MaildirFile' from a 'MID'
getMaildirFile :: MID -> MaildirM MaildirFile
getMaildirFile mid = do
  m <- getMIDCache
  case Map.lookup mid m of
    Nothing -> throwError ("No such message: " ++ mid)
    Just x  -> return x

------------------------------------------------------------------------------

withMaildirPath :: MaildirM a -> FilePath -> IO (Either String a)
withMaildirPath m p = do
  maildir <- openMaildir p
  case maildir of
    Left err -> return (Left err)
    Right maildir -> runMaildirM m maildir

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

getMessage :: MID -> MaildirM (Maybe CachedMessage)
getMessage mid = do
  cache <- view maildirCache
  liftIO (lookupCache cache mid)
