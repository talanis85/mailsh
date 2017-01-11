{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Data.CachedDir
  ( CachedDir
  , openCachedDir
  , lookupCache
  , listCache
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import qualified Data.Map as Map
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar
import Data.Typeable
import Data.SafeCopy
import System.Directory
import System.FilePath

data CachedDir k a = CachedDir FilePath (AcidState (Cache k a))
data Cache k a = Cache (Map.Map k (UTCTime, a))

type CacheFunction k a = k -> IO (Maybe a)

instance (Ord k, SafeCopy a, SafeCopy k) => SafeCopy (Cache k a) where
  putCopy (Cache c) = contain $ safePut c
  getCopy = contain $ Cache <$> safeGet

cacheGetTimes :: (Ord k) => Query (Cache k a) (Map.Map k UTCTime)
cacheGetTimes = do
  Cache m <- ask
  return (fst <$> m)

cacheLookup :: (Ord k) => k -> Query (Cache k a) (Maybe a)
cacheLookup k = do
  Cache m <- ask
  return (snd <$> Map.lookup k m)

cacheList :: (Ord k) => Query (Cache k a) (Map.Map k a)
cacheList = do
  Cache m <- ask
  return (snd <$> m)

cacheInsert :: (Ord k) => k -> (UTCTime, a) -> Update (Cache k a) ()
cacheInsert k v = do
  Cache m <- get
  put (Cache (Map.insert k v m))

cacheDelete :: (Ord k) => k -> Update (Cache k a) ()
cacheDelete k = do
  Cache m <- get
  put (Cache (Map.delete k m))

$(makeAcidic ''Cache ['cacheGetTimes, 'cacheLookup, 'cacheList, 'cacheInsert, 'cacheDelete])

openCachedDir :: (Ord k, Typeable k, SafeCopy k, Typeable a, SafeCopy a)
              => FilePath -> String -> (String -> k) -> CacheFunction k a -> IO (CachedDir k a)
openCachedDir path name keymap cacheFun = do
  acid <- openLocalStateFrom (path </> name) (Cache Map.empty)
  updateCache path acid keymap cacheFun
  return (CachedDir path acid)

lookupCache :: (Ord k, Typeable k, SafeCopy k, Typeable a, SafeCopy a)
            => CachedDir k a -> k -> IO (Maybe a)
lookupCache (CachedDir path acid) key = query acid (CacheLookup key)

listCache :: (Ord k, Typeable k, SafeCopy k, Typeable a, SafeCopy a)
          => CachedDir k a -> IO (Map.Map k a)
listCache (CachedDir path acid) = query acid CacheList

pairing :: (Ord k) => Map.Map k a -> Map.Map k b -> Map.Map k (a, b)
pairing a = Map.mapMaybeWithKey (\k v -> (,v) <$> Map.lookup k a)

getTimes :: FilePath -> IO (Map.Map String UTCTime)
getTimes path = do
  let noDotFiles ('.':_) = False
      noDotFiles _ = True
  dir <- filter noDotFiles <$> getDirectoryContents path
  Map.fromList . zip dir <$> mapM (getModificationTime . (path </>)) dir

filterUpdate :: (Ord k, Ord a) => Map.Map k a -> Map.Map k a -> Map.Map k a
filterUpdate old new =
  let added = new `Map.difference` old
      updated = fst <$> Map.filter (uncurry (>)) (new `pairing` old)
  in added `Map.union` updated

filterRemove :: (Ord k) => Map.Map k a -> Map.Map k a -> Map.Map k a
filterRemove old new = old `Map.difference` new

updateCache :: (Ord k, Typeable k, SafeCopy k, Typeable a, SafeCopy a)
            => FilePath -> AcidState (Cache k a) -> (String -> k) -> CacheFunction k a -> IO ()
updateCache path acid keymap cacheFun = do
  oldTimes <- query acid CacheGetTimes
  newTimes <- Map.mapKeys keymap <$> getTimes path
  let cacheWithTime (name, time) = do
        val <- cacheFun name
        return ((\val -> (name, (time, val))) <$> val)
  updatedValues <- catMaybes <$> mapM cacheWithTime (Map.toList (filterUpdate oldTimes newTimes))
  mapM_ (update acid . uncurry CacheInsert) updatedValues
  mapM_ (update acid . CacheDelete) (Map.keys (filterRemove oldTimes newTimes))
