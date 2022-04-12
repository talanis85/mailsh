{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Mailsh.Store.Store
  ( StoreM
  , StoreNumber
  , storeNumber
  , StoredMessage (..)
  , StoreFilter
  , makeStoreFilter
  , FilterResult (..)
  , Limit
  , withStorePath
  , updateStore
  , liftMaildir
  , queryStore
  , queryStore'
  , filterBy
  , lookupStoreNumber
  , getReferencingMessages
  , getReferencedMessages
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Morph
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import qualified Database.Esqueleto as E
import Database.Persist.Sqlite hiding (FilterAnd, FilterOr, Single)
import System.Directory
import System.FilePath

import Data.Reparser
import Mailsh.Maildir
import Mailsh.Message
import Mailsh.Store.Message
import Mailsh.Store.Schema
import Mailsh.Store.PersistentInstances ()
import Mailsh.Types

------------------------------------------------------------------------------

newtype StoreM a = StoreM { runStoreM_ :: ReaderT Store (LoggingT MaildirM) a }
  deriving ( Functor, Applicative, Monad, MonadReader Store, MonadIO
           , MonadError String, MonadLogger)

instance MFunctor LoggingT where
  hoist f (LoggingT g) = LoggingT $ f .g

runStoreM :: StoreM a-> Store -> LoggingT MaildirM a
runStoreM m s = runReaderT (runStoreM_ m) s

liftMaildir :: MaildirM a -> StoreM a
liftMaildir = StoreM . lift . lift

--------------------------------------------------------------------------------

data Store = Store
  { _storeCache :: SqlBackend
  }

makeLenses ''Store

--------------------------------------------------------------------------------

liftCache :: ReaderT SqlBackend StoreM a -> StoreM a
liftCache m = do
  backend <- view storeCache
  runReaderT m backend

initStore = runMigration migrateTables

withStorePath :: StoreM a -> FilePath -> LoggingT (ExceptT String IO) a
withStorePath m fp = do
  md <- liftIO $ openMaildir fp
  case md of
    Left err -> throwError err
    Right md -> do
      ret <- hoist lift $ withSqliteConn (T.pack (fp </> ".mailsh.cache")) $ \db -> do
        runReaderT initStore db
        let store = Store
              { _storeCache = db
              }
        LoggingT $ \x -> runExceptT $ runLoggingT (hoist (runMaildirM md) $ runStoreM m store) x
      case ret of
        Left err -> throwError err
        Right ret -> return ret

--------------------------------------------------------------------------------

makeStoreFilter :: FilterExp -> StoreM StoreFilter
makeStoreFilter f = do
  tz <- liftIO getCurrentTimeZone
  case f of
    FilterFlag flag -> return (filterFlag (flagChar flag))
    FilterUnseen -> return filterUnseen
    FilterUntrashed -> return filterUntrashed
    FilterAll -> return filterAll
    FilterKeyword x -> return (filterKeyword x)
    FilterString x -> return (filterString x)
    FilterReferencedByID x -> return (filterReferencedBy x)
    FilterReferencedByNumber x -> return filterAll -- TODO: Implement
    FilterDate from to ->
      let zoned x = ZonedTime { zonedTimeToLocalTime = x, zonedTimeZone = tz }
      in return (filterDate (zoned <$> from) (zoned <$> to))
    FilterNot a -> filterNot <$> makeStoreFilter a
    FilterAnd a b -> filterAnd <$> makeStoreFilter a <*> makeStoreFilter b
    FilterOr a b -> filterOr <$> makeStoreFilter a <*> makeStoreFilter b

type StoreFilter
  = ( E.SqlExpr (Entity MessageE)
    , E.SqlExpr (Entity AddressE)
    , E.SqlExpr (Entity ReferenceE)
    , E.SqlExpr (Entity PartE)
    , E.SqlExpr (Entity KeywordE)
    ) -> E.SqlExpr (E.Value Bool)

data FilterResult a = FilterResult
  { resultRows :: [a]
  , resultTotal :: Int
  } deriving (Functor, Foldable, Traversable)

filterAll :: StoreFilter
filterAll _ = E.val True

filterAnd :: StoreFilter -> StoreFilter -> StoreFilter
filterAnd a b x = a x E.&&. b x

filterOr :: StoreFilter -> StoreFilter -> StoreFilter
filterOr a b x = a x E.||. b x

filterNot :: StoreFilter -> StoreFilter
filterNot f x = E.not_ (f x)

filterFlag :: Char -> StoreFilter
filterFlag c (msg, _, _, _, _) = msg E.^. MessageEFlags `E.like` E.val ("%" ++ [c] ++ "%")

filterUnseen :: StoreFilter
filterUnseen = filterNot (filterFlag 'S' `filterOr` filterFlag 'T')

filterUntrashed :: StoreFilter
filterUntrashed = filterNot (filterFlag 'T')

filterStoreNumber :: StoreNumber -> StoreFilter
filterStoreNumber (StoreNumber key) (msg, _, _, _, _) = msg E.^. MessageEId E.==. E.val key

filterReferencedBy :: T.Text -> StoreFilter
filterReferencedBy msgid (_, _, ref, _, _) = ref E.^. ReferenceEMsgId E.==. E.val msgid

filterMessageId :: T.Text -> StoreFilter
filterMessageId msgid (msg, _, _, _, _) = msg E.^. MessageEMessageId E.==. E.val (Just msgid)

filterString :: T.Text -> StoreFilter
filterString s (msg, addr, _, _, _) =
        (msg E.^. MessageESubject `E.like` E.val (Just ("%" <> s <> "%")))
  E.||. (addr E.^. AddressEName `E.like` E.val (Just ("%" <> s <> "%")))
  E.||. (addr E.^. AddressEAddress `E.like` E.val ("%" <> s <> "%"))

filterKeyword :: T.Text -> StoreFilter
filterKeyword s (msg, _, _, _, keyw) = keyw E.^. KeywordEKeyword `E.like` E.val ("%" <> s <> "%")

filterDate :: Maybe ZonedTime -> Maybe ZonedTime -> StoreFilter
filterDate Nothing Nothing _ = E.val True
filterDate Nothing to (msg, _, _, _, _) = (msg E.^. MessageEDate E.<=. E.val to)
filterDate from Nothing (msg, _, _, _, _) = (msg E.^. MessageEDate E.>=. E.val from)
filterDate from to (msg, _, _, _, _) =
        (msg E.^. MessageEDate E.>=. E.val from)
  E.&&. (msg E.^. MessageEDate E.<=. E.val to)

--------------------------------------------------------------------------------

applyFilter :: StoreFilter -> Limit -> ReaderT SqlBackend StoreM (FilterResult (Key MessageE, MessageE, [AddressE], [ReferenceE], [PartE], [KeywordE]))
applyFilter flt lim = do
  messages <- E.select $ E.from $ \(msg `E.LeftOuterJoin` addr `E.LeftOuterJoin` ref `E.LeftOuterJoin` part `E.LeftOuterJoin` keyw) -> do
    E.on (msg E.^. MessageEId E.==. keyw  E.^. KeywordEMessageRef)
    E.on (msg E.^. MessageEId E.==. part  E.^. PartEMessageRef)
    E.on (msg E.^. MessageEId E.==. ref   E.^. ReferenceEMessageRef)
    E.on (msg E.^. MessageEId E.==. addr  E.^. AddressEMessageRef)
    E.where_ (flt (msg, addr, ref, part, keyw))
    E.groupBy (msg E.^. MessageEId)
    E.orderBy [E.desc (msg E.^. MessageEDate)]
    case lim of
      NoLimit -> return ()
      Limit lim -> E.limit (fromIntegral lim)
    return msg
  withoutLimit <- E.select $ E.from $ \(msg `E.LeftOuterJoin` addr `E.LeftOuterJoin` ref `E.LeftOuterJoin` part `E.LeftOuterJoin` keyw) -> do
    E.on (msg E.^. MessageEId E.==. keyw  E.^. KeywordEMessageRef)
    E.on (msg E.^. MessageEId E.==. part  E.^. PartEMessageRef)
    E.on (msg E.^. MessageEId E.==. ref   E.^. ReferenceEMessageRef)
    E.on (msg E.^. MessageEId E.==. addr  E.^. AddressEMessageRef)
    E.where_ (flt (msg, addr, ref, part, keyw))
    E.groupBy (msg E.^. MessageEId)
  combined <- forM messages $ \msg -> do
    addresses <- E.select $ E.from $ \x -> do
      E.where_ (x E.^. AddressEMessageRef E.==. E.val (entityKey msg))
      return x
    references <- E.select $ E.from $ \x -> do
      E.where_ (x E.^. ReferenceEMessageRef E.==. E.val (entityKey msg))
      return x
    parts <- E.select $ E.from $ \x -> do
      E.where_ (x E.^. PartEMessageRef E.==. E.val (entityKey msg))
      return x
    keywords <- E.select $ E.from $ \x -> do
      E.where_ (x E.^. KeywordEMessageRef E.==. E.val (entityKey msg))
      return x
    return (entityKey msg, entityVal msg, map entityVal addresses, map entityVal references, map entityVal parts, map entityVal keywords)
  return FilterResult
    { resultRows = reverse combined
    , resultTotal = length withoutLimit
    }

filterBy :: StoreFilter -> Limit -> ReaderT SqlBackend StoreM (FilterResult StoredMessage)
filterBy flt limit = applyFilter flt limit >>= mapM (lift . combineMessage)

lookupStoreNumber :: StoreNumber -> ReaderT SqlBackend StoreM (Maybe StoredMessage)
lookupStoreNumber mn = listToMaybe . resultRows <$> filterBy (filterStoreNumber mn) NoLimit

queryStore :: ReaderT SqlBackend StoreM a -> StoreM a
queryStore = liftCache

queryStore' q = do
  x <- queryStore q
  case x of
    Nothing -> throwError "No such message"
    Just x  -> return x

combineMessage
  :: (Key MessageE, MessageE, [AddressE], [ReferenceE], [PartE], [KeywordE])
  -> StoreM StoredMessage
combineMessage (key, msg, addresses, references, parts, keywords) = do
  filename <- liftMaildir (absoluteMaildirFile (messageEMid msg))
  flags <- liftMaildir (getFlags (messageEMid msg))
  
  let initMsg = Message mempty $ Stored
        { _storedMainBody = Message mempty (messageEBody msg)
            & (contentType .~ toContentType (messageEBodyType msg))
        , _storedMid = Just (messageEMid msg)
        , _storedNumber = Just (StoreNumber key)
        , _storedFlags = flags
        , _storedParts = map (\x -> (toDisposition =<< partEDisposition x, toContentType (partEType x))) parts
        , _storedSource = B.readFile filename
        }
      combinedMsg = initMsg
        & (headerDate .~ messageEDate msg)
        & (headerMessageID .~ (reparse' messageIDParser =<< messageEMessageId msg))
        & (headerFrom defaultCharsets .~ addrsOfType "from" addresses)
        & (headerTo defaultCharsets .~ addrsOfType "to" addresses)
        & (headerCC defaultCharsets .~ addrsOfType "cc" addresses)
        & (headerBCC defaultCharsets .~ addrsOfType "bcc" addresses)
        & (headerReferences .~ mapMaybe (reparse' messageIDParser . referenceEMsgId) references)
        & (headerReplyTo defaultCharsets .~ addrsOfType "replyto" addresses)
        & (headerSubject defaultCharsets .~ messageESubject msg)
        & (headerKeywords defaultCharsets .~ map keywordEKeyword keywords)

  return combinedMsg

  where
    toContentType = fromMaybe defaultContentType . reparse' contentTypeParser
    toDisposition = reparse' contentDispositionParser
    addrsOfType t = mapMaybe (addrOfType t)
    addrOfType t x =
      if t == addressEType x
        then Single <$> Mailbox (addressEName x) <$> (reparse' addrSpecParser (addressEAddress x))
        else Nothing

--------------------------------------------------------------------------------

updateStore :: StoreM ()
updateStore = do
  mids <- liftMaildir listMaildir

  let timeOfMid m = do
        path <- liftMaildir $ absoluteMaildirFile m
        time <- liftIO $ getModificationTime path
        return (m, time)
  let flagsOfMid m = do
        flags <- liftMaildir $ getFlags m
        return (m, flags)

  newDates <- Map.fromList <$> mapM timeOfMid mids
  oldDates <- Map.fromList <$> liftCache queryMidModified
  newFlags <- Map.fromList <$> mapM flagsOfMid mids
  oldFlags <- Map.fromList <$> liftCache queryMidFlags

  let toUpdate = filterUpdate oldDates newDates
      toRemove = filterRemove oldDates newDates
      toAdd    = filterAdded oldDates newDates
      toReflag = filterReflag oldFlags newFlags

  forM_ (Map.toList toUpdate) $ \(mid, _) -> do
    -- Possible race condition. Should be atomic.
    now        <- liftIO getCurrentTime
    newMessage <- liftMaildir $ cacheFun mid
    case newMessage of
      Left err         -> logWarnN (T.pack ("Could not parse message " ++ mid ++ "\nReason:\n" ++ err))
      Right newMessage -> liftCache $ updateMessage now newMessage

  forM_ (Map.toList toRemove) $ \(mid, _) -> liftCache $ deleteMessage mid

  forM_ (Map.toList toAdd) $ \(mid, _) -> do
    now        <- liftIO getCurrentTime
    newMessage <- liftMaildir $ cacheFun mid
    case newMessage of
      Left err         -> logWarnN (T.pack ("Could not parse message " ++ mid ++ "\nReason:\n" ++ err))
      Right newMessage -> liftCache $ updateMessage now newMessage

  forM_ (Map.toList toReflag) $ \(mid, flags) -> do
    liftCache $ updateWhere [MessageEMid ==. mid] [MessageEFlags =. flags]

    where
      pairing a = Map.mapMaybeWithKey (\k v -> (,v) <$> Map.lookup k a)
      filterAdded old new = new `Map.difference` old
      filterUpdate old new = snd <$> Map.filter (\(d,e) -> d > e) (new `pairing` old)
      filterRemove old new = old `Map.difference` new
      filterReflag old new = fst <$> Map.filter (\(d,e) -> d /= e) (new `pairing` old)
      cacheFun mid = do
        flags <- getFlags mid
        fp <- absoluteMaildirFile mid
        liftIO $ parseMessageFile mid flags fp

parseMessageFile :: MID -> String -> String -> IO (Either String StoredMessage)
parseMessageFile mid flags fp = readStoredMessage (Just mid) Nothing flags (B.readFile fp)

{-
type Query a = Monad m => SqlReadT m a
type Update a = Monad m => SqlWriteT m a
-}

-- queryMidModified :: Query [(MID, UTCTime)]
queryMidModified = map (\(a,b) -> (E.unValue a, E.unValue b)) <$> queryMidModified'
  where
    queryMidModified' = E.select $ E.from $ \(message `E.InnerJoin` modified) -> do
      E.where_ (message E.^. MessageEId E.==. modified E.^. ModifiedEMessageRef)
      return (message E.^. MessageEMid, modified E.^. ModifiedEDate)

queryMidFlags = map (\(a,b) -> (E.unValue a, E.unValue b)) <$> queryMidFlags'
  where
    queryMidFlags' = E.select $ E.from $ \message -> do
      return (message E.^. MessageEMid, message E.^. MessageEFlags)

assertMID :: Maybe MID -> MID
assertMID = fromMaybe (error "Called updateMessage on a message without a MID. This is a bug!")

-- updateMessage :: UTCTime -> Message -> Update ()
updateMessage t msg = do
  key <- fmap entityKey <$> getBy (MessageEUniqueMid (assertMID (msg ^. body . storedMid)))
  case key of
    Nothing -> do
      key <- insert (extractMessageE msg)
      insertMany (extractAddressEs key msg)
      insertMany (extractReferenceEs key msg)
      insertMany (extractPartEs key msg)
      insertMany (extractKeywordEs key msg)
      insert (mkModifiedE key t)
      return ()
    Just key -> do
      replace key (extractMessageE msg)
      deleteWhere [AddressEMessageRef ==. key]
      insertMany (extractAddressEs key msg)
      deleteWhere [ReferenceEMessageRef ==. key]
      insertMany (extractReferenceEs key msg)
      deleteWhere [PartEMessageRef ==. key]
      insertMany (extractPartEs key msg)
      deleteWhere [KeywordEMessageRef ==. key]
      insertMany (extractKeywordEs key msg)
      updateWhere [ModifiedEMessageRef ==. key] [ModifiedEDate =. t]
      return ()
  where
    extractMessageE :: StoredMessage -> MessageE
    extractMessageE msg = MessageE
      { messageEMid       = assertMID (msg ^. body . storedMid)
      , messageEMessageId = msg ^? headerMessageID . traversed . to (reprint messageIDParser)
      , messageEDate      = msg ^. headerDate
      , messageESubject   = msg ^. headerSubject defaultCharsets
      , messageEBody      = msg ^. body . storedMainBody . body
      , messageEBodyType  = msg ^. body . storedMainBody . contentType . to (reprint contentTypeParser)
      , messageEFlags     = msg ^. body . storedFlags
      }
    extractAddressEs :: Key MessageE -> StoredMessage -> [AddressE]
    extractAddressEs key msg = concatMap (toaddr "from"    ) (msg ^. headerFrom defaultCharsets)
                            ++ concatMap (toaddr "to"      ) (msg ^. headerTo defaultCharsets)
                            ++ concatMap (toaddr "cc"      ) (msg ^. headerCC defaultCharsets)
                            ++ concatMap (toaddr "bcc"     ) (msg ^. headerBCC defaultCharsets)
                            ++ concatMap (toaddr "replyto" ) (msg ^. headerReplyTo defaultCharsets)
      where
        toaddr t (Single mailbox) = [ toaddr' t mailbox ]
        toaddr t (Group groupName mailboxes) = map (toaddr' t) mailboxes
        toaddr' t (Mailbox name addr) = AddressE
          { addressEMessageRef = key
          , addressEName = name
          , addressEAddress = reprint addrSpecParser addr
          , addressEType = t
          }
    extractReferenceEs :: Key MessageE -> StoredMessage -> [ReferenceE]
    extractReferenceEs key msg = map toref (msg ^. headerReferences)
      where
        toref r = ReferenceE
          { referenceEMessageRef = key
          , referenceEMsgId = reprint messageIDParser r
          }
    extractPartEs :: Key MessageE -> StoredMessage -> [PartE]
    extractPartEs key msg = map topart (msg ^. body . storedParts)
      where
        topart p = PartE
          { partEMessageRef = key
          , partEDisposition = reprint contentDispositionParser <$> fst p
          , partEType = reprint contentTypeParser (snd p)
          }
    extractKeywordEs :: Key MessageE -> StoredMessage -> [KeywordE]
    extractKeywordEs key msg = map tokeyword (msg ^. headerKeywords defaultCharsets)
      where
        tokeyword k = KeywordE
          { keywordEMessageRef = key
          , keywordEKeyword = k
          }
    mkModifiedE :: Key MessageE -> UTCTime -> ModifiedE
    mkModifiedE key t = ModifiedE
      { modifiedEMessageRef = key
      , modifiedEDate = t
      }

-- deleteMessage :: MID -> Update ()
deleteMessage mid = deleteCascadeWhere [MessageEMid ==. mid]

---

getReferencedMessages :: Message a b -> StoreM [StoredMessage]
getReferencedMessages msg =
  let referencedIds = msg ^. headerReferences
      filter = foldr (filterOr . filterMessageId. reprint messageIDParser) (filterNot filterAll) referencedIds
  in resultRows <$> queryStore (filterBy filter NoLimit)

getReferencingMessages :: Message a b -> StoreM [StoredMessage]
getReferencingMessages msg = case msg ^. headerMessageID of
  Nothing -> return []
  Just msgid -> resultRows <$>
    (queryStore (filterBy (filterReferencedBy (reprint messageIDParser msgid)) NoLimit))
