{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
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

module Mailsh.Store
  ( StoreM
  , MessageNumber
  , messageNumber
  , StoreMessage (..)
  , parseMessageFile
  , parseMessageString
  , FilterExp
  , FilterResult (..)
  , Limit
  , withStorePath
  , liftMaildir
  , queryStore
  , filterBy
  , lookupMessageNumber
  , filterAll
  , filterAnd
  , filterOr
  , filterNot
  , filterFlag
  , filterUnseen
  , filterUntrashed
  , filterReferencedBy
  , filterMessageId
  , filterString
  , filterKeyword
  ) where

import Control.Applicative
import Control.Lens hiding ((^.))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Morph
import Control.Monad.Reader
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid ((<>))
import Data.List (union)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Compat
import Data.Time.Clock
import qualified Database.Esqueleto as E
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Directory
import System.FilePath

import Mailsh.Maildir
import Mailsh.Message
import Mailsh.MimeType
import Mailsh.Parse
import Mailsh.PersistentInstances ()
import Mailsh.Fields

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

data StoreMessage = StoreMessage
  { messageMid :: MID
  , messageFlags :: [Char]
  , messageDigest :: DigestMessage
  }

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
MessageE
  mid         MID
  date        UTCTime
  messageId   MsgID
  subject     T.Text
  body        T.Text
  bodyType    T.Text
  flags       String
  MessageEUniqueMid mid
ModifiedE
  messageRef  MessageEId
  date        UTCTime
  ModifiedEUniqueMessageRef messageRef
AddressE
  messageRef  MessageEId
  type        String
  name        T.Text Maybe
  address     T.Text
ReferenceE
  messageRef  MessageEId
  msgId       MsgID
PartE
  messageRef  MessageEId
  filename    T.Text Maybe
  type        MimeType
KeywordE
  messageRef  MessageEId
  keyword     T.Text
  |]

newtype MessageNumber = MessageNumber { getMessageNumber :: MessageEId }

messageNumber :: Int -> MessageNumber
messageNumber n = MessageNumber (toSqlKey (fromIntegral n))

instance Read MessageNumber where
  readsPrec n s = map (\(a,b) -> (MessageNumber (toSqlKey a), b)) (readsPrec n s)

instance Show MessageNumber where
  show = show . fromSqlKey . getMessageNumber

type Limit = Maybe Int

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
    Right md -> withSqliteConn (T.pack (fp </> ".mailsh.cache")) $ \db -> do
      runReaderT initStore db
      let store = Store
            { _storeCache = db
            }
      hoist (runMaildirM md) $ runStoreM (updateStore >> m) store

--------------------------------------------------------------------------------

type FilterExp
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

filterAll :: FilterExp
filterAll _ = E.val True

filterAnd :: FilterExp -> FilterExp -> FilterExp
filterAnd a b x = a x E.&&. b x

filterOr :: FilterExp -> FilterExp -> FilterExp
filterOr a b x = a x E.||. b x

filterNot :: FilterExp -> FilterExp
filterNot f x = E.not_ (f x)

filterFlag :: Char -> FilterExp
filterFlag c (msg, _, _, _, _) = msg E.^. MessageEFlags `E.like` E.val ("%" ++ [c] ++ "%")

filterUnseen :: FilterExp
filterUnseen = filterNot (filterFlag 'S' `filterOr` filterFlag 'T')

filterUntrashed :: FilterExp
filterUntrashed = filterNot (filterFlag 'T')

filterMessageNumber :: MessageNumber -> FilterExp
filterMessageNumber (MessageNumber key) (msg, _, _, _, _) = msg E.^. MessageEId E.==. E.val key

filterReferencedBy :: MsgID -> FilterExp
filterReferencedBy msgid (_, _, ref, _, _) = ref E.^. ReferenceEMsgId E.==. E.val msgid

filterMessageId :: MsgID -> FilterExp
filterMessageId msgid (msg, _, _, _, _) = msg E.^. MessageEMessageId E.==. E.val msgid

filterString :: T.Text -> FilterExp
filterString s (msg, addr, _, _, _) =
        (msg E.^. MessageESubject `E.like` E.val ("%" <> s <> "%"))
  E.||. (addr E.^. AddressEName `E.like` E.val (Just ("%" <> s <> "%")))
  E.||. (addr E.^. AddressEAddress `E.like` E.val ("%" <> s <> "%"))

filterKeyword :: T.Text -> FilterExp
filterKeyword s (msg, _, _, _, keyw) = keyw E.^. KeywordEKeyword `E.like` E.val ("%" <> s <> "%")

--------------------------------------------------------------------------------

applyFilter :: (MonadIO m) => FilterExp -> Limit -> ReaderT SqlBackend m (FilterResult (Key MessageE, MessageE, [AddressE], [ReferenceE], [PartE], [KeywordE]))
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
      Nothing -> return ()
      Just lim -> E.limit (fromIntegral lim)
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

filterBy :: (MonadIO m) => FilterExp -> Limit -> ReaderT SqlBackend m (FilterResult (MessageNumber, StoreMessage))
filterBy flt limit = fmap combineMessage <$> applyFilter flt limit

lookupMessageNumber :: (MonadIO m) => MessageNumber -> ReaderT SqlBackend m (Maybe StoreMessage)
lookupMessageNumber mn = fmap snd . listToMaybe . resultRows <$> filterBy (filterMessageNumber mn) Nothing

queryStore :: ReaderT SqlBackend StoreM a -> StoreM a
queryStore = liftCache

combineMessage :: (Key MessageE, MessageE, [AddressE], [ReferenceE], [PartE], [KeywordE]) -> (MessageNumber, StoreMessage)
combineMessage (key, msg, addresses, references, parts, keywords) = (MessageNumber key, StoreMessage
  { messageMid         = messageEMid msg
  , messageFlags       = messageEFlags msg
  , messageDigest      = DigestMessage
    { messageDate        = messageEDate msg
    , messageMessageId   = messageEMessageId msg
    , messageFrom        = addrsOfType "from" addresses
    , messageTo          = addrsOfType "to" addresses
    , messageCc          = addrsOfType "cc" addresses
    , messageBcc         = addrsOfType "bcc" addresses
    , messageReferences  = map referenceEMsgId references
    , messageReplyTo     = addrsOfType "replyto" addresses
    , messageSubject     = messageESubject msg
    , messageKeywords    = map keywordEKeyword keywords
    , messageBody        = messageEBody msg
    , messageBodyType    = CI.mk (messageEBodyType msg)
    , messageParts       = map (\x -> (partEFilename x, partEType x)) parts
    }
  })
    where
      addrsOfType t = mapMaybe (addrOfType t)
      addrOfType t x = if t == addressEType x
                          then Just Mailbox { mailboxName = addressEName x
                                            , mailboxAddr = addressEAddress x
                                            }
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

parseMessageString :: MID -> String -> B.ByteString -> Either String StoreMessage
parseMessageString mid flags bs = parseStoreMessage mid flags bs

parseMessageFile :: MID -> String -> String -> IO (Either String StoreMessage)
parseMessageFile mid flags fp = parseMessageString mid flags <$> B.readFile fp

parseStoreMessage :: MID -> String -> B.ByteString -> Either String StoreMessage
parseStoreMessage mid flags bs = do
  msg <- parseOnly messageP bs
  return StoreMessage
    { messageMid = mid
    , messageFlags = flags
    , messageDigest = digestMessage msg
    }

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

-- updateMessage :: UTCTime -> Message -> Update ()
updateMessage t msg = do
  key <- fmap entityKey <$> getBy (MessageEUniqueMid (messageMid msg))
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
    extractMessageE :: StoreMessage -> MessageE
    extractMessageE msg = MessageE
      { messageEMid       = messageMid msg
      , messageEMessageId = messageMessageId $ messageDigest msg
      , messageEDate      = messageDate $ messageDigest msg
      , messageESubject   = messageSubject $ messageDigest msg
      , messageEBody      = messageBody $ messageDigest msg
      , messageEBodyType  = CI.foldedCase $ messageBodyType $ messageDigest msg
      , messageEFlags     = messageFlags msg
      }
    extractAddressEs :: Key MessageE -> StoreMessage -> [AddressE]
    extractAddressEs key msg = map (toaddr "from"    ) (messageFrom    $ messageDigest msg)
                            ++ map (toaddr "to"      ) (messageTo      $ messageDigest msg)
                            ++ map (toaddr "cc"      ) (messageCc      $ messageDigest msg)
                            ++ map (toaddr "bcc"     ) (messageBcc     $ messageDigest msg)
                            ++ map (toaddr "replyto" ) (messageReplyTo $ messageDigest msg)
      where
        toaddr t a = AddressE
          { addressEMessageRef = key
          , addressEName = mailboxName a
          , addressEAddress = mailboxAddr a
          , addressEType = t
          }
    extractReferenceEs :: Key MessageE -> StoreMessage -> [ReferenceE]
    extractReferenceEs key msg = map toref (messageReferences $ messageDigest msg)
      where
        toref r = ReferenceE
          { referenceEMessageRef = key
          , referenceEMsgId = r
          }
    extractPartEs :: Key MessageE -> StoreMessage -> [PartE]
    extractPartEs key msg = map topart (messageParts $ messageDigest msg)
      where
        topart p = PartE
          { partEMessageRef = key
          , partEFilename = fst p
          , partEType = snd p
          }
    extractKeywordEs :: Key MessageE -> StoreMessage -> [KeywordE]
    extractKeywordEs key msg = map tokeyword (messageKeywords $ messageDigest msg)
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
