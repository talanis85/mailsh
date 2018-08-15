{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Mailsh.Store
  ( StoreM
  , MessageNumber
  , messageNumber
  , Message (..)
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
  ) where

import Control.Lens hiding ((^.))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Morph
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Maybe
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
import Mailsh.Parse
import Mailsh.PersistentInstances ()

import Network.Email

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

data Message = Message
  { messageMid         :: MID
  , messageDate        :: UTCTime
  , messageFlags       :: String
  , messageMessageId   :: MsgID
  , messageFrom        :: [NameAddr]
  , messageTo          :: [NameAddr]
  , messageCc          :: [NameAddr]
  , messageBcc         :: [NameAddr]
  , messageReferences  :: [MsgID]
  , messageReplyTo     :: [NameAddr]
  , messageSubject     :: String
  , messageBody        :: T.Text
  , messageBodyType    :: String
  , messageParts       :: [MimeType]
  }

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
MessageE
  mid         MID
  date        UTCTime
  messageId   MsgID
  subject     String
  body        T.Text
  bodyType    String
  flags       String
  MessageEUniqueMid mid
ModifiedE
  messageRef  MessageEId
  date        UTCTime
  ModifiedEUniqueMessageRef messageRef
AddressE
  messageRef  MessageEId
  type        String
  name        String Maybe
  address     String
ReferenceE
  messageRef  MessageEId
  msgId       MsgID
PartE
  messageRef  MessageEId
  type        MimeType
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
filterFlag c (msg, _, _, _) = msg E.^. MessageEFlags `E.like` E.val ("%" ++ [c] ++ "%")

filterUnseen :: FilterExp
filterUnseen = filterNot (filterFlag 'S' `filterOr` filterFlag 'T')

filterUntrashed :: FilterExp
filterUntrashed = filterNot (filterFlag 'T')

filterMessageNumber :: MessageNumber -> FilterExp
filterMessageNumber (MessageNumber key) (msg, _, _, _) = msg E.^. MessageEId E.==. E.val key

filterReferencedBy :: MsgID -> FilterExp
filterReferencedBy msgid (_, _, ref, _) = ref E.^. ReferenceEMsgId E.==. E.val msgid

filterMessageId :: MsgID -> FilterExp
filterMessageId msgid (msg, _, _, _) = msg E.^. MessageEMessageId E.==. E.val msgid

filterString :: String -> FilterExp
filterString s (msg, addr, _, _) =
        (msg E.^. MessageESubject `E.like` E.val ("%" ++ s ++ "%"))
  E.||. (addr E.^. AddressEName `E.like` E.val (Just ("%" ++ s ++ "%")))
  E.||. (addr E.^. AddressEAddress `E.like` E.val ("%" ++ s ++ "%"))

--------------------------------------------------------------------------------

applyFilter :: (MonadIO m) => FilterExp -> Limit -> ReaderT SqlBackend m (FilterResult (Key MessageE, MessageE, [AddressE], [ReferenceE], [PartE]))
applyFilter flt lim = do
  messages <- E.select $ E.from $ \(msg `E.LeftOuterJoin` addr `E.LeftOuterJoin` ref `E.LeftOuterJoin` part) -> do
    E.on (msg E.^. MessageEId E.==. part  E.^. PartEMessageRef)
    E.on (msg E.^. MessageEId E.==. ref   E.^. ReferenceEMessageRef)
    E.on (msg E.^. MessageEId E.==. addr  E.^. AddressEMessageRef)
    E.where_ (flt (msg, addr, ref, part))
    E.groupBy (msg E.^. MessageEId)
    E.orderBy [E.desc (msg E.^. MessageEDate)]
    case lim of
      Nothing -> return ()
      Just lim -> E.limit (fromIntegral lim)
    return msg
  withoutLimit <- E.select $ E.from $ \(msg `E.LeftOuterJoin` addr `E.LeftOuterJoin` ref `E.LeftOuterJoin` part) -> do
    E.on (msg E.^. MessageEId E.==. part  E.^. PartEMessageRef)
    E.on (msg E.^. MessageEId E.==. ref   E.^. ReferenceEMessageRef)
    E.on (msg E.^. MessageEId E.==. addr  E.^. AddressEMessageRef)
    E.where_ (flt (msg, addr, ref, part))
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
    return (entityKey msg, entityVal msg, map entityVal addresses, map entityVal references, map entityVal parts)
  return FilterResult
    { resultRows = reverse combined
    , resultTotal = length withoutLimit
    }

filterBy :: (MonadIO m) => FilterExp -> Limit -> ReaderT SqlBackend m (FilterResult (MessageNumber, Message))
filterBy flt limit = fmap combineMessage <$> applyFilter flt limit

lookupMessageNumber :: (MonadIO m) => MessageNumber -> ReaderT SqlBackend m (Maybe Message)
lookupMessageNumber mn = fmap snd . listToMaybe . resultRows <$> filterBy (filterMessageNumber mn) Nothing

queryStore :: ReaderT SqlBackend StoreM a -> StoreM a
queryStore = liftCache

combineMessage :: (Key MessageE, MessageE, [AddressE], [ReferenceE], [PartE]) -> (MessageNumber, Message)
combineMessage (key, msg, addresses, references, parts) = (MessageNumber key, Message
  { messageMid         = messageEMid msg
  , messageDate        = messageEDate msg
  , messageFlags       = messageEFlags msg
  , messageMessageId   = messageEMessageId msg
  , messageFrom        = addrsOfType "from" addresses
  , messageTo          = addrsOfType "to" addresses
  , messageCc          = addrsOfType "cc" addresses
  , messageBcc         = addrsOfType "bcc" addresses
  , messageReferences  = map referenceEMsgId references
  , messageReplyTo     = addrsOfType "replyto" addresses
  , messageSubject     = messageESubject msg
  , messageBody        = messageEBody msg
  , messageBodyType    = messageEBodyType msg
  , messageParts       = map partEType parts
  })
    where
      addrsOfType t = mapMaybe (addrOfType t)
      addrOfType t x = if t == addressEType x
                          then Just NameAddr { nameAddr_name = addressEName x
                                             , nameAddr_addr = addressEAddress x
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
      Left err         -> logInfoN (T.pack ("Could not parse message " ++ mid ++ "\nReason:\n" ++ err))
      Right newMessage -> liftCache $ updateMessage now newMessage

  forM_ (Map.toList toRemove) $ \(mid, _) -> liftCache $ deleteMessage mid

  forM_ (Map.toList toAdd) $ \(mid, _) -> do
    now        <- liftIO getCurrentTime
    newMessage <- liftMaildir $ cacheFun mid
    case newMessage of
      Left err         -> logInfoN (T.pack ("Could not parse message " ++ mid ++ "\nReason:\n" ++ err))
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

parseMessageString :: MID -> String -> BL.ByteString -> IO (Either String Message)
parseMessageString mid flags bs = do
  let hasCrlf = detectCrlf bs
      result = if hasCrlf
                  then parseByteString bs (basicMessage mid flags)
                  else parseCrlfByteString bs (basicMessage mid flags)
  case result of
    Left err -> return (Left err)
    Right x  -> Right <$> x

parseMessageFile :: MID -> String -> String -> IO (Either String Message)
parseMessageFile mid flags fp = BL.readFile fp >>= parseMessageString mid flags

basicMessage :: MID -> String -> Attoparsec (IO Message)
basicMessage mid flags = do
  headers <- parseHeaders
  msg     <- parseMessage (mimeTextPlain "utf8") headers

  let defaultUTCTime = UTCTime { utctDay = ModifiedJulianDay 0, utctDayTime = 0 }

  return $ do
    let (mainBodyType, mainBody) = fromMaybe ("plain", T.pack "NO TEXT") $ firstTextPart msg
    return Message
      { messageMid          = mid
      , messageDate         = fromMaybe defaultUTCTime (toUTCTime <$> listToMaybe (lookupField fDate headers))
      , messageMessageId    = fromMaybe (MsgID "") (listToMaybe (lookupField fMessageID headers))
      , messageFlags        = flags
      , messageFrom         = mconcat (lookupField fFrom headers)
      , messageTo           = mconcat (lookupField fTo headers)
      , messageCc           = mconcat (lookupField fCc headers)
      , messageBcc          = mconcat (lookupField fBcc headers)
      , messageReferences   = mconcat (lookupField fReferences headers) `union` mconcat (lookupField fInReplyTo headers)
      , messageReplyTo      = mconcat (lookupField fReplyTo headers)
      , messageSubject      = fromMaybe "" (listToMaybe (lookupField fSubject headers))
      , messageBody         = mainBody
      , messageBodyType     = mainBodyType
      , messageParts        = map typeOfPart (partList msg)
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
      updateWhere [ModifiedEMessageRef ==. key] [ModifiedEDate =. t]
      return ()
  where
    extractMessageE :: Message -> MessageE
    extractMessageE msg = MessageE
      { messageEMid       = messageMid msg
      , messageEMessageId = messageMessageId msg
      , messageEDate      = messageDate msg
      , messageESubject   = messageSubject msg
      , messageEBody      = messageBody msg
      , messageEBodyType  = messageBodyType msg
      , messageEFlags     = messageFlags msg
      }
    extractAddressEs :: Key MessageE -> Message -> [AddressE]
    extractAddressEs key msg = map (toaddr "from"    ) (messageFrom msg    )
                            ++ map (toaddr "to"      ) (messageTo msg      )
                            ++ map (toaddr "cc"      ) (messageCc msg      )
                            ++ map (toaddr "bcc"     ) (messageBcc msg     )
                            ++ map (toaddr "replyto" ) (messageReplyTo msg )
      where
        toaddr t a = AddressE
          { addressEMessageRef = key
          , addressEName = nameAddr_name a
          , addressEAddress = nameAddr_addr a
          , addressEType = t
          }
    extractReferenceEs :: Key MessageE -> Message -> [ReferenceE]
    extractReferenceEs key msg = map toref (messageReferences msg)
      where
        toref r = ReferenceE
          { referenceEMessageRef = key
          , referenceEMsgId = r
          }
    extractPartEs :: Key MessageE -> Message -> [PartE]
    extractPartEs key msg = map topart (messageParts msg)
      where
        topart p = PartE
          { partEMessageRef = key
          , partEType = p
          }
    mkModifiedE :: Key MessageE -> UTCTime -> ModifiedE
    mkModifiedE key t = ModifiedE
      { modifiedEMessageRef = key
      , modifiedEDate = t
      }

-- deleteMessage :: MID -> Update ()
deleteMessage mid = deleteCascadeWhere [MessageEMid ==. mid]
