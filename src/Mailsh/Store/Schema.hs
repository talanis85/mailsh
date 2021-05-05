{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Mailsh.Store.Schema where

import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Time
import Numeric.Natural

import Mailsh.Maildir
import Mailsh.Store.PersistentInstances ()

--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
MessageE
  mid         MID
  date        ZonedTime Maybe
  messageId   T.Text Maybe
  subject     T.Text Maybe
  body        B.ByteString
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
  msgId       T.Text
PartE
  messageRef  MessageEId
  disposition T.Text Maybe
  type        T.Text
KeywordE
  messageRef  MessageEId
  keyword     T.Text
  |]

--------------------------------------------------------------------------------

newtype StoreNumber = StoreNumber { getStoreNumber :: MessageEId }

storeNumber :: (Integral a) => a -> StoreNumber
storeNumber n = StoreNumber (toSqlKey (fromIntegral n))

storeNumberToNatural :: StoreNumber -> Natural
storeNumberToNatural (StoreNumber x) = fromIntegral (fromSqlKey x)

instance Read StoreNumber where
  readsPrec n s = map (\(a,b) -> (StoreNumber (toSqlKey a), b)) (readsPrec n s)

instance Show StoreNumber where
  show = show . fromSqlKey . getStoreNumber
