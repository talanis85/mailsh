{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Mailsh.Store.Message
  ( Stored (..)
  , StoredMessage
  , readStoredMessage
  , withMainPart
  , storedMainBody
  , storedMid
  , storedNumber
  , storedFlags
  , storedParts
  , storedSource
  , partStoreInfo

  , storedAttachments
  , storedFiles
  ) where

import Control.Applicative
import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

import Mailsh.Maildir
import Mailsh.Message
import Mailsh.Store.Schema

data Stored = Stored
  { _storedMainBody :: ByteEntity
  , _storedFlags :: String
  , _storedMid :: Maybe MID
  , _storedNumber :: Maybe StoreNumber
  , _storedParts :: [(Maybe ContentDisposition, ContentType)]
  , _storedSource :: IO BS.ByteString
  }

makeLenses ''Stored

type StoredMessage = Message EncStateWire Stored

type instance MessageContext Stored = EncStateWire

readStoredMessage
  :: Maybe MID -> Maybe StoreNumber -> String -> IO BS.ByteString -> IO (Either String StoredMessage)
readStoredMessage mid num flags reader = do
  bs <- reader
  return $ makeStored <$> parse (message mime) bs
  where
    makeStored msg = Message (msg ^. headers) $ Stored
      { _storedMainBody = fromMaybe (defaultMessage (msg ^. headers)) $
          msg ^? elementOf entities 0 . transferDecoded' . traversed
      , _storedMid = mid
      , _storedNumber = num
      , _storedFlags = flags
      , _storedParts = msg ^.. entities . partStoreInfo
      , _storedSource = reader
      }
    defaultMessage headers = Message headers (BS.empty)

withMainPart :: StoredMessage -> ByteEntity -> StoredMessage
withMainPart msg be = Message (msg ^. headers) $ (msg ^. body) { _storedMainBody = be }

partStoreInfo :: Getter WireEntity (Maybe ContentDisposition, ContentType)
partStoreInfo = to (\x -> (x ^. contentDisposition, x ^. contentType))

storedAttachments :: IndexedFold Int Stored (Maybe T.Text, ContentType)
storedAttachments = (storedParts . traversed) <. folding isAttachment
  where
    isAttachment :: (Maybe ContentDisposition, ContentType) -> Maybe (Maybe T.Text, ContentType)
    isAttachment (Nothing, _) = Nothing
    isAttachment (Just dis, ct)
      | dis ^. dispositionType == Attachment =
          Just ((dis ^? filename defaultCharsets) <|> (ct ^? filename defaultCharsets), ct)
      | otherwise = Nothing

-- | Variant of 'storedAttachments' that also traverses inline parts
--   that have a filename.
storedFiles :: IndexedFold Int Stored (Maybe T.Text, ContentType)
storedFiles = (storedParts . traversed) <. folding isAttachment
  where
    isAttachment :: (Maybe ContentDisposition, ContentType) -> Maybe (Maybe T.Text, ContentType)
    isAttachment (Nothing, _) = Nothing
    isAttachment (Just dis, ct)
      | dis ^. dispositionType == Attachment ||
        dis ^? filename defaultCharsets /= Nothing ||
        ct ^? filename defaultCharsets /= Nothing =
          Just ((dis ^? filename defaultCharsets) <|> (ct ^? filename defaultCharsets), ct)
      | otherwise = Nothing
