{-# LANGUAGE TemplateHaskell #-}
module Mailsh.PersistentInstances where

import Database.Persist.TH

import Mailsh.Fields
import Mailsh.MimeType

derivePersistField "MsgID"
derivePersistField "Mailbox"
derivePersistField "MimeType"
