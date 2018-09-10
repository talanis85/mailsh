{-# LANGUAGE TemplateHaskell #-}
module Mailsh.PersistentInstances where

import Database.Persist.TH

import Network.Email

derivePersistField "MsgID"
derivePersistField "NameAddr"
derivePersistField "MimeType"
