{-# LANGUAGE TemplateHaskell #-}
module Mailsh.Store.PersistentInstances where

import Database.Persist.TH
import Data.Time

derivePersistField "ZonedTime"
