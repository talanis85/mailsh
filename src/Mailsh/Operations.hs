module Mailsh.Operations
  ( mailshList
  , mailshGetContents
  ) where

import Control.Lens
import Control.Monad.Trans
import Data.List
import Mailsh.Types
import Mailsh.Maildir

mailshList :: Mailsh [MaildirFile]
mailshList = do
  maildir <- view msMaildir
  sort <$> liftIO (listMaildir maildir)

mailshGetContents :: MaildirFile -> Mailsh String
mailshGetContents mf = do
  maildir <- view msMaildir
  liftIO (readFile (mf `inMaildir` maildir))
