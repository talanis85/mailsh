{-# LANGUAGE OverloadedStrings #-}
module Mailsh.MimeType
  ( MimeType (..)
  , formatMimeType
  , formatMimeTypeShort
  , mkMimeType
  , lookupMimeParam
  , withMimeParam
  , mimeApplicationOctetStream
  , mimeTextPlain
  ) where

import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import qualified Data.Text as T

data MimeType = MimeType
  { mimeType :: CI T.Text
  , mimeSubtype :: CI T.Text
  , mimeParams :: Map.Map (CI T.Text) T.Text
  }
  deriving (Show, Read, Eq)

formatMimeType :: MimeType -> T.Text
formatMimeType t = formatMimeTypeShort t <> mconcat (map formatParam (Map.toList (mimeParams t)))
  where formatParam (k,v) = ";" <> CI.foldedCase k <> "=" <> v

formatMimeTypeShort :: MimeType -> T.Text
formatMimeTypeShort t = CI.foldedCase (mimeType t) <> "/" <> CI.foldedCase (mimeSubtype t)

mkMimeType :: CI T.Text -> CI T.Text -> MimeType
mkMimeType t st = MimeType
  { mimeType = t
  , mimeSubtype = st
  , mimeParams = mempty
  }

lookupMimeParam :: CI T.Text -> MimeType -> Maybe T.Text
lookupMimeParam k = Map.lookup k . mimeParams

withMimeParam :: CI T.Text -> T.Text -> MimeType -> MimeType
withMimeParam k v t = t { mimeParams = Map.insert k v (mimeParams t) }

mimeApplicationOctetStream :: MimeType
mimeApplicationOctetStream = MimeType
  { mimeType = "application"
  , mimeSubtype = "octet-stream"
  , mimeParams = mempty
  }

mimeTextPlain :: T.Text -> MimeType
mimeTextPlain cs = MimeType
  { mimeType = "text"
  , mimeSubtype = "plain"
  , mimeParams = Map.fromList [(CI.mk "charset", cs)]
  }
