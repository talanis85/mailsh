{-# LANGUAGE OverloadedStrings #-}
module Mailsh.MimeRender
  ( renderType
  ) where

import           Data.CaseInsensitive (CI)
import qualified Data.Text as T
import           Text.Pandoc

renderType :: CI T.Text -> T.Text -> T.Text
renderType t = case t of
  "html" -> renderHtml
  _      -> renderText

renderText :: T.Text -> T.Text
renderText = id

renderHtml :: T.Text -> T.Text
renderHtml s =
  let readOpts = def
        { readerStandalone = True
        }
      writeOpts = def
        { writerReferenceLinks = True
        }
      result = writeAsciiDoc writeOpts <$> readHtml readOpts (T.unpack s)
  in case result of
       Left err -> T.pack ("PandocError: " ++ show err)
       Right s' -> T.pack s'
