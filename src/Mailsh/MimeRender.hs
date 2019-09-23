{-# LANGUAGE OverloadedStrings #-}
module Mailsh.MimeRender
  ( renderType
  ) where

import           Data.CaseInsensitive (CI)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Text.HTML.DOM
import           Text.PrettyPrint
import qualified Text.XML as XML

renderType :: CI T.Text -> T.Text -> T.Text
renderType t = case t of
  "html" -> renderHtml
  _      -> renderText

renderText :: T.Text -> T.Text
renderText = id

renderHtml :: T.Text -> T.Text
renderHtml s = T.pack (render (printElement (XML.documentRoot (parseLT (TL.fromStrict s)))))

printElement :: XML.Element -> Doc
printElement el = printNodes (XML.elementNodes el)

printNodes :: [XML.Node] -> Doc
printNodes nodes = vcat (map printNode nodes)

printNode :: XML.Node -> Doc
printNode (XML.NodeElement el) = printElement el
printNode (XML.NodeContent t) = text (T.unpack t)
printNode _ = mempty
