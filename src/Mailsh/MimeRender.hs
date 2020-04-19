{-# LANGUAGE OverloadedStrings #-}
module Mailsh.MimeRender
  ( renderType
  ) where

import           Control.Monad.State
import           Data.CaseInsensitive (CI)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Text.HTML.DOM
import           Text.PrettyPrint.Leijen hiding ((<$>))
import qualified Text.XML as XML

renderType :: CI T.Text -> T.Text -> T.Text
renderType t = case t of
  "html" -> renderHtml
  _      -> renderText

renderText :: T.Text -> T.Text
renderText = id

renderHtml :: T.Text -> T.Text
renderHtml s = T.pack $ displayS (renderPretty 1.0 100 $ printXML (XML.documentRoot (parseLT (TL.fromStrict s)))) ""

nontextElements :: [XML.Name]
nontextElements = [ "script", "style", "head", "img" ]

inlineElements :: [XML.Name]
inlineElements = [ "i", "b", "u", "strong", "span", "td" ]

linkElements :: [XML.Name]
linkElements = [ "a" ]

linebreakElements :: [XML.Name]
linebreakElements = [ "br" ]

data Token
  = Word T.Text
  | Link T.Text
  | Linebreak
  | Paragraph

compactBreaks :: [Token] -> [Token]
compactBreaks [] = []
compactBreaks (Linebreak : Linebreak : xs) = compactBreaks (Linebreak : xs)
compactBreaks (Paragraph : Paragraph : xs) = compactBreaks (Paragraph : xs)
compactBreaks (x : xs) = x : compactBreaks xs

tokensToDoc :: [Token] -> Doc
tokensToDoc tokens =
  let (doc, (_, links)) = runState (foldM tokenToDoc mempty tokens) (1, [])
  in doc <> linebreak <> linebreak <> vcat (map linkToDoc links)
  where
    tokenToDoc :: Doc -> Token -> State (Int, [(Int, T.Text)]) Doc
    tokenToDoc doc (Word word) = return $ doc </> text (T.unpack word)
    tokenToDoc doc Linebreak   = return $ doc <> linebreak
    tokenToDoc doc Paragraph   = return $ doc <> linebreak <> linebreak
    tokenToDoc doc (Link href) = do
      (n, links) <- get
      put (n+1, (links ++ [(n, href)]))
      return (doc </> text ("[" ++ show n ++ "]"))
    linkToDoc :: (Int, T.Text) -> Doc
    linkToDoc (n, href) = text ("[" ++ show n ++ "] " ++ T.unpack href)

printXML :: XML.Element -> Doc
printXML = tokensToDoc . compactBreaks . printElement

printElement :: XML.Element -> [Token]
printElement el = printElement' (XML.elementName el)
  where
    printElement' name
      | name `elem` nontextElements = []
      | name `elem` inlineElements = concatMap printNode (XML.elementNodes el)
      | name `elem` linkElements = case Map.lookup "href" (XML.elementAttributes el) of
                                     Nothing -> concatMap printNode (XML.elementNodes el)
                                     Just href -> if "#" `T.isPrefixOf` href
                                                     then concatMap printNode (XML.elementNodes el)
                                                     else Link href : concatMap printNode (XML.elementNodes el)
      | name `elem` linebreakElements = Linebreak : concatMap printNode (XML.elementNodes el)
      | otherwise = Paragraph : concatMap printNode (XML.elementNodes el)

printNode :: XML.Node -> [Token]
printNode (XML.NodeElement el) = printElement el
printNode (XML.NodeContent t) = map Word (T.words t)
printNode _ = []
