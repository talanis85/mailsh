module Mailsh.MimeRender
  ( renderType
  ) where

import qualified Data.Text as T
import Text.Pandoc

{-
renderPartList :: Renderer
renderPartList b =
  let parts = partList b
  in concat <$> mapM renderPart (zip ([1..] :: [Int]) parts)
    where
      renderPart (n, PartText t s)  = return $ printf "%d: text/%s\n" n t
      renderPart (n, PartBinary t s) = return $ printf "%d: %s\n" n (renderMimeType t)
      renderMimeType t = case lookupMimeParam "name" t of
                           Nothing -> simpleMimeType t
                           Just n  -> printf "%s (%s)" (simpleMimeType t) n
-}

renderType :: String -> T.Text -> String
renderType t = case t of
  "html" -> renderHtml
  _      -> renderText

renderText :: T.Text -> String
renderText = T.unpack

renderHtml :: T.Text -> String
renderHtml s =
  let readOpts = def
        { readerStandalone = True
        }
      writeOpts = def
        { writerReferenceLinks = True
        }
      result = writePlain writeOpts <$> readHtml readOpts (T.unpack s)
  in case result of
       Left err -> "PandocError: " ++ show err
       Right s' -> s'
