module Mailsh.Render
  ( Renderer
  , renderPreview
  , renderDefault
  , renderTextParts
  , renderPartList
  , renderMainPart
  , renderType
  , outputPart
  , renderOutline
  ) where

import Control.Monad
import Control.Lens
import Data.Maybe
import Network.Email
import System.IO
import System.Process
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

type Renderer = PartTree -> IO String

renderPreview :: Renderer
renderPreview b = do
  mainPart <- renderMainPart b
  return (unlines (take 5 (lines mainPart)))

renderDefault :: Renderer
renderDefault b = do
  mainPart <- renderMainPart b
  attachments <- renderPartList b
  return (mainPart ++ "\n--\nParts:\n" ++ attachments)

renderTextParts :: Renderer
renderTextParts b =
  let typeFilter = anyF [isSimpleMimeType "text/plain", isSimpleMimeType "text/html"]
      parts      = partList <$> collapseAlternatives typeFilter b
      textParts  = fromMaybe [] $ mapMaybe (^? _PartText) <$> parts
  in concat <$> mapM (uncurry renderType) textParts

renderMainPart :: Renderer
renderMainPart b =
  let typeFilter = anyF [isSimpleMimeType "text/plain", isSimpleMimeType "text/html"]
      parts      = partList <$> collapseAlternatives typeFilter b
      part       = head <$> mapMaybe (^? _PartText) <$> parts
  in case part of
       Nothing -> return ""
       Just (t, s) -> renderType t s

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

outputPart :: Int -> PartTree -> IO ()
outputPart n b =
  let typeFilter = const True
      parts      = partList b
  in case parts !! (n-1) of
       PartText t s   -> renderType t s >>= putStrLn
       PartBinary t s -> BS.putStr s

renderType :: String -> T.Text -> IO String
renderType t = case t of
  "html" -> renderW3m
  _      -> renderText

renderText :: T.Text -> IO String
renderText = return . T.unpack

renderW3m :: T.Text -> IO String
renderW3m s = do
  hFlush stdout
  (Just inH, Just outH, _, procH) <-
    createProcess_ "see" (shell "w3m -T text/html | cat") { std_in = CreatePipe
                                                          , std_out = CreatePipe }
  T.hPutStrLn inH s
  r <- hGetContents outH
  void $ waitForProcess procH
  return r

renderOutline :: Renderer
renderOutline = return . outline
