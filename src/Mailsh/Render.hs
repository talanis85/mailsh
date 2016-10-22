module Mailsh.Render
  ( Renderer
  , renderDefault
  , renderTextParts
  , renderPartList
  , renderMainPart
  , renderType
  , outputPart
  , renderOutline
  ) where

import Control.Monad
import Data.Maybe
import Network.Email
import System.IO
import System.Process
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BL

type Renderer = Body -> IO String

renderDefault :: Renderer
renderDefault b = do
  mainPart <- renderMainPart b
  let sep = replicate 78 '-' 
  attachments <- renderPartList b
  return (mainPart ++ "\n" ++ sep ++ "\n" ++ attachments)

renderTextParts :: Renderer
renderTextParts b =
  let typeFilter = anyF [isSimpleMimeType "text/plain", isSimpleMimeType "text/html"]
      bodies     = fromMaybe [] $ textBodies <$> collapseAlternatives typeFilter b
  in concat <$> mapM (uncurry renderType) bodies

renderMainPart :: Renderer
renderMainPart b =
  let typeFilter = anyF [isSimpleMimeType "text/plain", isSimpleMimeType "text/html"]
      body       = head <$> textBodies <$> collapseAlternatives typeFilter b
  in case body of
       Nothing -> return ""
       Just (t, s) -> renderType t s

renderPartList :: Renderer
renderPartList b =
  let bodies = allBodies b
  in concat <$> mapM renderPartListElement (zip ([1..] :: [Int]) bodies)
    where
      renderPartListElement (n, Left (t,s))  = return $ printf "%d: text/%s\n" n t
      renderPartListElement (n, Right (t,s)) = return $ printf "%d: %s\n" n (renderMimeType t)
      renderMimeType t = case lookupMimeParam "name" t of
                           Nothing -> simpleMimeType t
                           Just n  -> printf "%s (%s)" (simpleMimeType t) n

outputPart :: Int -> Body -> IO ()
outputPart n b =
  let typeFilter = const True
      bodies     = allBodies b
  in case bodies !! (n-1) of
       Left (t, s) -> renderType t s >>= putStrLn
       Right (t, s) -> BL.putStr s

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
