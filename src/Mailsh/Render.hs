module Mailsh.Render
  ( Renderer
  , renderMainPart
  , renderOutline
  ) where

import Control.Monad
import Network.Email
import System.IO
import System.Process
import Text.Printf
import qualified Data.ByteString.Lazy.Char8 as BL

type Renderer = Body -> IO String

renderMainPart :: Renderer
renderMainPart b =
  let (t, s) = head $ bodiesOf (anyF [isSimpleMimeType "text/plain", isSimpleMimeType "text/html"]) b
  in case simpleMimeType t of
    "text/html" -> renderW3m s
    _           -> renderText s

renderText :: BL.ByteString -> IO String
renderText = return . BL.unpack

renderW3m :: BL.ByteString -> IO String
renderW3m s = do
  hFlush stdout
  (Just inH, Just outH, _, procH) <-
    createProcess_ "see" (shell "w3m -T text/html | cat") { std_in = CreatePipe
                                                          , std_out = CreatePipe }
  BL.hPutStrLn inH s
  r <- hGetContents outH
  void $ waitForProcess procH
  return r

renderOutline :: Renderer
renderOutline = return . outline
