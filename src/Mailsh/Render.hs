module Mailsh.Render
  ( renderSimple
  ) where

import Control.Monad
import Network.Email
import System.IO
import System.Process

renderSimple :: MimeType -> String -> IO String
renderSimple mimeType =
  case simpleMimeType mimeType of
    "text/html" -> renderW3m
    _           -> renderText

renderText :: String -> IO String
renderText = return

renderW3m :: String -> IO String
renderW3m s = do
  hFlush stdout
  (Just inH, Just outH, _, procH) <-
    createProcess_ "see" (shell "w3m -T text/html | cat") { std_in = CreatePipe
                                                          , std_out = CreatePipe }
  hPutStrLn inH s
  r <- hGetContents outH
  void $ waitForProcess procH
  return r
