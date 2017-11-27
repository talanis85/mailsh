module Mailsh.MimeRender
  ( renderType
  ) where

import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Email
import System.IO
import System.Process

w3mPath :: FilePath
w3mPath = "/usr/bin/w3m"

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

renderType :: String -> T.Text -> IO String
renderType t = case t of
  "html" -> renderW3m
  _      -> renderText

renderText :: T.Text -> IO String
renderText = return . T.unpack

renderW3m :: T.Text -> IO String
renderW3m s = do
  let w3m = w3mPath ++ " -T text/html -o display_link_number=1 -cols 10000 | cat"
  hFlush stdout
  (Just inH, Just outH, _, procH) <-
    createProcess_ "see" (shell w3m) { std_in = CreatePipe
                                     , std_out = CreatePipe }
  T.hPutStrLn inH s
  r <- hGetContents outH
  void $ waitForProcess procH
  return r
