module Roundtrip where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import Mailsh.Message

main :: IO ()
main = do
  i <- BS.getContents
  case parse (message mime) i of
    Left err -> error ("Error parsing message: " ++ err)
    Right msg -> do
      BL.putStr (renderMessage msg)
      putStr "\n"
