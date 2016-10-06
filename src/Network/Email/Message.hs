module Network.Email.Message
  ( encoded_message
  ) where

import qualified Codec.MIME.Base64 as Base64
import qualified Codec.MIME.QuotedPrintable as QuotedPrintable
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Char8.Utils
import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Encoding

import Network.Email.Rfc2822
import Network.Email.Rfc2047
import Network.Email.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

encoded_message :: MimeType -> EncodingType -> Parser Body
encoded_message t e = do
  decoded <- decodeMessage e
  case mimeType t of
    "multipart" -> case Map.lookup "boundary" (mimeParams t) of
                     Nothing -> fail "multipart without boundary"
                     Just b  -> do
                       let parts = splitMultipart b decoded
                       return (BodyTree (mapMaybe (parseMaybe bodyP) parts))
    _           -> let charsetParam = Map.lookup "charset" (mimeParams t)
                       charset = charsetParam >>= encodingFromStringExplicit
                       finalBody = case charset of
                         Nothing -> B.unpack decoded
                         Just charset -> case decodeStringExplicit charset (B.unpack decoded) of
                                          Left err -> B.unpack decoded
                                          Right v  -> v
                   in return (BodyLeaf t finalBody)

splitMultipart :: String -> B.ByteString -> [B.ByteString]
splitMultipart boundary s = fromMaybe [] (parseMaybe (multipartP boundary >> many (multipartP boundary)) s)

multipartP :: String -> Parser B.ByteString
multipartP boundary = B.pack <$> manyTill anyChar (string (B.pack ("--" ++ boundary ++ "\r\n")))

parseMaybe :: Parser a -> B.ByteString -> Maybe a
parseMaybe p b = case parseOnly p b of
                Left err -> Nothing
                Right v  -> Just v

bodyP :: Parser Body
bodyP = do
  headers <- fields
  let contentType =
        fromMaybe defaultMimeType (listToMaybe (lookupField fContentType headers))
      contentTransferEncoding =
        fromMaybe EightBit (listToMaybe (lookupField fContentTransferEncoding headers))
  encoded_message contentType contentTransferEncoding

passthrough = do
  s <- takeByteString
  return (TE.decodeUtf8 s)

decodeMessage :: EncodingType -> Parser B.ByteString
decodeMessage encoding = do
  enctext <- takeByteString
  return $ case encoding of
    QuotedPrintable -> B.pack $ QuotedPrintable.decode (B.unpack enctext)
    Base64          -> B.pack $ Base64.decodeToString (B.unpack enctext)
    EightBit        -> enctext
