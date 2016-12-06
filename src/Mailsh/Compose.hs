{-# LANGUAGE FlexibleContexts #-}
module Mailsh.Compose
  ( parseComposedHeaders
  , parseComposedMessage
  , renderCompose
  ) where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Except
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Maybe
import Data.Text.Encoding

import Network.Email
import Network.Email.Rfc2822
import Mailsh.Render

renderCompose :: (MonadIO m, MonadError String m) => [Field] -> String -> m String
renderCompose headers body = do
  let renderedHeaders = formatHeaders [ IsField fFrom
                                      , IsField fTo
                                      , IsField fCc
                                      , IsField fSubject
                                      , IsField fReplyTo
                                      , IsField fInReplyTo
                                      , IsField fReferences
                                      ] headers
  return (renderedHeaders ++ "\n" ++ body)

parseComposedHeaders :: Parser [Field]
parseComposedHeaders = catMaybes <$> many (headerP <* char '\n')

parseComposedMessage :: Parser T.Text
parseComposedMessage = do
  s <- takeByteString
  return (decodeUtf8 s) -- TODO: decoding actually must depend on the locale

headerP :: Parser (Maybe Field)
headerP = choice $ map try
  [ Just . mkField fSubject <$> headerNameP "Subject" unstructuredP
  , Just . mkField fFrom <$> headerNameP "From" mailboxListP
  , Just . mkField fTo <$> headerNameP "To" mailboxListP
  , Just . mkField fCc <$> headerNameP "Cc" mailboxListP
  , Just . mkField fBcc <$> headerNameP "Bcc" mailboxListP
  , Just . mkField fReplyTo <$> headerNameP "Reply-To" mailboxListP
  , Just . mkField fInReplyTo <$> headerNameP "In-Reply-To" msgidsP
  , Just . mkField fReferences <$> headerNameP "References" msgidsP
  , Just . mkField (fOptionalField "Attachment") <$> headerNameP "Attachment" unstructuredP
  , takeWhile1 (notInClass "\n") *> pure Nothing
  ]

tok :: Parser a -> Parser a
tok p = try (takeWhile isSpace >> p)

headerNameP :: String -> Parser a -> Parser a
headerNameP s p = do
  tok (string (B.pack s))
  tok (char ':')
  p

unstructuredP :: Parser String
unstructuredP = B.unpack <$> takeWhile (notInClass "\n")

msgidP :: Parser MsgID
msgidP = MsgID <$> angleAddrP

msgidsP :: Parser [MsgID]
msgidsP = tok msgidP `sepBy` tok (char ',')

mailboxP :: Parser NameAddr
mailboxP = try nameAddrP <|> fmap (NameAddr Nothing) addrSpecP
           <?> "mailbox"

nameAddrP :: Parser NameAddr
nameAddrP = do name <- maybeOption displayNameP
               addr <- angleAddrP
               return (NameAddr name addr)
            <?> "name address"

mailboxListP :: Parser [NameAddr]
mailboxListP = mailboxP `sepBy` tok (char ',')

wordP :: String -> Parser String
wordP except = B.unpack <$> tok (takeWhile1 (notInClass (" " ++ except)))

displayNameP :: Parser String
displayNameP = quotedWordP <|> (unwords <$> many1 (wordP "<"))

quotedWordP :: Parser String
quotedWordP = do
  tok (char '"')
  r <- takeWhile1 (notInClass "\"")
  tok (char '"')
  return (B.unpack r)

angleAddrP :: Parser String
angleAddrP = try (unfold (do _ <- tok (char '<')
                             r <- tok addrSpecP
                             _ <- tok (char '>')
                             return r)
                  <?> "angle address"
                 )

addrSpecP :: Parser String
addrSpecP = B.unpack <$> takeWhile1 (notInClass ">\n")
