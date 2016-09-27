{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
module Mailsh.Message
  ( Field
  , IsField (IsField)
  , _OptionalField, _From, _Sender, _ReturnPath, _ReplyTo
  , _To, _Cc, _Bcc, _MessageID, _InReplyTo, _References
  , _Subject, _Comments, _Keywords, _Date
  -- , _Resent*
  , _Received, _ObsReceived
  , lookupField, lookupOptionalField, filterFields
  , simpleContentType
  , showField
  , parseHeaders
  , getMessageFileHeaders
  , formatNameAddr
  ) where

import Control.Applicative
import Control.Lens
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as B
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Pipes
import qualified Pipes.Attoparsec as PA
import qualified Pipes.ByteString as PB
import qualified Pipes.Parse as PP
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Rfc2822 as EH
import Text.ParserCombinators.Parsec.Rfc2822 (Field (..))
import System.IO

makePrisms ''Field

data IsField = forall a. IsField (Prism' Field a)

lookupField :: Prism' Field a -> [Field] -> [a]
lookupField p = mapMaybe (^? p)

isn't' :: IsField -> Field -> Bool
isn't' (IsField x) f = isn't x f

filterFields :: [IsField] -> [Field] -> [Field]
filterFields f = filter (\x -> or (map (not . flip isn't' x) f))

lookupOptionalField :: String -> [Field] -> [String]
lookupOptionalField key = map (dropWhile (== ' ') . snd)
                        . filter ((== key) . fst)
                        . lookupField _OptionalField

simpleContentType :: String -> String
simpleContentType = takeWhile (\x -> x /= ' ' && x /= ';')

parseHeaders :: (Monad m) => PP.Parser B.ByteString m [Field]
parseHeaders = do
  headerPart' <- PA.parse $ do
    lines <- many $ do
      x <- AP.takeWhile1 (AP.notInClass "\n")
      nl <- AP.take 1
      return (x <> nl)
    let fixNL 10 = B.pack [13, 10]
        fixNL x  = B.pack [x]
    return (B.concatMap fixNL (mconcat lines))
    -- TODO clean this up
  case headerPart' of
    Nothing -> return []
    Just (Left err) -> return []
    Just (Right headerPart) ->
      case Parsec.parse EH.fields "" (T.unpack (TE.decodeUtf8 headerPart)) of
        Left err -> error (show err) >> return []
        Right fields -> return fields

getMessageFileHeaders :: FilePath -> IO [Field]
getMessageFileHeaders fp =
  withFile fp ReadMode $ \hIn ->
    PP.evalStateT parseHeaders (PB.fromHandle hIn)

showField :: Field -> String
showField = show

formatNameAddr :: EH.NameAddr -> String
formatNameAddr na = case EH.nameAddr_name na of
                      Nothing -> EH.nameAddr_addr na
                      Just name -> name ++ " <" ++ EH.nameAddr_addr na ++ ">"
