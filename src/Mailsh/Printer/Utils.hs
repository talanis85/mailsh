module Mailsh.Printer.Utils
  ( forAllM
  , utf8decoder
  , parseOrFail
  , ignoreEither
  , ignoreMaybe
  , ignoreError
  ) where

import Control.Monad.Trans
import Data.List
import qualified Pipes.Text.IO as PTIO
import qualified Pipes.Parse as PP
import qualified Pipes.Attoparsec as PA
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Network.Email
import Mailsh.Printer.Types

forAllM :: (Monad m) => (a -> m ()) -> PP.Parser a m ()
forAllM f = PP.foldAllM (const f) (return ()) (const (return ()))

utf8decoder :: Printer' ()
utf8decoder = forAllM (liftIO . TIO.putStr . TE.decodeUtf8)

parseOrFail :: AP.Parser a -> Printer' a
parseOrFail p = do
  r <- PA.parse p
  case r of
    Nothing -> fail "No data"
    Just (Left err) -> fail (show err)
    Just (Right v) -> return v

ignoreEither :: Monoid b => Either a b -> b
ignoreEither (Right v) = v
ignoreEither (Left _ ) = mempty

ignoreMaybe :: Monoid a => Maybe a -> a
ignoreMaybe (Just v) = v
ignoreMaybe Nothing = mempty

ignoreError :: Monoid b => Maybe (Either a b) -> b
ignoreError (Just (Right v)) = v
ignoreError _ = mempty
