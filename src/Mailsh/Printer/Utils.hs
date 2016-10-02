module Mailsh.Printer.Utils
  ( forAllM
  , utf8decoder
  , formatHeaders
  , ignoreEither
  , ignoreMaybe
  , ignoreError
  ) where

import Control.Monad.Trans
import Data.List
import qualified Pipes.Text.IO as PTIO
import qualified Pipes.Parse as PP
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Network.Email
import Mailsh.Printer.Types

forAllM :: (Monad m) => (a -> m ()) -> PP.Parser a m ()
forAllM f = PP.foldAllM (const f) (return ()) (const (return ()))

utf8decoder :: Printer' ()
utf8decoder = forAllM (liftIO . TIO.putStr . TE.decodeUtf8)

formatHeaders :: [IsField] -> [Field] -> String
formatHeaders filter hs = unlines $ map (formatHeader hs) filter
  where
    formatHeader hs (IsField f) = unlines $ map (formatSingleHeader (fieldName f)) (lookupField f hs)
    formatSingleHeader name value = name ++ ": " ++ showFieldValue value

ignoreEither :: Monoid b => Either a b -> b
ignoreEither (Right v) = v
ignoreEither (Left _ ) = mempty

ignoreMaybe :: Monoid a => Maybe a -> a
ignoreMaybe (Just v) = v
ignoreMaybe Nothing = mempty

ignoreError :: Monoid b => Maybe (Either a b) -> b
ignoreError (Just (Right v)) = v
ignoreError _ = mempty
