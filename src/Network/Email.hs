module Network.Email
  ( module Network.Email.Types
  , module Network.Email.Parser
  , module Network.Email.Render
  , formatHeaders
  , firstTextPart
  ) where

import Control.Lens
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Network.Email.Parser
import Network.Email.Render
import Network.Email.Types

formatHeaders :: [IsField] -> [Field] -> String
formatHeaders filter hs = unlines $ concatMap (formatHeader hs) filter
  where
    formatHeader hs (IsField f) = map (formatSingleHeader (fieldName f)) (lookupField f hs)
    formatSingleHeader name value = name ++ ": " ++ showFieldValue value

firstTextPart :: PartTree -> Maybe (String, T.Text)
firstTextPart msg =
  let typeFilter t = mimeType t == "text"
      parts        = partList <$> collapseAlternatives typeFilter msg
  in join $ listToMaybe <$> mapMaybe (^? _PartText) <$> parts
