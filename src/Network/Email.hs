module Network.Email
  ( module Network.Email.Types
  , module Network.Email.Parser
  , module Network.Email.Render
  , formatHeaders
  ) where

import Network.Email.Parser
import Network.Email.Render
import Network.Email.Types

formatHeaders :: [IsField] -> [Field] -> String
formatHeaders filter hs = unlines $ concatMap (formatHeader hs) filter
  where
    formatHeader hs (IsField f) = map (formatSingleHeader (fieldName f)) (lookupField f hs)
    formatSingleHeader name value = name ++ ": " ++ showFieldValue value
