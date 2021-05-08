{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module RichString
  ( RichString
  , Attributes
  , attrForeground
  , attrBackground
  , attrIntensity
  , richString
  , unrichString
  , withAttributes
  , mapAttributes
  , printRichString
  ) where

import Control.Lens
import Control.Monad.State
import Data.Bifunctor
import Data.String
import System.Console.ANSI

newtype RichString = RichString { getRichString :: [(Attributes, Char)] }
  deriving (Semigroup, Monoid)

data Attributes = Attributes
  { _attrForeground :: Maybe (ColorIntensity, Color)
  , _attrBackground :: Maybe (ColorIntensity, Color)
  , _attrIntensity :: ConsoleIntensity
  }

makeLenses ''Attributes

instance IsString RichString where
  fromString = richString

defaultAttributes :: Attributes
defaultAttributes = Attributes
  { _attrForeground = Nothing
  , _attrBackground = Nothing
  , _attrIntensity = NormalIntensity
  }

richString :: String -> RichString
richString = RichString . map (\x -> (defaultAttributes, x))

withAttributes :: Attributes -> RichString -> RichString
withAttributes = mapAttributes . const

mapAttributes :: (Attributes -> Attributes) -> RichString -> RichString
mapAttributes f = RichString . map (first f) . getRichString

unrichString :: RichString -> String
unrichString = map snd . getRichString

printRichString :: RichString -> IO ()
printRichString s = void $ runStateT (printRichString' s) defaultAttributes

printRichString' :: RichString -> StateT Attributes IO ()
printRichString' = mapM_ f . getRichString
  where
    f (a, c) = do
      currentFg <- use attrForeground
      when (currentFg /= a ^. attrForeground) $ do
        liftIO $ case a ^. attrForeground of
          Nothing -> putStr "\ESC[39m"
          Just x -> setSGR [SetColor Foreground (fst x) (snd x)]
        attrForeground .= a ^. attrForeground

      currentBg <- use attrBackground
      when (currentBg /= a ^. attrBackground) $ do
        liftIO $ case a ^. attrBackground of
          Nothing -> putStr "\ESC[49m"
          Just x -> setSGR [SetColor Background (fst x) (snd x)]
        attrBackground .= a ^. attrBackground

      currentIntensity <- use attrIntensity
      when (currentIntensity /= a ^. attrIntensity) $ do
        liftIO $ setSGR [SetConsoleIntensity (a ^. attrIntensity)]
        attrIntensity .= a ^. attrIntensity

      liftIO $ putChar c
