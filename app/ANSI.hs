{-# LANGUAGE TemplateHaskell #-}

module ANSI
  ( ANSI
  , runANSI
  , withForegroundColor
  , withBackgroundColor
  , withIntensity
  
  -- * Re-exports
  , ConsoleIntensity (..)
  , ColorIntensity (..)
  , Color (..)
  ) where

import Control.Lens
import Control.Monad.Reader
import System.Console.ANSI

data ANSIState = ANSIState
  { _asForeground :: Maybe (ColorIntensity, Color)
  , _asBackground :: Maybe (ColorIntensity, Color)
  , _asIntensity :: ConsoleIntensity
  }

makeLenses ''ANSIState

type ANSI = ReaderT ANSIState IO

runANSI :: ANSI a -> IO a
runANSI ansi = runReaderT ansi initANSIState <* setSGR [Reset]

initANSIState = ANSIState
  { _asForeground = Nothing
  , _asBackground = Nothing
  , _asIntensity = NormalIntensity
  }

withForegroundColor :: ColorIntensity -> Color -> ANSI a -> ANSI a
withForegroundColor intensity color x = do
  liftIO $ setSGR [SetColor Foreground intensity color]
  r <- local (asForeground .~ Just (intensity, color)) x
  old <- view asForeground
  case old of
    Nothing -> liftIO $ putStr "\ESC[39m"
    Just (intensity', color') -> liftIO $ setSGR [SetColor Foreground intensity' color']
  return r

withBackgroundColor :: ColorIntensity -> Color -> ANSI a -> ANSI a
withBackgroundColor intensity color x = do
  liftIO $ setSGR [SetColor Background intensity color]
  r <- local (asBackground .~ Just (intensity, color)) x
  old <- view asBackground
  case old of
    Nothing -> liftIO $ putStr "\ESC[49m"
    Just (intensity', color') -> liftIO $ setSGR [SetColor Background intensity' color']
  return r

withIntensity :: ConsoleIntensity -> ANSI a -> ANSI a
withIntensity intensity x = do
  liftIO $ setSGR [SetConsoleIntensity intensity]
  r <- local (asIntensity .~ intensity) x
  intensity' <- view asIntensity
  liftIO $ setSGR [SetConsoleIntensity intensity']
  return r
