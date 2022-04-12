{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Browse
  ( browseStoredMessages
  ) where

import Control.Lens
import Control.Zipper
import Data.Maybe (fromMaybe)
import Control.Monad.Trans
import qualified Data.Text as T
import Data.Time
import Disguise.Cairo
import Disguise.Gtk.Main
import Disguise.Gtk.Event
import Text.Wrap

import Data.Reparser
import Mailsh.Message
import Mailsh.MimeRender
import Mailsh.Store.Message

import System.IO.Unsafe

import Format
import RichString

{-# NOINLINE defaultFont #-}
defaultFont :: FontDescription
defaultFont = unsafePerformIO $ loadFont "monospace 10"

white :: RGB
white = RGB 1 1 1

red :: RGB
red = RGB 1 0 0

type ListZipper a = Top :>> [a] :>> a

data Model = Model
  { _modelMessageZipper :: Maybe (ListZipper StoredMessage)
  }

makeLenses ''Model

browseStoredMessages :: RichFormat -> [StoredMessage] -> IO ()
browseStoredMessages format msgs = do
  tz <- getCurrentTimeZone
  -- style <- defaultStyleWithFont "monospace 10"
  pureMain (initModel msgs) updateModel (pad 10 . ui tz format)

initModel :: [StoredMessage] -> Model
initModel messages = Model
  { _modelMessageZipper = within traversed (zipper messages)
  }

updateModel :: Event -> Model -> Model
updateModel ev = case ev of
  LoadEvent -> id
  KeyEvent k -> case keyName k of
    "Up" -> modelMessageZipper . traversed %~ tug leftward
    "Down" -> modelMessageZipper . traversed %~ tug rightward
    _ -> id

ui :: TimeZone -> RichFormat -> Model -> CairoWidget (V Dim) (V Dim) IO
ui tz format model = tabularV
  [ (0.3, pad 5 (box white (pad 5 (messageList tz format model))))
  , (0.7, messageView tz model)
  ]

messageList :: TimeZone -> RichFormat -> Model -> CairoWidget (V Dim) (V Dim) IO
messageList tz format model = list white red defaultFont (model ^. modelMessageZipper) display
  where
    cols = 80
    display = unrichString . formatStoredMessage tz format cols

messageView :: TimeZone -> Model -> CairoWidget (V Dim) (V Dim) IO
messageView tz model = headerView `topOf` bodyView
  where
    headerView = messageHeaders tz (model ^? modelMessageZipper . traversed . focus)
    bodyView = messageBody (model ^? modelMessageZipper . traversed . focus)

messageBody :: Maybe StoredMessage -> CairoWidget (V Dim) (V Dim) IO
messageBody Nothing = space
messageBody (Just msg) =
  let content = case msg ^. body . storedMainBody . charsetDecoded' defaultCharsets of
        Left err -> show err
        Right msg' -> T.unpack (renderType (msg ^. contentType) (msg' ^. body))
  in alignLeft (alignTop (text white defaultFont content))

messageHeaders :: TimeZone -> Maybe StoredMessage -> CairoWidget (V Dim) (F Dim) IO
messageHeaders tz Nothing = spaceH
messageHeaders tz (Just msg) =
          headerView "From" (T.unpack (reprint addressesParser (msg ^. headerFrom defaultCharsets)))
  `topOf` headerView "To" (T.unpack (reprint addressesParser (msg ^. headerTo defaultCharsets)))
  `topOf` headerView "Subject" (T.unpack (fromMaybe "(no subject)" (msg ^. headerSubject defaultCharsets)))
  `topOf` headerView "Date" (fromMaybe "(no date)" (localDate ^? traversed . to (formatTime defaultTimeLocale dateTimeFormat)))
    where
      localDate = utcToZonedTime tz <$> zonedTimeToUTC <$> msg ^. headerDate
      headerView :: String -> String -> CairoWidget (V Dim) (F Dim) IO
      headerView name value = tabularH
        [ (0.3, alignLeft (text white defaultFont name))
        , (0.7, alignLeft (text white defaultFont value))
        ]

dateTimeFormat :: String
dateTimeFormat = "%a %b %d %Y %H:%M"
