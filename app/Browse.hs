module Browse where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Control.Monad
import qualified Data.Vector as Vec
import Graphics.Vty

import Mailsh.Store
import Render

data Ev = EvUp | EvDown

browseMessages :: [(MessageNumber, Message)] -> IO ()
browseMessages messages = void $ defaultMain browser (initState messages)

browser :: App BrowserState Event
browser = App
  { appDraw = drawBrowser
  , appChooseCursor = showFirstCursor
  , appHandleEvent = appEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  , appLiftVtyEvent = id
  }

data BrowserState = BrowserState
  { bsList :: List (MessageNumber, Message)
  }

initState :: [(MessageNumber, Message)] -> BrowserState
initState messages = BrowserState
  { bsList = list (Name "testlist") (Vec.fromList messages) 1
  }

drawBrowser :: BrowserState -> [Widget]
drawBrowser s = [ui]
  where
    drawHeader sel (mn, msg) =
      if sel
         then withAttr listSelectedAttr (str (formatMessageSingle mn msg 100))
         else withAttr listAttr (str (formatMessageSingle mn msg 100))
    ui = renderList (bsList s) drawHeader

appEvent :: BrowserState -> Event -> EventM (Next BrowserState)
appEvent s e =
  case e of
    EvKey (KChar 'q') _ -> halt s
    ev -> do
      l' <- handleEvent ev (bsList s)
      continue $ s { bsList = l' }

theMap :: AttrMap
theMap = attrMap defAttr
  [ (listSelectedAttr, defAttr `withStyle` reverseVideo)
  ]
