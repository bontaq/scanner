{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import qualified Brick.Main           as M
import qualified Brick.Types          as T
import qualified Brick.Focus          as F
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit   as E
import qualified Brick.AttrMap        as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List   as L
import qualified Graphics.Vty         as V
import Brick.Widgets.Core
       ( vBox
       )

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Vector as Vec
import           Data.Monoid
import Network.Wreq
import Control.Lens

import           Data.ByteString.Char8  (pack)
import           System.Environment     (getEnv)

data View = View { id :: Int, name :: String } deriving Show
instance FromJSON View where
  parseJSON = withObject "view" $ \o ->
    View <$> o .: "id" <*> o .: "name"

viewsFromAPI :: Value -> Parser [View]
viewsFromAPI = withObject "views" $ \o -> o .: "views"

ui :: Widget ()
ui = str "Hello, world!"

data Name = Edit1 | Edit2 deriving (Eq, Ord)

data AppState = AppState {
  _focusRing :: F.FocusRing Name
  , _views :: [View]
  , _position :: Int
  }

makeLenses ''AppState

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s = if sel
                 then withAttr customAttr (str $ "<" <> s <> ">")
                 else str s
  in C.hCenter $ str "Item " <+> (selStr $ show a)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

drawUI :: AppState -> [T.Widget ()]
drawUI st = [ui]
    where
--        e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit1)
--        e2 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit2)
        label = str "Item" <+> str (show pos)
        items = L.list () (Vec.fromList ["a", "b", "c"])
        pos = view position st
        box = B.borderWithLabel label $
          hLimit 25 $
          vLimit 15 $
          L.renderList listDrawElement True (items pos)
        ui = C.center $
          vBox [ C.hCenter box ]

appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc []    -> M.halt st
        V.EvKey (V.KDown) [] -> M.continue =<< (L.handleListEvent ev (view views st))
        V.EvKey (V.KUp) []   -> M.continue $ over position (subtract 1) $ st
--        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
--        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
--
--        _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
--               Just Edit1 -> T.handleEventLensed st edit1 E.handleEditorEvent ev
--               Just Edit2 -> T.handleEventLensed st edit2 E.handleEditorEvent ev
--               Nothing -> return st
appEvent st _ = M.continue st

appCursor :: AppState -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.yellow)
    ]

theApp :: M.App AppState e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
  auth <- getEnv "JIRA_AUTH_TOKEN"
  let authHeader = "Basic " ++ auth
      opts       = defaults & header "Authorization" .~ [pack authHeader]

  r <- asValue =<< getWith opts "https://pelotoncycle.atlassian.net/rest/greenhopper/1.0/rapidview"

  let body   = r ^. responseBody
      views' = parseMaybe viewsFromAPI body

  print views'

  let views'' = case views' of
        Just v  -> v
        Nothing -> []

      initialState = AppState {
          _views=views''
        , _focusRing=F.focusRing [Edit1, Edit2]
        , _position=0
        }

  st <- M.defaultMain theApp initialState

  pure ()
