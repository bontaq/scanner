{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Focus as F
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V

import Data.Aeson
import Data.Aeson.Types
import Network.Wreq
import Control.Lens

data View = View { id :: Int, name :: String } deriving Show
instance FromJSON View where
  parseJSON = withObject "view" $ \o ->
    View <$> o .: "id" <*> o .: "name"

viewsFromAPI :: Value -> Parser [View]
viewsFromAPI = withObject "views" $ \o -> o .: "views"

ui :: Widget ()
ui = str "Hello, world!"

data Name = Edit1 | Edit2 deriving (Eq, Ord)

data AppState = AppState { _focusRing :: F.FocusRing Name,  _views :: [View] }

makeLenses ''AppState

drawUI :: AppState -> [T.Widget Name]
drawUI st = [ui]
    where
--        e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit1)
--        e2 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit2)

        ui = C.center $
            str "Press Tab to switch between editors, Esc to quit."

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
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

theApp :: M.App AppState e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
  let opts = defaults & header "Authorization" .~ ["Basic"]
  r <- asValue =<< getWith opts "https://pelotoncycle.atlassian.net/rest/greenhopper/1.0/rapidview"

  let body   = r ^. responseBody
      views' = parseMaybe viewsFromAPI body

  print views'

  let views'' = case views' of
        Just v -> v
        _      -> []

      initialState = AppState { _views=views'', _focusRing=F.focusRing [Edit1, Edit2] }

  st <- M.defaultMain theApp initialState

  pure ()
