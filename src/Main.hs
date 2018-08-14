{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import qualified Brick.Main           as M
import qualified Brick.Types          as T
import qualified Brick.Focus          as F
import qualified Brick.Widgets.Core   as Core
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

class ShowUser a where
  showUser :: a -> String

data Board = Board { id :: Int, name :: String } deriving Show
instance FromJSON Board where
  parseJSON = withObject "view" $ \o ->
    Board <$> o .: "id" <*> o .: "name"
instance ShowUser Board where
  showUser (Board id name) = name

boardsFromAPI :: Value -> Parser [Board]
boardsFromAPI = withObject "views" $ \o -> o .: "views"

ui :: Widget ()
ui = str "Hello, world!"

data Name = Edit1 | Edit2 deriving (Eq, Ord)

data AppState = AppState {
  _focusRing  :: F.FocusRing Name
  , _boards   :: L.List () Board
  , _position :: Int
  }

makeLenses ''AppState

listDrawElement :: (ShowUser a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s = if sel
                 then withAttr customAttr (str $ "<" <> s <> ">")
                 else str s
  in (selStr $ showUser a)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

drawUI :: AppState -> [T.Widget ()]
drawUI st = [ui]
    where
--        e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit1)
--        e2 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit2)
        label = str "Boards"
        items = L.list () (Vec.fromList ["a", "b", "c"])
        pos = view position st
        box = B.borderWithLabel label $
          hLimit 50 $
          vLimit 50 $
          L.renderList listDrawElement True (view boards st)
        ui = C.vCenter $
          vBox [ C.hCenter box ]

appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc []    -> M.halt st
        _ ->
          let l' = L.handleListEvent ev (view boards st)
          in l' >>= (\l -> return $ set boards l st) >>= M.continue
--        V.EvKey (V.KUp) [] ->
--          let l' = L.handleListEvent ev (view boards st)
--          in l' >>= (\l -> return $ set boards l st) >>= M.continue

--           l' <- L.handleListEvent ev (view boards st)
--           let newState = set boards l' st
--           M.continue newState
        -- V.EvKey (V.KUp) []   -> M.continue $ over position (subtract 1) $ st
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
      views' = parseMaybe boardsFromAPI body

  print views'

  let views'' = case views' of
        Just v  -> L.list () (Vec.fromList v)
        Nothing -> L.list () (Vec.empty)

      initialState = AppState {
          _boards=views'' 0
        , _focusRing=F.focusRing [Edit1, Edit2]
        , _position=0
        }

  st <- M.defaultMain theApp initialState

  pure ()
