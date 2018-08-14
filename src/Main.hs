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
import           Data.List (sortBy)
import           Data.Ord  (comparing)
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

data Name = Edit1 | Edit2 deriving (Eq, Ord)
data Page =
  BoardSelection
  | ActiveSprint

data AppState = AppState {
  _focusRing     :: F.FocusRing Name
  , _boards      :: L.List () Board
  , _page        :: Page
  , _activeBoard :: Maybe Int
  }

makeLenses ''AppState

theApp :: M.App AppState e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }


listDrawElement :: (ShowUser a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s = if sel
                 then withAttr customAttr (str $ "> " <> s)
                 else str s
  in (selStr $ showUser a)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

drawUI :: AppState -> [T.Widget ()]
drawUI st =
  case view page st of
    BoardSelection -> [ui]
      where
        label = str "Boards"
        box = B.borderWithLabel label $
          hLimit 50 $
          vLimit 50 $
          L.renderList listDrawElement True (view boards st)
        ui = C.vCenter $
          vBox [ C.hCenter box ]
    ActiveSprint -> [ui]
      where
        ui = vBox [ str "hello" ]

appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent st (T.VtyEvent ev) =
  case view page st of
    BoardSelection ->
      case ev of
        V.EvKey V.KEsc []    -> M.halt st
        _ ->
          let l' = L.handleListEvent ev (view boards st)
          in l' >>= (\l -> return $ set boards l st) >>= M.continue
appEvent st _ = M.continue st

appCursor :: AppState -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.yellow)
    , (customAttr,                   fg V.cyan)
    ]


sortByName :: [Board] -> [Board]
sortByName = sortBy (comparing name)

main :: IO ()
main = do
  auth <- getEnv "JIRA_AUTH_TOKEN"
  let authHeader = "Basic " ++ auth
      opts       = defaults & header "Authorization" .~ [pack authHeader]

  r <- asValue =<< getWith opts "https://pelotoncycle.atlassian.net/rest/greenhopper/1.0/rapidview"

  let body   = r ^. responseBody
      views' = parseMaybe boardsFromAPI body

  let views'' = case views' of
        Just v  -> L.list () $ Vec.fromList $ sortByName v
        Nothing -> L.list () (Vec.empty)

      initialState = AppState {
          _boards      = views'' 0
        , _focusRing   = F.focusRing [Edit1, Edit2]
        , _page        = BoardSelection
        , _activeBoard = Nothing
        }

  st <- M.defaultMain theApp initialState

  pure ()
