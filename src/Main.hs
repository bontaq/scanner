{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Focus as F
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
-- import qualified Graphics.Vty as V
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import GHC.Word
import Data.Default
import Data.Aeson.Lens (key, _String)
import Data.Aeson
import Data.Aeson.Types
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Char8
import Network.Wreq
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Maybe

data View = View { id :: Int, name :: String } deriving Show
instance FromJSON View where
  parseJSON = withObject "view" $ \o ->
    View <$> o .: "id" <*> o .: "name"

views :: Value -> Parser [View]
views = withObject "views" $ \o -> o .: "views"

ui :: Widget ()
ui = str "Hello, world!"

data AppState = AppState { _views :: [View] }

data Name = Edit1 | Edit2

drawUI :: AppState -> [T.Widget Name]
drawUI st = [ui]
    where
        e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit1)
        e2 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit2)

        ui = C.center $
            (str "Input 1 (unlimited): " <+> (hLimit 30 $ vLimit 5 e1)) <=>
            str " " <=>
            (str "Input 2 (limited to 2 lines): " <+> (hLimit 30 e2)) <=>
            str " " <=>
            str "Press Tab to switch between editors, Esc to quit."

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev

        _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
               Just Edit1 -> T.handleEventLensed st edit1 E.handleEditorEvent ev
               Just Edit2 -> T.handleEventLensed st edit2 E.handleEditorEvent ev
               Nothing -> return st
appEvent st _ = M.continue st

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
      views' = parseMaybe Main.views body
  -- let v = r >>= \x -> return $ parseMaybe Main.views x
  print views'

  let app = App { }
      views'' = case views' of
        Just v -> v
        _      -> []

      initialState = AppState { _views=views'' }

  pure ()
  -- a <- Main.views r
  -- print z
  -- simpleMain ui
