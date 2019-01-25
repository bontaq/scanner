{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Board where

import           Brick (vLimit, hLimit)
import           Brick.Widgets.Core (vBox)
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Shared
import           Models

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List   as L
import qualified Brick.Widgets.Center as C
import qualified Brick.Types          as T

data Board = Board {
  id :: Int,
  name :: String
  } deriving Show

instance FromJSON Board where
  parseJSON = withObject "view" $ \o ->
    Board <$> o .: "id" <*> o .: "name"

instance ShowUser Board where
  showUser (Board id name) = name

boardsFromAPI :: Value -> Parser [Board]
boardsFromAPI = withObject "values" $ \o -> o .: "values"

valueToBoard :: [Value] -> Parser [Board]
valueToBoard = mapM parseJSON

view :: AppState -> [T.Widget ()]
view st = [ui]
  where
    label = str "Boards"
    box = B.borderWithLabel label $
      hLimit 50 $
      vLimit 50 $
      L.renderList listDrawElement True (view boards st)
    ui = C.vCenter $
      vBox [ C.hCenter box ]
