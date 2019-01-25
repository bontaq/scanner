{-# LANGUAGE OverloadedStrings #-}
module Shared where

import           Brick
import qualified Brick.AttrMap        as A
import qualified Brick.Widgets.List   as L
import           Data.Monoid

class ShowUser a where
  showUser :: a -> String

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

listDrawElement :: (ShowUser a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s = if sel
                 then withAttr customAttr (str $ "> " <> s)
                 else str s
  in (selStr $ showUser a)
