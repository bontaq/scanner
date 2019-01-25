module Models where

import qualified Brick.Widgets.List   as L
import qualified Brick.Focus          as F

import Board

data AppState = AppState {
  _focusRing      :: F.FocusRing Name
  , _boards       :: L.List () Board
  , _page         :: Page
  , _activeBoard  :: Maybe Board
  , _activeSprint :: Maybe Sprint
  , _issues       :: L.List () Issue
  , _sprints      :: L.List () Sprint
  , _opts         :: Network.Wreq.Options
  }
