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
import           Network.Wreq
import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (fromJust)
import qualified Data.Text as Text

import           Data.ByteString.Char8  (pack)
import           System.Environment     (getEnv)

import           Shared
import           Sprint
import           Board

data ApiReturn a = ApiReturn {
  _isLast :: Bool
  , _values :: a
} deriving Show
instance (FromJSON a) => FromJSON (ApiReturn a) where
  parseJSON = withObject "return" $ \o ->
    ApiReturn <$> o .: "isLast" <*> o .: "values"

makeLenses ''ApiReturn

data AltApiReturn a = AltApiReturn {
  _aTotal :: Int
  , _aIssues :: a
  } deriving Show
instance (FromJSON a) => FromJSON (AltApiReturn a) where
  parseJSON = withObject "return" $ \o ->
    AltApiReturn <$> o .: "total" <*> o .: "issues"

data Issue = Issue {
  _iId :: String
  , _iDescription :: Maybe String
  } deriving Show
instance ShowUser Issue where
  showUser (Issue id description) = (show id) ++ " " ++ (show description)
instance FromJSON Issue where
  parseJSON = withObject "issue" $ \o -> do
    id          <- o .: "id"
    fields      <- o .: "fields"
    description <- fields .: "description"
    return (Issue id description)

data Name = Edit1 | Edit2 deriving (Eq, Ord)
data Page =
  BoardSelection
  | SprintSelection
  | ActiveSprint

data Request a =
  Unstarted
  | Started
  | Finished a

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
    SprintSelection -> [ui]
      where
        label = str "Sprints"
        box = B.borderWithLabel label $
          hLimit 50 $
          vLimit 50 $
          L.renderList listDrawElement True (view sprints st)
        ui = C.vCenter $
          vBox [ C.hCenter box ]
    ActiveSprint -> [ui]
      where
        label = str "Issues"
        box = B.borderWithLabel label $
          hLimit 100 $
          vLimit 50 $
          L.renderList listDrawElement True (view issues st)
        ui = C.vCenter $
          vBox [ C.hCenter box ]

getSprints' :: Network.Wreq.Options -> Int -> Int -> IO [Sprint]
getSprints' opts boardId offset = do
  let opts' = opts
        & param "startAt" .~ [(Text.pack $ show offset)]
        & param "maxResults" .~ ["50"]
  r <- getWith opts'
    ("https://pelotoncycle.atlassian.net/rest/agile/1.0/board/"
     ++ (show boardId)
     ++ "/sprint")

  let sprintsResponse =
        eitherDecode (r ^. responseBody) :: Either String (ApiReturn [Sprint])
      sprints = case sprintsResponse of
        Right (ApiReturn _ values) -> values
        Left e -> error e
      isLast = case sprintsResponse of
        Right (ApiReturn isLast _) -> isLast
        Left e -> error e
  allSprints <- case isLast of
    False -> do
      newSprints <- (getSprints' opts boardId (offset + 50))
      return $ sprints ++ newSprints
    True -> pure sprints
  return allSprints

getSprints :: AppState -> Int -> IO (L.List () Sprint)
getSprints state boardId = do
  let opts' = view opts state
  sprints <- getSprints' opts' boardId 0

  let sprints' = (L.list () $ Vec.fromList sprints) 0
  return sprints'

getIssues' :: Network.Wreq.Options -> Int -> Int -> IO [Issue]
getIssues' opts sprintId offset = do
  let opts' = opts
        & param "startAt" .~ [(Text.pack $ show offset)]
        & param "maxResults" .~ ["50"]
  r <- getWith opts'
    ("https://pelotoncycle.atlassian.net/rest/agile/1.0/sprint/"
     ++ (show sprintId)
     ++ "/issue")

  let issuesResponse =
        eitherDecode (r ^. responseBody) :: Either String (AltApiReturn [Issue])
      issues = case issuesResponse of
        Right (AltApiReturn _ values) -> values
        Left e -> error e
      total = case issuesResponse of
        Right (AltApiReturn total _) -> total
        Left e -> error e
  allIssues <- case total > (offset + 50) of
    False -> do
      error $ (show offset) ++ " " ++ (show total)
      newIssues <- (getIssues' opts sprintId (offset + 50))
      return $ issues ++ newIssues
    True -> pure issues
  return allIssues

getIssues :: AppState -> Int -> IO (L.List () Issue)
getIssues state sprintId = do
  let opts' = view opts state
  issues <- getIssues' opts' sprintId 0

  let issues' = (L.list () $ Vec.fromList issues) 0
  return issues'


appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent st (T.VtyEvent ev) =
  case view page st of
    BoardSelection ->
      case ev of
        V.EvKey V.KEsc   [] -> M.halt st
        V.EvKey V.KEnter [] -> do
          let boards' = (view boards st) ^. L.listElementsL
              selectedBoardIndex = fromJust $ (view boards st) ^. L.listSelectedL
              selectedBoard = boards' Vec.! selectedBoardIndex
              selectedBoardId = Board.id selectedBoard
          sprints' <- liftIO $ getSprints st selectedBoardId
          let newState = st
                & activeBoard .~ (Just selectedBoard)
                & page        .~ SprintSelection
                & sprints     .~ sprints'
          M.continue newState
        _ ->
          let l' = L.handleListEvent ev (view boards st)
          in l' >>= (\l -> return $ set boards l st) >>= M.continue
    SprintSelection ->
      case ev of
        V.EvKey V.KEsc   [] -> M.halt st
        V.EvKey V.KEnter [] -> do
          let sprints' = (view sprints st) ^. L.listElementsL
              selectedSprintIndex = fromJust $ (view sprints st) ^. L.listSelectedL
              selectedSprint = sprints' Vec.! selectedSprintIndex
              selectedSprintId = (view sId selectedSprint)
          issues' <- liftIO $ getIssues st selectedSprintId
          let newState = st
                & issues       .~ issues'
                & activeSprint .~ (Just selectedSprint)
                & page         .~ ActiveSprint
          M.continue newState
        _ ->
          let l' = L.handleListEvent ev (view sprints st)
          in l' >>= (\l -> return $ set sprints l st) >>= M.continue
    ActiveSprint ->
      case ev of
        V.EvKey V.KEsc   [] -> M.halt st
        _ ->
          let l' = L.handleListEvent ev (view issues st)
          in l' >>= (\l -> return $ set issues l st) >>= M.continue


appEvent st _ = M.continue st

appCursor :: AppState
          -> [T.CursorLocation Name]
          -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.yellow)
    , (customAttr,                   fg V.green)
    ]

sortByName :: [Board] -> [Board]
sortByName = sortBy (comparing name)

isLast' :: Value -> Parser Bool
isLast' = withObject "return" $ \o -> o .: "isLast"

values' :: Value -> Parser [Value]
values' = withObject "return" $ \o -> o .: "values"

unspool :: Network.Wreq.Options -> String -> Int -> IO ([Value])
unspool opts url offSet = do
  let opts' = opts & param "startAt" .~ [(Text.pack $ show offSet)]
  r <- asValue =<< getWith opts' url

  let stop = fromJust $ parseMaybe isLast' (r ^. responseBody)
      values = fromJust $ parseMaybe values' (r ^. responseBody)

  if stop
    then pure $ values
    else do
      v' <- unspool opts url (offSet + 50)
      pure $ values ++ v'

main :: IO ()
main = do
  auth <- getEnv "JIRA_AUTH_TOKEN"
  let authHeader = "Basic " ++ auth
      opts       = defaults & header "Authorization" .~ [pack authHeader]

  r <- unspool opts "https://pelotoncycle.atlassian.net/rest/agile/1.0/board" 0

  let views' = parseMaybe (valueToBoard) r

  let views'' = case views' of
        Just v  -> L.list () $ Vec.fromList $ sortByName v
        Nothing -> L.list () (Vec.empty)

      initialState = AppState {
          _boards       = views'' 0
        , _focusRing    = F.focusRing [Edit1, Edit2]
        , _page         = BoardSelection
        , _activeBoard  = Nothing
        , _activeSprint = Nothing
        , _issues       = (L.list () (Vec.empty)) 0
        , _sprints      = (L.list () (Vec.empty)) 0
        , _opts         = opts
        }

  st <- M.defaultMain theApp initialState

  pure ()
