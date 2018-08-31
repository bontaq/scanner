{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Sprint where

import           Data.Aeson
import           Control.Lens (makeLenses)
import           Shared

data Sprint = Sprint {
  _sId :: Int
  , _sName :: String
  , _sState :: String
  } deriving Show

instance FromJSON Sprint where
  parseJSON = withObject "sprint" $ \o ->
    Sprint <$> o .: "id" <*> o .: "name" <*> o .: "state"

instance ShowUser Sprint where
  showUser (Sprint id name state) = name ++ " " ++ state

makeLenses ''Sprint
