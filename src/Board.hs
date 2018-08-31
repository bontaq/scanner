{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Board where

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Shared

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
