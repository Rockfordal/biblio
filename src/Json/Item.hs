{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Json.Item where

import GHC.Generics (Generic)
import qualified Data.Aeson as A
import Servant.Docs -- (markdown, docs)

data Item = Item {
    id       :: Maybe Int
  , name     :: String
  , info     :: String
  }
  deriving (Show, Generic)

instance A.ToJSON Item
instance A.FromJSON Item

instance ToSample Item where
  toSamples _ = singleSample Item { Json.Item.id = Just 1, name = "Träspik", info = "4mm Plattskalle" }
