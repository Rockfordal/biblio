{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Json.Shelf where

import GHC.Generics (Generic)
import qualified Data.Aeson as A
import Servant.Docs -- (markdown, docs)

data Shelf = Shelf {
    id       :: Maybe Int
  , name     :: String
  , size     :: Int
  -- , room_id  :: Maybe Int
  }
  deriving (Show, Generic)

instance A.ToJSON Shelf
instance A.FromJSON Shelf

instance ToSample Shelf where
  toSamples _ = singleSample Shelf { Json.Shelf.id = Just 1, name = "A01", size = 10 }
