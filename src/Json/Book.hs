{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Json.Book where

import GHC.Generics (Generic)
import qualified Data.Aeson as A

data Book = Book {
    id       :: Maybe Int
  , title    :: String
  , author   :: String
  , content  :: String
  , year     :: Int
  , user_id  :: Maybe Int
  }
  deriving (Show, Generic)

instance A.ToJSON Book
instance A.FromJSON Book
