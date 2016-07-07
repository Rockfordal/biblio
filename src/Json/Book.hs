{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Json.Book where

import GHC.Generics (Generic)
import qualified Data.Aeson as A
import Servant.Docs -- (markdown, docs)

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


instance ToSample Book where
  toSamples _ = singleSample Book { Json.Book.id = Just 1, title = "hej", author = "jag", content = "innehåll", year = 2004, user_id = Nothing }
