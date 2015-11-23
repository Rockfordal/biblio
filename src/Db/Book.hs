{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Db.Book where

import GHC.Generics
import Database.Persist.TH
import Database.Persist
import Database.Persist.Sql

import qualified Data.Text as T

-- import qualified Domain.Book as Dom

share [mkPersist sqlSettings] [persistLowerCase|
Book
    title    String
    author   String
    content  String
    year     Int
    user_id  Int Maybe
    deriving Show
    deriving Generic
|]

