{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Db.Shelf where

import GHC.Generics
import Database.Persist.TH

share [mkPersist sqlSettings] [persistLowerCase|
Shelf
    name     String
    size     Int
    deriving Eq
    deriving Show
    deriving Generic
|]
-- room_id  Int
