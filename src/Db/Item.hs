{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Db.Item where

import GHC.Generics
import Database.Persist.TH

share [mkPersist sqlSettings] [persistLowerCase|
Item
    name     String
    info     String
    deriving Eq
    deriving Show
    deriving Generic
|]
