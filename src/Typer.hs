{-# LANGUAGE DeriveGeneric #-}
module Typer where

import Data.Aeson
-- import Data.Aeson.TH
import GHC.Generics

static :: FilePath
static = "static"

-- authHEAD :: ByteString
-- authHEAD = "Authorization"

newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Generic, Show)

instance ToJSON HelloMessage
instance FromJSON HelloMessage


-- newtype Hej = Hej String
--   deriving Generic

-- instance Show Hej where
--   show a = a

-- instance ToJSON Hej

ccc :: Char
ccc = 'c'

ttt :: String
ttt = "text"

sss :: String
sss = "streng"
