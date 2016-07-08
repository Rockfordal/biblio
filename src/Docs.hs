{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Docs where

import Data.Proxy
import Servant.Docs
import Servant.API

import API
import Logic.User
import Logic.Book

