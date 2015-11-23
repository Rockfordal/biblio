{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( mainFunc
    ) where

import Network.Wai.Handler.Warp
import qualified Data.Configurator as C
import Data.Text
import Servant

import Database.Persist.MySQL
import Control.Monad.Logger

import Auth.AuthHeader
import Auth.DbAuth
import AppConfig
import Db.Common

import Json.Book (Book(..))
import Logic.Books

configFileName :: String
configFileName = "application.conf"


connectInfo :: AppConfig -> ConnectInfo
connectInfo conf = defaultConnectInfo { connectUser = dbUser conf
                                      , connectPassword = dbPassword conf
                                      , connectDatabase = dbName conf
                                      , connectHost = dbHost conf
                                      , connectPort = dbPort conf
                                      }

---------------------------------------

type API = "book" :> Header "Authorization" Text :> ReqBody '[JSON] Book :> Post '[PlainText] ()
      :<|> "book" :> Header "Authorization" Text :> ReqBody '[JSON] Book :> Put '[PlainText] ()
      :<|> "book" :> Header "Authorization" Text :> Capture "id" Int :> Delete '[PlainText] ()
      :<|> "book" :> Capture "id" Int :> Get '[JSON] (Maybe Book)
      :<|> "book" :> QueryParam "searchStr" String :> Get '[JSON] [Book]

type API' = API :<|> Raw


api :: Proxy API
api = Proxy

api' :: Proxy API'
api' = Proxy

server :: ConnectionPool -> String -> Server API
server pool salt = createBook pool salt
              :<|> updateBook pool salt
              :<|> deleteBook pool salt
              :<|> showBook pool
              :<|> selectBooks pool

server' :: ConnectionPool -> String -> Server API'
server' pool salt = server pool salt
               :<|> serveDirectory "static"


---------------------------------------

mainFunc :: IO ()
mainFunc = do
    putStrLn "Starting server..."
    loadedConf <- C.load [C.Required configFileName]
    maybeConf <- makeDbConfig loadedConf
    case maybeConf of
        Nothing -> putStrLn $ "Can't parse \"" ++ configFileName ++ "\" file, terminating!"
        Just conf -> do
            pool <- runStdoutLoggingT $ createMySQLPool (connectInfo conf) (fromIntegral $ poolSize conf)
            run (fromIntegral $ appPort conf) (serve api' (server' pool (dbSalt conf)))
