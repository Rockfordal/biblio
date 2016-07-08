module Lib (mainFunc) where

import Network.Wai.Handler.Warp
import qualified Data.Configurator as C
import Servant
import Servant.Swagger.UI

import Database.Persist.MySQL
import Control.Monad.Logger

import AppConfig
import Ware
import API
import Docs
import Server

connectInfo :: AppConfig -> ConnectInfo
connectInfo conf = defaultConnectInfo { connectUser     = dbUser conf
                                      , connectPassword = dbPassword conf
                                      , connectDatabase = dbName conf
                                      , connectHost     = dbHost conf
                                      , connectPort     = dbPort conf
                                      }

server :: ConnectionPool -> String -> Server API
server pool salt = basicServer pool salt
              :<|> return swaggerDoc
              :<|> swaggerUIServer
              :<|> serveDirectory "static"

mainFunc :: IO ()
mainFunc = do
    putStrLn "Starting server..."
    loadedConf <- C.load [C.Required configFileName]
    maybeConf  <- makeDbConfig loadedConf
    case maybeConf of
        Nothing -> putStrLn $ "Can't parse \"" ++ configFileName ++ "\" file, terminating!"
        Just conf -> do
            pool <- runStdoutLoggingT $ createMySQLPool (connectInfo conf) (fromIntegral $ poolSize conf)
            run (fromIntegral $ appPort conf) $ myCors $ serve api (server pool (dbSalt conf))
            -- run (fromIntegral $ appPort conf) $ (addHeaders [(authHEAD, authHEAD)]) $ myCors $ serve api' (server' pool (dbSalt conf))
