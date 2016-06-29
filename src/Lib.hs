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
import Data.Word
import Servant

import Database.Persist.MySQL
import Control.Monad.Logger

import Auth.AuthHeader
import Auth.DbAuth
import AppConfig
import Db.Common


import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Logic.Books
import Json.Book


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
-- Examples section

helloUser :: ConnectionPool -> String -> Maybe Text -> EitherT ServantErr IO Text
helloUser pool salt Nothing = left $ err403 { errBody = "No Authorization header found!" }
helloUser pool salt (Just authHeader) =
    withUser pool authHeader salt $ \user -> do
        liftIO $ print user
        return $ pack $ show user

hello :: EitherT ServantErr IO Text
hello  = return "Hello, World!"

---------------------------------------

type API = "helloUser" :> Header "Authorization" Text :> Get '[PlainText] Text
      :<|> "hello" :> Get '[PlainText] Text
      :<|> "book" :> Capture "id" Int :> Get '[JSON] (Maybe Book)
      :<|> "book" :> QueryParam "searchField" String :> QueryParam "searchStr" String :> QueryParam "offset" Word16 :> QueryParam "limit" Word16 :> Get '[JSON] [Book]
      :<|> "books" :> Get '[JSON] [Book]
      :<|> "book" :> Header "Authorization" Text :> ReqBody '[JSON] Book :> Post '[PlainText] ()
      :<|> "book" :> Header "Authorization" Text :> ReqBody '[JSON] Book :> Put '[PlainText] ()
      :<|> "book" :> Header "Authorization" Text :> Capture "id" Int :> Delete '[PlainText] ()


type API' = API :<|> Raw


api :: Proxy API
api = Proxy

api' :: Proxy API'
api' = Proxy

server :: ConnectionPool -> String -> Server API
server pool salt = helloUser pool salt
              :<|> hello
              :<|> showBook pool
              :<|> selectBooks pool
              :<|> (selectBooks pool (Just "title") (Just "") (Just 0) (Just 10))
              :<|> createBook pool salt
              :<|> updateBook pool salt
              :<|> deleteBook pool salt


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
