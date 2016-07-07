{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib (mainFunc) where

import Network.Wai.Handler.Warp
import qualified Data.Configurator as C
import Control.Lens       hiding ((.=))
-- import Data.Aeson.Types (camelTo2)
import Data.Aeson (encode)
import Data.Text
import Data.Text.Internal
import Data.Word
import Data.Swagger hiding (Header, Http)
import qualified Data.ByteString.Lazy.Char8 as BL8

import System.FilePath
import qualified Lackey
import Servant
import Servant.Docs hiding (API) -- (markdown, docs)
import Servant.JS (writeJSForAPI, vanillaJS)
import Servant.Mock -- (mock)
import Test.QuickCheck.Arbitrary

import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.Client
import Servant.Swagger
import Servant.Swagger.UI

import Database.Persist.MySQL
import Control.Monad.Logger

import Auth.AuthHeader
import Auth.DbAuth
import AppConfig
import Db.Common

import Control.Monad.IO.Class
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Logic.Books
import Logic.Users
import Json.Book hiding (title)
import Json.User
import Ware
import Typer

configFileName :: String
configFileName = "application.conf"


connectInfo :: AppConfig -> ConnectInfo
connectInfo conf = defaultConnectInfo { connectUser     = dbUser conf
                                      , connectPassword = dbPassword conf
                                      , connectDatabase = dbName conf
                                      , connectHost     = dbHost conf
                                      , connectPort     = dbPort conf
                                      }

helloUser :: ConnectionPool -> String -> Maybe Text -> Handler String
helloUser pool salt Nothing = throwError $ err403 { errBody = "No Authorization header found!" }
helloUser pool salt (Just authHeader) =
    withUser pool authHeader salt $ \user -> do
        return $ show user
        -- liftIO $ print user

hello :: Maybe String -> Handler HelloMessage
hello mname = return . HelloMessage $ case mname of
  Nothing -> "Hello, anonymous coward"
  Just n  -> "Hello from Biblio Servant, " ++ n

type SwaggerSchemaEndpoint = "swagger.js" :> Get '[JSON] Swagger

type UserAPI = "users" :> Header "Authorization" Text :> Get '[JSON] [User]
               :<|> "helloUser" :> Header "Authorization" Text :> Get '[PlainText] String

type HelloAPI = "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage

type BookAPI = "book"  :> Capture "id" Int :> Get '[JSON] (Maybe Book)
          :<|> "book"  :> QueryParam "searchField" String :> QueryParam "searchStr" String
                       :> QueryParam "offset" Word16 :> QueryParam "limit" Word16 :> Get '[JSON] [Book]
          :<|> "books" :> Get '[JSON] [Book]
          :<|> "book"  :> Header "Authorization" Text :> ReqBody '[JSON] Book :> Post   '[JSON] String
          :<|> "book"  :> Header "Authorization" Text :> ReqBody '[JSON] Book :> Put    '[JSON] String
          :<|> "book"  :> Header "Authorization" Text :> Capture "id"    Int  :> Delete '[JSON] String

-- type BasicAPI = HelloAPI
--            :<|> UserAPI
--            :<|> BookAPI

data API
-- type API' = BasicAPI
type API' = HelloAPI
       :<|> UserAPI
       :<|> BookAPI
       :<|> SwaggerSchemaEndpoint
       :<|> SwaggerUI "ui" SwaggerSchemaEndpoint API
       :<|> Raw

instance HasServer API
  context where
  type ServerT API m = ServerT API' m
  route _ = route (Proxy :: Proxy API')


helloapi :: Proxy HelloAPI
helloapi = Proxy

userapi :: Proxy UserAPI
userapi = Proxy

bookapi :: Proxy BookAPI
bookapi = Proxy

api :: Proxy API
api = Proxy

helloServer :: ConnectionPool -> String -> Server HelloAPI
helloServer pool salt = hello

userServer :: ConnectionPool -> String -> Server UserAPI
userServer pool salt = selectUsersAuth pool salt
              :<|> helloUser pool salt

bookServer :: ConnectionPool -> String -> Server BookAPI
bookServer pool salt = showBook pool
              :<|> selectBooks pool
              :<|> selectBooks pool (Just "title") (Just "") (Just 0) (Just 10)
              :<|> createBook pool salt
              :<|> updateBook pool salt
              :<|> deleteBook pool salt

server :: ConnectionPool -> String -> Server API
server pool salt = helloServer pool salt
              :<|> userServer pool salt
              :<|> bookServer pool salt
              :<|> return swaggerDoc
              :<|> swaggerUIServer
              :<|> serveDirectory "static"

--- Böcker ---
instance ToParam (QueryParam "limit" Word16) where
  toParam _ = DocQueryParam "limit" ["10", "20"] "Hejlimit" Normal -- List | Flag

instance ToParam (QueryParam "offset" Word16) where
  toParam _ = DocQueryParam "offset" ["0", "10"] "Hejoffset" Normal -- List | Flag

instance ToParam (QueryParam "searchField" [Char]) where
  toParam _ = DocQueryParam "searchField" ["title", "author"] "Hejsearchfield" Normal -- List | Flag

instance ToParam (QueryParam "searchStr" [Char]) where
  toParam _ = DocQueryParam "searchStr" ["Kurt", "Anders"] "Hejsearchstr" Normal -- List | Flag

instance ToCapture (Capture "id" Int) where
  toCapture _ = DocCapture "1" "Record number"

instance ToSample Char where
  toSamples _ = [("apa", 'c')]

--- Mockserver ---
instance Arbitrary HelloMessage where
  arbitrary = HelloMessage <$> arbitrary

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Book where
  arbitrary = Book <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Swagger

instance Arbitrary (SwaggerUiHtml SwaggerSchemaEndpoint API)

--- Swagger ---
type instance IsElem' e API = IsElem e API'

instance ToSchema Book
instance ToSchema HelloMessage

js :: IO ()
js = writeJSForAPI bookapi vanillaJS (static </> "vanilla"  </> "api.js")

rubyClient :: Text
rubyClient = Lackey.rubyForAPI bookapi

helloclient :: Client HelloAPI
helloclient = client helloapi

queries :: Manager -> BaseUrl -> ExceptT ServantError IO HelloMessage
queries manager baseurl = do
  message <- helloclient (Just "servant") manager baseurl
  return message

haskell :: IO ()
haskell = do
  let baseUrl = (BaseUrl Http "localhost" 3000 "")
  manager <- newManager defaultManagerSettings
  res <- runExceptT $ queries manager baseUrl
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right message -> do
      print message

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy BookAPI)
    & info.title       .~ "Items API"
    & info.version     .~ "2016.7.7"
    & info.description ?~ "Hemlig API för hemlig projektutveckling"

genHello :: IO ()
genHello = BL8.putStr $ encode swaggerDoc

-- mockServer :: IO ()
-- mockServer = run 3000 $ serve mainapi $ mock mainapi Proxy

apiDocs :: String
apiDocs = markdown $ docs bookapi

writeDocs :: IO ()
writeDocs = writeFile "mydocs.md" apiDocs

mainFunc :: IO ()
mainFunc = do
    putStrLn "Starting server..."
    loadedConf <- C.load [C.Required configFileName]
    maybeConf <- makeDbConfig loadedConf
    case maybeConf of
        Nothing -> putStrLn $ "Can't parse \"" ++ configFileName ++ "\" file, terminating!"
        Just conf -> do
            pool <- runStdoutLoggingT $ createMySQLPool (connectInfo conf) (fromIntegral $ poolSize conf)
            run (fromIntegral $ appPort conf) $ myCors $ serve api (server pool (dbSalt conf))
            -- run (fromIntegral $ appPort conf) $ (addHeaders [(authHEAD, authHEAD)]) $ myCors $ serve api' (server' pool (dbSalt conf))
