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

{-# LANGUAGE RecordWildCards #-}
module Lib (mainFunc) where

import Network.Wai.Handler.Warp
import qualified Data.Configurator as C
import Control.Lens hiding ((.=))
import Data.Aeson (encode)
import Data.Text
import qualified Data.Text.IO as TextIO
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

import AppConfig
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Json.Book hiding (title)
import Json.User
import Ware
import Typer
import API
import Server


connectInfo :: AppConfig -> ConnectInfo
connectInfo conf = defaultConnectInfo { connectUser     = dbUser conf
                                      , connectPassword = dbPassword conf
                                      , connectDatabase = dbName conf
                                      , connectHost     = dbHost conf
                                      , connectPort     = dbPort conf
                                      }


type SwaggerSchemaEndpoint = "swagger.js" :> Get '[JSON] Swagger

type API' = BasicAPI
       :<|> SwaggerSchemaEndpoint
       :<|> SwaggerUI "ui" SwaggerSchemaEndpoint API
       :<|> Raw

instance HasServer API
  context where
  type ServerT API m = ServerT API' m
  route _ = route (Proxy :: Proxy API')


userapi :: Proxy UserAPI
userapi = Proxy

bookapi :: Proxy BookAPI
bookapi = Proxy

api :: Proxy API
api = Proxy

server :: ConnectionPool -> String -> Server API
server pool salt = basicServer pool salt
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

instance HasMock API context0

--- Swagger ---
type instance IsElem' e API = IsElem e API'

instance ToSchema Book
instance ToSchema HelloMessage
instance ToSchema User


js :: IO ()
js = writeJSForAPI bookapi vanillaJS (static </> "vanilla"  </> "api.js")

rubyClient :: Text
rubyClient = Lackey.rubyForAPI bookapi

writeRubyClient :: IO ()
writeRubyClient = TextIO.writeFile "rubyClient.rb" rubyClient

qhello :: Maybe String -> Manager -> BaseUrl -> ClientM HelloMessage
qhellouser :: Maybe Text -> Manager -> BaseUrl -> ClientM String
qgetusers :: Maybe Text -> Manager -> BaseUrl -> ClientM [User]
qhello :<|> qhellouser :<|> qgetusers = client userapi

queries :: Manager -> BaseUrl -> ExceptT ServantError IO (HelloMessage, String, [User])
queries manager baseurl = do
  let auth = Just "Basic dXNlcjpwYXNzd29yZA=="
  msg    <- qhello     (Just "tjena") manager baseurl
  secret <- qhellouser auth manager baseurl
  users  <- qgetusers  auth manager baseurl
  return (msg, secret, users)

haskell :: IO ()
haskell = do
  let baseUrl = (BaseUrl Http "localhost" 3000 "/users") -- /users/hello
  manager <- newManager defaultManagerSettings
  res <- runExceptT $ queries manager baseUrl
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (msg, secret, users) -> do
      print msg
      print secret
      print users

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy BasicAPI)
    & info.title       .~ "Items API"
    & info.version     .~ "2016.7.7"
    & info.description ?~ "Hemlig API för hemlig projektutveckling"

genHello :: IO ()
genHello = BL8.putStr $ encode swaggerDoc

mockServer :: IO ()
mockServer = run 3000 $ serve api $ mock api Proxy

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
