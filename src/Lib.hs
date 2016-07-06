{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib ( mainFunc) where

import Network.Wai.Handler.Warp
import qualified Data.Configurator as C
import Data.Text
import Data.Word
-- import Data.Aeson
-- import Data.Aeson.TH
-- import GHC.Generics

import System.FilePath
import Servant
import Servant.Docs -- (markdown, docs)
-- import Servant.Docs.Internal -- (ToSample)
import Servant.JS (writeJSForAPI)
import qualified Lackey

-- import Servant.Swagger
-- import Servant.Swagger.UI
-- import Servant.API.ContentTypes
-- import Servant.Client

import Database.Persist.MySQL
import Control.Monad.Logger

import Auth.AuthHeader
import Auth.DbAuth
import AppConfig
import Db.Common

import Control.Monad.IO.Class
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Except (ExceptT)

import Logic.Books
import Logic.Users
import Json.Book
import Json.User
import Ware
import Typer

configFileName :: String
configFileName = "application.conf"


connectInfo :: AppConfig -> ConnectInfo
connectInfo conf = defaultConnectInfo { connectUser = dbUser conf
                                      , connectPassword = dbPassword conf
                                      , connectDatabase = dbName conf
                                      , connectHost = dbHost conf
                                      , connectPort = dbPort conf
                                      }

-- helloUser :: ConnectionPool -> String -> Maybe Text -> ExceptT ServantErr IO Text
-- -- helloUser :: ConnectionPool -> String -> Maybe Text -> Handler HelloMessage
-- helloUser pool salt Nothing = throwError $ err403 { errBody = "No Authorization header found!" }
-- helloUser pool salt (Just authHeader) =
--     withUser pool authHeader salt $ \user -> do
--         liftIO $ print user
--         return $ pack $ show user

hello :: Maybe String -> Handler HelloMessage
hello mname = return . HelloMessage $ case mname of
  Nothing -> "Hello, anonymous coward"
  Just n  -> "Hello from Biblio Servant, " ++ n

type UserAPI = "users" :> Header "Authorization" Text :> Get '[JSON] [User]
               :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
               -- "hello" :> Get '[PlainText] Text
               -- :<|> "helloUser" :> Header "Authorization" Text :> Get '[PlainText] Text

type BookAPI = "book"  :> Capture "id" Int :> Get '[JSON] (Maybe Book)
          :<|> "book"  :> QueryParam "searchField" String :> QueryParam "searchStr" String
                       :> QueryParam "offset" Word16 :> QueryParam "limit" Word16 :> Get '[JSON] [Book]
          :<|> "books" :> Get '[JSON] [Book]
          -- :<|> "book"  :> Header "Authorization" Text :> ReqBody '[JSON] Book :> Post   '[PlainText] [Char]
          -- :<|> "book"  :> Header "Authorization" Text :> ReqBody '[JSON] Book :> Put    '[PlainText] [Char]
          -- :<|> "book"  :> Header "Authorization" Text :> Capture "id"     Int :> Delete '[PlainText] String

userapi :: Proxy UserAPI
userapi = Proxy

bookapi :: Proxy BookAPI
bookapi = Proxy

type MainAPI =      UserAPI
           :<|> BookAPI
           :<|> Raw

mainapi :: Proxy MainAPI
mainapi = Proxy

userServer :: ConnectionPool -> String -> Server UserAPI
userServer pool salt = selectUsersAuth pool salt
                  :<|> hello
              -- :<|> helloUser pool salt

bookServer :: ConnectionPool -> String -> Server BookAPI
bookServer pool salt = showBook pool
                  :<|> selectBooks pool
                  :<|> (selectBooks pool (Just "title") (Just "") (Just 0) (Just 10))
                  -- :<|> createBook pool salt
                  -- :<|> updateBook pool salt
                  -- :<|> deleteBook pool salt

server :: ConnectionPool -> String -> Server MainAPI
server pool salt = userServer pool salt
              :<|> bookServer pool salt
              :<|> serveDirectory "static"

instance ToSample Book where
  toSamples _ = singleSample Book { Json.Book.id = Just 1, title = "hej", author = "jag", content = "innehåll", year = 2004, user_id = Nothing }

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

-- instance ToSample Char where
--   toSamples _ = "xxx"

-- instance MimeRender PlainText ()

-- ttt :: String
-- ttt = "text"

-- sss :: String
-- sss = "streng"

----------------------------------------------------------------------------
-- No instance for servant-foreign-0.7.1:Servant.Foreign.Internal.NotFound -
-- js :: IO ()
-- js = writeJSForAPI bookapi vanillaJS (static </> "vanilla"  </> "api.js")
-- rubyClient :: Text
-- rubyClient = Lackey.rubyForAPI bookapi
----------------------------------------------------------------------------

-- getBooks :: Handler [Book]
-- getBooks = client api host where
--   Right host = parseBaseUrl "http://localhost:3000/books"


-- mockServer :: IO ()
--   mockServer = run 3000 $ serve mainapi $ mock bookapi

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
            run (fromIntegral $ appPort conf) $ myCors $ serve mainapi (server pool (dbSalt conf))
            -- run (fromIntegral $ appPort conf) $ myCors $ serve api (server pool (dbSalt conf))
            -- run (fromIntegral $ appPort conf) $ (addHeaders [(authHEAD, authHEAD)]) $ myCors $ serve api' (server' pool (dbSalt conf))
