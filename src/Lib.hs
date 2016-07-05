{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( mainFunc
    ) where

import Data.ByteString (ByteString)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.HTTP.Types.Header (hAuthorization)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, corsMethods, corsRequestHeaders, corsOrigins, corsExposedHeaders, corsOrigins, corsRequireOrigin, corsIgnoreFailures, corsMaxAge, corsVaryOrigin, simpleHeaders)
import qualified Data.Configurator as C
import Data.Text
import Data.Word
-- import System.FilePath
import Servant
-- import Servant.JS
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


configFileName :: String
configFileName = "application.conf"


connectInfo :: AppConfig -> ConnectInfo
connectInfo conf = defaultConnectInfo { connectUser = dbUser conf
                                      , connectPassword = dbPassword conf
                                      , connectDatabase = dbName conf
                                      , connectHost = dbHost conf
                                      , connectPort = dbPort conf
                                      }


helloUser :: ConnectionPool -> String -> Maybe Text -> ExceptT ServantErr IO Text
helloUser pool salt Nothing = throwError $ err403 { errBody = "No Authorization header found!" }
helloUser pool salt (Just authHeader) =
    withUser pool authHeader salt $ \user -> do
        liftIO $ print user
        return $ pack $ show user

hello :: ExceptT ServantErr IO Text
hello  = return "Hello, World!"


-- type GetEntity path kind
--   = path :> Capture "id" Int :> Get '[JSON] kind -- (Maybe kind)


type API = "hello" :> Get '[PlainText] Text
      :<|> "helloUser" :> Header "Authorization" Text :> Get '[PlainText] Text
      :<|> "book" :> Capture "id" Int :> Get '[JSON] (Maybe Book)
      -- :<|> GetEntity "book" Book
      :<|> "book" :> QueryParam "searchField" String :> QueryParam "searchStr" String :> QueryParam "offset" Word16 :> QueryParam "limit" Word16 :> Get '[JSON] [Book]
      :<|> "books" :> Get '[JSON] [Book]
      :<|> "book"  :> Header "Authorization" Text :> ReqBody '[JSON] Book :> Post   '[PlainText] [Char]
      :<|> "book"  :> Header "Authorization" Text :> ReqBody '[JSON] Book :> Put    '[PlainText] [Char]
      :<|> "book"  :> Header "Authorization" Text :> Capture "id"     Int :> Delete '[PlainText] [Char]
      :<|> "users" :> Header "Authorization" Text                         :> Get    '[JSON]      [User]


type API' = API :<|> Raw

api :: Proxy API
api = Proxy

api' :: Proxy API'
api' = Proxy

server :: ConnectionPool -> String -> Server API
server pool salt = hello
      :<|> helloUser pool salt
      :<|> showBook pool
      :<|> selectBooks pool
      :<|> (selectBooks pool (Just "title") (Just "") (Just 0) (Just 10))
      :<|> createBook pool salt
      :<|> updateBook pool salt
      :<|> deleteBook pool salt
      :<|> selectUsersAuth pool salt

server' :: ConnectionPool -> String -> Server API'
server' pool salt = server pool salt
      :<|> serveDirectory "static"


-- static :: FilePath
-- static = "static"

-- js :: IO ()
-- js = writeJSForAPI pointApi vanillaJS (static </> "vanilla"  </> "api.js")

-- md :: String
-- md = markdown (docs serverApi)

-- instance toSample [Book] [Book] where
--   toSample _ = Just [Book { title = "hej", author = "jag", content = "innehåll", year = 2004 } ]

-- instance MimeRender PlainText ()

-- getBooks :: EitherT ServantErr IO [Book]
-- getBooks = client api host where
--   Right host = parseBaseUrl "http://localhost:3000/books"

---------------------------------------

-- authHEAD :: ByteString
-- authHEAD = "Authorization"

myCors :: Middleware
myCors = cors $ const (Just myResourcePolicy)

myResourcePolicy :: CorsResourcePolicy
myResourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Just (["http://127.0.0.1:8000", "http://localhost:8000", "http://127.0.0.1:8001", "chrome-extension://fhbjgbiflinjbdggehcddcbncdddomop"], True)
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTION"]
        , corsRequestHeaders = simpleHeaders ++ [hAuthorization]
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

mainFunc :: IO ()
mainFunc = do
    putStrLn "Starting server..."
    loadedConf <- C.load [C.Required configFileName]
    maybeConf <- makeDbConfig loadedConf
    case maybeConf of
        Nothing -> putStrLn $ "Can't parse \"" ++ configFileName ++ "\" file, terminating!"
        Just conf -> do
            pool <- runStdoutLoggingT $ createMySQLPool (connectInfo conf) (fromIntegral $ poolSize conf)
            run (fromIntegral $ appPort conf) $ myCors $ serve api' (server' pool (dbSalt conf))
            -- run (fromIntegral $ appPort conf) $ myCors $ serve api (server pool (dbSalt conf))
            -- run (fromIntegral $ appPort conf) $ (addHeaders [(authHEAD, authHEAD)]) $ myCors $ serve api' (server' pool (dbSalt conf))
