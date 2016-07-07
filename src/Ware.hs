module Ware where

import Data.ByteString.Char8 (pack)
import Network.HTTP.Types.Header (hAuthorization)
import Network.HTTP.Types.Method -- (methodGet, methodPost, methodPut)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors -- (Origin, CorsResourcePolicy(..), cors, corsMethods, corsRequestHeaders, corsOrigins, corsExposedHeaders, corsOrigins, corsRequireOrigin, corsIgnoreFailures, corsMaxAge, corsVaryOrigin, simpleHeaders)


pux :: Origin
pux = pack "http://localhost:8000"

ajax :: Origin
ajax = pack "http://127.0.0.1:8001"

elm :: Origin
elm = pack "http://fire.solidcrm.se:8000"

postman :: Origin
postman = pack "chrome-extension://fhbjgbiflinjbdggehcddcbncdddomop"

myResourcePolicy :: CorsResourcePolicy
myResourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Just ([pux, ajax, elm, postman], True)
        , corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodHead, methodOptions]
        , corsRequestHeaders = simpleHeaders ++ [hAuthorization]
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

myCors :: Middleware
myCors = cors $ const (Just myResourcePolicy)
