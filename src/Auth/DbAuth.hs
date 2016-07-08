{-# LANGUAGE OverloadedStrings #-}
module Auth.DbAuth where

import Data.Text
import Database.Persist.MySQL
import Control.Monad.IO.Class
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Except (ExceptT)
import Servant

import Auth.AuthHeader

import qualified Db.User as Db
import qualified Json.User as J
import qualified Convert.UserConverter as C

findUser :: ConnectionPool -> Text -> Text -> String -> IO (Maybe J.User)
findUser pool login password salt = do
     users <- flip runSqlPool pool $
                       selectList [Db.UserLogin ==. unpack login, Db.UserPassword ==. encodePassword salt (unpack password)] []
     return $ oneUser users

     where oneUser [user] = C.toJson user
           oneUser _ = Nothing


authHeaderToUser :: MonadIO m => ConnectionPool -> Text -> String -> m (Maybe J.User)
authHeaderToUser pool authHeader salt = do
    let auth = extractBasicAuth authHeader
    liftIO $ tryFindUser auth

    where tryFindUser Nothing = return Nothing
          tryFindUser (Just (login, password)) = findUser pool login password salt

withUser :: ConnectionPool -> Text -> String -> (J.User -> ExceptT ServantErr IO a) -> ExceptT ServantErr IO a
withUser pool authHeader salt func = do
    maybeUser <- authHeaderToUser pool authHeader salt
    case maybeUser of
        Nothing -> throwError $ err403 { errBody = "No user found" }
        Just user -> func user

withUser2 :: ConnectionPool -> Text -> String -> (J.User -> ExceptT ServantErr IO a) -> ExceptT ServantErr IO a
withUser2 pool authHeader salt func = do
    maybeUser <- authHeaderToUser pool authHeader salt
    case maybeUser of
        Nothing -> throwError $ err403 { errBody = "No user found" }
        Just user -> func user
