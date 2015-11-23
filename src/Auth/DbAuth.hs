{-# LANGUAGE OverloadedStrings #-}
module Auth.DbAuth where

import Data.Text
import Database.Persist.MySQL
import Database.Persist.Sql
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Servant.API
import Servant

import Auth.AuthHeader

import Db.Common
import qualified Db.User as Db
import qualified Domain.User as Dom


findUser :: ConnectionPool -> Text -> Text -> String -> IO (Maybe Dom.User)
findUser pool login password salt = do
     users <- flip runSqlPool pool $
                       selectList [Db.UserLogin ==. unpack login, Db.UserPassword ==. encodePassword salt (unpack password)] []
     return $ oneUser users
     
     where oneUser [user] = toDomain user
           oneUser _ = Nothing


authHeaderToUser :: MonadIO m => ConnectionPool -> Text -> String -> m (Maybe Dom.User)
authHeaderToUser pool authHeader salt = do
    let auth = extractBasicAuth authHeader
    liftIO $ tryFindUser auth

    where tryFindUser Nothing = return Nothing
          tryFindUser (Just (login, password)) = findUser pool login password salt

withUser :: ConnectionPool -> Text -> String -> (Dom.User -> EitherT ServantErr IO a) -> EitherT ServantErr IO a
withUser pool authHeader salt func = do
    maybeUser <- authHeaderToUser pool authHeader salt
    case maybeUser of
        Nothing -> left $ err403 { errBody = "No user found" }
        Just user -> func user
