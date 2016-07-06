{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Logic.Users where

import Prelude hiding (id)
import Database.Persist.MySQL
import Servant
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text hiding (replace, length, any, concat, map, filter)
import Data.Char
import Data.Maybe
import Data.Word

import Auth.DbAuth
import Db.Common
import Json.User (User(..))
import qualified Json.User as JsonUser
import qualified Db.User as DbUser
import qualified Convert.UserConverter as C


createUser :: ConnectionPool -> String -> Maybe Text -> User -> Handler ()
createUser _ _ Nothing _ = return ()
createUser pool salt (Just authHeader) entity =
    withUser pool authHeader salt $ \_user -> do
        -- query pool $ insert (C.toRecord entity :: DbUser.User)
        _ <- ($) query pool $ insert (C.toRecord entity :: DbUser.User)
        return ()

-- entityBelongsToUser :: MonadIO m => ConnectionPool -> User -> Int -> m Bool
-- entityBelongsToUser _ (User {JsonUser.id = Nothing}) _ = return False -- No user id
-- entityBelongsToUser pool (User {JsonUser.id = _id}) _bid = do
--     entitys <- query pool $ selectList [DbUser.UserUser_id ==. _id, DbUser.UserId ==. toKey _bid] []
--     return $ length entitys == 1

updateUser :: ConnectionPool -> String -> Maybe Text -> User -> Handler ()
updateUser _ _ Nothing _ = return ()
updateUser pool salt (Just authHeader) entity =
    withUser pool authHeader salt $ \_user ->
        case entity of
            User {JsonUser.id = Nothing} -> throwError $ err400 { errBody = "No id specified!" }
            User {JsonUser.id = Just _id} -> do
                -- belongs <- entityBelongsToUser pool user _id
                -- if belongs
                --     then do
                --         query pool $ replace (toKey _id :: Key DbUser.User)
                --                              (C.toRecord entity :: DbUser.User)
                        return ()
                    -- else return ()

deleteUser :: ConnectionPool -> String -> Maybe Text -> Int -> Handler ()
deleteUser _ _ Nothing _ = return ()
deleteUser pool salt (Just authHeader) _id =
    withUser pool authHeader salt $ \_user -> do
        -- belongs <- entityBelongsToUser pool user _id
        -- if belongs
        --     then do
        --         query pool $ delete (toKey _id :: Key DbUser.User)
                return ()
            -- else return ()

showUser :: ConnectionPool -> Int -> Handler (Maybe User)
showUser pool userid = do
    entity <- query pool $ selectFirst [DbUser.UserId ==. (toKey userid :: Key DbUser.User)] []
    return $ maybeUser entity
    where maybeUser Nothing = Nothing
          maybeUser (Just b) = C.toJson b


selectUsersAuth :: ConnectionPool -> String -> Maybe Text -> Handler [User]
selectUsersAuth _pool _salt Nothing = throwError $ err403 { errBody = "No Authorization header found!" }
selectUsersAuth pool salt (Just authHeader) =
    withUser pool authHeader salt $ \user -> do
        liftIO $ print user
        -- liftIO $ selectUsers pool (Just "login") (Just "") (Just 0) (Just 10)
        -- return $ pack $ show user
        -- return (selectUsers pool (Just "login") (Just "") (Just 0) (Just 10))
        let entityField = DbUser.UserLogin
        do
          entitys <- query pool $ selectList [Filter entityField (Left $ concat ["%", "", "%"]) (BackendSpecificFilter "like")]
                                             -- [OffsetBy $ fromIntegral 0
                                             -- , LimitTo $ fromIntegral 10]
                                             [OffsetBy 0 , LimitTo 10]
          return $ map (\(Just b) -> b) $ filter isJust $ map C.toJson entitys


selectUsers :: ConnectionPool -> Maybe String -> Maybe String -> Maybe Word16 -> Maybe Word16 -> Handler [User]
selectUsers _ Nothing _ _ _= return []
selectUsers _ _ Nothing _ _ = return []
selectUsers _ _ _ Nothing _ = return []
selectUsers _ _ _ _ Nothing = return []
selectUsers pool (Just field) (Just searchStr) (Just offset) (Just limit) = do
    let entityField = case field of
                        "login" -> DbUser.UserLogin
    if any (not . isAlphaNum) searchStr
        then return []
        else do
            entitys <- query pool $ selectList [Filter entityField (Left $ concat ["%", searchStr, "%"]) (BackendSpecificFilter "like")]
                                               [OffsetBy $ fromIntegral offset, LimitTo $ fromIntegral limit]
            return $ map (\(Just b) -> b) $ filter isJust $ map C.toJson entitys
