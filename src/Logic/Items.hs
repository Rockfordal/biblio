{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Logic.Items where

import Prelude hiding (id)
import Database.Persist.MySQL
import Servant
-- import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Error.Class (throwError)
-- import Control.Monad.IO.Class
import Data.Text hiding (replace, length, any, concat, map, filter)
import Data.Char
import Data.Maybe
import Data.Word

import Auth.DbAuth
import Db.Common
import Json.Item (Item(..))
import qualified Json.Item as JsonItem
import qualified Db.Item as DbItem
-- import Json.User (User(User)) -- (User(..))
-- import qualified Json.User as JsonUser
import qualified Convert.ItemConverter as C
-- import Typer


createItem :: ConnectionPool -> String -> Maybe Text -> Item -> Handler [Char]
createItem _ _ Nothing _ = return "nej"
createItem pool salt (Just authHeader) item =
    withUser pool authHeader salt $ \_user -> do
        -- query pool $ insert (C.toRecord item {user_id = JsonUser.id user} :: DbItem.Item)
        -- _ <- ($) query pool $ insert (C.toRecord item {user_id = JsonUser.id user} :: DbItem.Item)
        _ <- ($) query pool $ insert (C.toRecord item :: DbItem.Item)
        return "ja"

-- itemBelongsToUser :: MonadIO m => ConnectionPool -> User -> Int -> m Bool
-- itemBelongsToUser _ (User {JsonUser.id = Nothing}) _ = return False -- No user id
-- itemBelongsToUser pool (User {JsonUser.id = _id}) _bid = do
--     items <- query pool $ selectList [DbItem.ItemUser_id ==. _id, DbItem.ItemId ==. toKey _bid] []
--     return $ length items == 1

updateItem :: ConnectionPool -> String -> Maybe Text -> Item -> Handler [Char]
updateItem _ _ Nothing _ = return "nej"
updateItem pool salt (Just authHeader) item =
    withUser pool authHeader salt $ \_user ->
        case item of
            Item {JsonItem.id = Nothing} -> throwError $ err400 { errBody = "No id specified!" }
            Item {JsonItem.id = Just _id} -> do
                -- belongs <- itemBelongsToUser pool user _id
                -- if belongs
                    -- then do
                      do
                        query pool $ replace (toKey _id :: Key DbItem.Item)
                                             -- (C.toRecord item {user_id = JsonUser.id user} :: DbItem.Item)
                                             (C.toRecord item :: DbItem.Item)
                        return "ja"
                    -- else return "nej"


deleteItem :: ConnectionPool -> String -> Maybe Text -> Int -> Handler String
deleteItem _ _ Nothing _ = return "nej"
deleteItem pool salt (Just authHeader) _id =
    withUser pool authHeader salt $ \_user -> do
        -- belongs <- itemBelongsToUser pool user _id
        -- if belongs
            -- then do
              do
                query pool $ delete (toKey _id :: Key DbItem.Item)
                return "ja"
            -- else throwError $ err400 { errBody = "deleteItem error"}


showItem :: ConnectionPool -> Int -> Handler (Maybe Item)
showItem pool itemid = do
    -- item <- query pool $ selectFirst [DbItem.ItemId ==. (toKey id :: Key DbItem.Item)] []
    item <- query pool $ selectFirst [DbItem.ItemId ==. (toKey itemid :: Key DbItem.Item)] []
    return $ maybeItem item
    where maybeItem Nothing = Nothing
          maybeItem (Just b) = C.toJson b

selectItems :: ConnectionPool -> Maybe String -> Maybe String -> Maybe Word16 -> Maybe Word16 -> Handler [Item]
selectItems _ Nothing _ _ _ = return []
selectItems _ _ Nothing _ _ = return []
selectItems _ _ _ Nothing _ = return []
selectItems _ _ _ _ Nothing = return []
selectItems pool (Just field) (Just searchStr) (Just offset) (Just limit) = do
    let entityField = case field of
                        "name"  -> DbItem.ItemName
                        -- "size" -> DbItem.ItemContent
                        _         -> DbItem.ItemName
    if any (not . isAlphaNum) searchStr
        then return []
        else do
            items <- query pool $ selectList [Filter entityField (Left $ concat ["%", searchStr, "%"]) (BackendSpecificFilter "like")]
                                            [OffsetBy $ fromIntegral offset, LimitTo $ fromIntegral limit]
            return $ map (\(Just b) -> b) $ filter isJust $ map C.toJson items
