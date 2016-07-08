{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Logic.Shelfs where

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
import Json.Shelf (Shelf(..))
import qualified Json.Shelf as JsonShelf
import qualified Db.Shelf as DbShelf
-- import Json.User (User(User)) -- (User(..))
-- import qualified Json.User as JsonUser
import qualified Convert.ShelfConverter as C
-- import Typer


createShelf :: ConnectionPool -> String -> Maybe Text -> Shelf -> Handler [Char]
createShelf _ _ Nothing _ = return "nej"
createShelf pool salt (Just authHeader) shelf =
    withUser pool authHeader salt $ \_user -> do
        -- query pool $ insert (C.toRecord shelf {user_id = JsonUser.id user} :: DbShelf.Shelf)
        -- _ <- ($) query pool $ insert (C.toRecord shelf {user_id = JsonUser.id user} :: DbShelf.Shelf)
        _ <- ($) query pool $ insert (C.toRecord shelf :: DbShelf.Shelf)
        return "ja"

-- shelfBelongsToUser :: MonadIO m => ConnectionPool -> User -> Int -> m Bool
-- shelfBelongsToUser _ (User {JsonUser.id = Nothing}) _ = return False -- No user id
-- shelfBelongsToUser pool (User {JsonUser.id = _id}) _bid = do
--     shelfs <- query pool $ selectList [DbShelf.ShelfUser_id ==. _id, DbShelf.ShelfId ==. toKey _bid] []
--     return $ length shelfs == 1

updateShelf :: ConnectionPool -> String -> Maybe Text -> Shelf -> Handler [Char]
updateShelf _ _ Nothing _ = return "nej"
updateShelf pool salt (Just authHeader) shelf =
    withUser pool authHeader salt $ \_user ->
        case shelf of
            Shelf {JsonShelf.id = Nothing} -> throwError $ err400 { errBody = "No id specified!" }
            Shelf {JsonShelf.id = Just _id} -> do
                -- belongs <- shelfBelongsToUser pool user _id
                -- if belongs
                    -- then do
                      do
                        query pool $ replace (toKey _id :: Key DbShelf.Shelf)
                                             -- (C.toRecord shelf {user_id = JsonUser.id user} :: DbShelf.Shelf)
                                             (C.toRecord shelf :: DbShelf.Shelf)
                        return "ja"
                    -- else return "nej"


deleteShelf :: ConnectionPool -> String -> Maybe Text -> Int -> Handler String
deleteShelf _ _ Nothing _ = return "nej"
deleteShelf pool salt (Just authHeader) _id =
    withUser pool authHeader salt $ \_user -> do
        -- belongs <- shelfBelongsToUser pool user _id
        -- if belongs
            -- then do
              do
                query pool $ delete (toKey _id :: Key DbShelf.Shelf)
                return "ja"
            -- else throwError $ err400 { errBody = "deleteShelf error"}


showShelf :: ConnectionPool -> Int -> Handler (Maybe Shelf)
showShelf pool shelfid = do
    -- shelf <- query pool $ selectFirst [DbShelf.ShelfId ==. (toKey id :: Key DbShelf.Shelf)] []
    shelf <- query pool $ selectFirst [DbShelf.ShelfId ==. (toKey shelfid :: Key DbShelf.Shelf)] []
    return $ maybeShelf shelf
    where maybeShelf Nothing = Nothing
          maybeShelf (Just b) = C.toJson b

selectShelfs :: ConnectionPool -> Maybe String -> Maybe String -> Maybe Word16 -> Maybe Word16 -> Handler [Shelf]
selectShelfs _ Nothing _ _ _ = return []
selectShelfs _ _ Nothing _ _ = return []
selectShelfs _ _ _ Nothing _ = return []
selectShelfs _ _ _ _ Nothing = return []
selectShelfs pool (Just field) (Just searchStr) (Just offset) (Just limit) = do
    let entityField = case field of
                        "name"  -> DbShelf.ShelfName
                        -- "size" -> DbShelf.ShelfContent
                        _         -> DbShelf.ShelfName
    if any (not . isAlphaNum) searchStr
        then return []
        else do
            shelfs <- query pool $ selectList [Filter entityField (Left $ concat ["%", searchStr, "%"]) (BackendSpecificFilter "like")]
                                            [OffsetBy $ fromIntegral offset, LimitTo $ fromIntegral limit]
            return $ map (\(Just b) -> b) $ filter isJust $ map C.toJson shelfs
