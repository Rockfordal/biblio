{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Logic.Books where

import Prelude hiding (id)
import Database.Persist.MySQL
import Servant
-- import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class
import Data.Text hiding (replace, length, any, concat, map, filter)
import Data.Char
import Data.Maybe
import Data.Word

import Auth.DbAuth
import Db.Common
import Json.Book (Book(..))
import qualified Json.Book as JsonBook
import qualified Db.Book as DbBook
import Json.User (User(User)) -- (User(..))
import qualified Json.User as JsonUser
import qualified Convert.BookConverter as C
-- import Typer


createBook :: ConnectionPool -> String -> Maybe Text -> Book -> Handler [Char]
createBook _ _ Nothing _ = return "nej"
createBook pool salt (Just authHeader) book =
    withUser pool authHeader salt $ \user -> do
        -- query pool $ insert (C.toRecord book {user_id = JsonUser.id user} :: DbBook.Book)
        _ <- ($) query pool $ insert (C.toRecord book {user_id = JsonUser.id user} :: DbBook.Book)
        return "ja"

bookBelongsToUser :: MonadIO m => ConnectionPool -> User -> Int -> m Bool
bookBelongsToUser _ (User {JsonUser.id = Nothing}) _ = return False -- No user id
bookBelongsToUser pool (User {JsonUser.id = _id}) _bid = do
    books <- query pool $ selectList [DbBook.BookUser_id ==. _id, DbBook.BookId ==. toKey _bid] []
    return $ length books == 1

updateBook :: ConnectionPool -> String -> Maybe Text -> Book -> Handler [Char]
updateBook _ _ Nothing _ = return "nej"
updateBook pool salt (Just authHeader) book =
    withUser pool authHeader salt $ \user ->
        case book of
            Book {JsonBook.id = Nothing} -> throwError $ err400 { errBody = "No id specified!" }
            Book {JsonBook.id = Just _id} -> do
                belongs <- bookBelongsToUser pool user _id
                if belongs
                    then do
                        query pool $ replace (toKey _id :: Key DbBook.Book)
                                             (C.toRecord book {user_id = JsonUser.id user} :: DbBook.Book)
                        return "ja"
                    else return "nej"


deleteBook :: ConnectionPool -> String -> Maybe Text -> Int -> Handler String
deleteBook _ _ Nothing _ = return "nej"
deleteBook pool salt (Just authHeader) _id =
    withUser pool authHeader salt $ \user -> do
        belongs <- bookBelongsToUser pool user _id
        if belongs
            then do
                query pool $ delete (toKey _id :: Key DbBook.Book)
                return "ja"
            else throwError $ err400 { errBody = "deleteBook error"}


showBook :: ConnectionPool -> Int -> Handler (Maybe Book)
showBook pool bookid = do
    -- book <- query pool $ selectFirst [DbBook.BookId ==. (toKey id :: Key DbBook.Book)] []
    book <- query pool $ selectFirst [DbBook.BookId ==. (toKey bookid :: Key DbBook.Book)] []
    return $ maybeBook book
    where maybeBook Nothing = Nothing
          maybeBook (Just b) = C.toJson b

selectBooks :: ConnectionPool -> Maybe String -> Maybe String -> Maybe Word16 -> Maybe Word16 -> Handler [Book]
selectBooks _ Nothing _ _ _ = return []
selectBooks _ _ Nothing _ _ = return []
selectBooks _ _ _ Nothing _ = return []
selectBooks _ _ _ _ Nothing = return []
selectBooks pool (Just field) (Just searchStr) (Just offset) (Just limit) = do
    let entityField = case field of
                        "author"  -> DbBook.BookAuthor
                        "content" -> DbBook.BookContent
                        _         -> DbBook.BookTitle
    if any (not . isAlphaNum) searchStr
        then return []
        else do
            books <- query pool $ selectList [Filter entityField (Left $ concat ["%", searchStr, "%"]) (BackendSpecificFilter "like")]
                                            [OffsetBy $ fromIntegral offset, LimitTo $ fromIntegral limit]
            return $ map (\(Just b) -> b) $ filter isJust $ map C.toJson books
