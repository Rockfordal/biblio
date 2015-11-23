{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Logic.Books where

import Database.Persist.MySQL
import Servant
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.Text hiding (replace, length, any, concat, map, filter)
import Data.Char
import Data.Maybe

import Auth.DbAuth
import Db.Common
import Domain.Book (Book(..))
import qualified Domain.Book as DomainBook
import qualified Db.Book as DbBook
import Domain.User (User(..))
import qualified Domain.User as DomainUser


createBook :: ConnectionPool -> String -> Maybe Text -> Book -> EitherT ServantErr IO ()
createBook _ _ Nothing _ = return ()
createBook pool salt (Just authHeader) book =
    withUser pool authHeader salt $ \user -> do
        query pool $ insert (toRecord book {user_id = DomainUser.id user} :: DbBook.Book)
        return ()

bookBelongsToUser :: MonadIO m => ConnectionPool -> User -> Int -> m Bool
bookBelongsToUser _ (User {DomainUser.id = Nothing}) _ = return False -- No user id
bookBelongsToUser pool (User {DomainUser.id = _id}) _bid = do
    books <- query pool $ selectList [DbBook.BookUser_id ==. _id, DbBook.BookId ==. toKey _bid] []
    return $ length books == 1

updateBook :: ConnectionPool -> String -> Maybe Text -> Book -> EitherT ServantErr IO ()
updateBook _ _ Nothing _ = return ()
updateBook pool salt (Just authHeader) book =
    withUser pool authHeader salt $ \user ->
        case book of
            Book {DomainBook.id = Nothing} -> left $ err400 { errBody = "No id specified!" }
            Book {DomainBook.id = Just _id} -> do
                belongs <- bookBelongsToUser pool user _id
                if belongs
                    then do
                        query pool $ replace (toKey _id :: Key DbBook.Book)
                                             (toRecord book {user_id = DomainUser.id user} :: DbBook.Book)
                        return ()
                    else return ()

deleteBook :: ConnectionPool -> String -> Maybe Text -> Int -> EitherT ServantErr IO ()
deleteBook _ _ Nothing _ = return ()
deleteBook pool salt (Just authHeader) _id =
    withUser pool authHeader salt $ \user -> do
        belongs <- bookBelongsToUser pool user _id
        if belongs
            then do
                query pool $ delete (toKey _id :: Key DbBook.Book)
                return ()
            else return ()
        

showBook :: ConnectionPool -> Int -> EitherT ServantErr IO (Maybe Book)
showBook pool id = do
    book <- query pool $ selectFirst [DbBook.BookId ==. (toKey id :: Key DbBook.Book)] []
    return $ maybeBook book
    where maybeBook Nothing = Nothing
          maybeBook (Just b) = toDomain b


selectBooks :: ConnectionPool -> Maybe String  -> EitherT ServantErr IO [Book]
selectBooks _ Nothing = return []
selectBooks pool (Just searchStr) =
    if any (not . isAlphaNum) searchStr
        then return []
        else do
            books <- query pool $ selectList [Filter DbBook.BookTitle (Left $ concat ["%", searchStr, "%"]) (BackendSpecificFilter "like")] []
            return $ map (\(Just b) -> b) $ filter isJust $ map toDomain books
