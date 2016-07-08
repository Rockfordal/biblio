module Convert.BookConverter where

import Prelude hiding (id)

import Database.Persist.MySQL
import Data.Text

import qualified Db.Book as D
import Json.Book

toEntity :: Book -> Maybe (Entity D.Book)
toEntity domEntity =
    case id domEntity of
        Nothing -> Nothing
        Just _id -> Just $ Entity (toSqlKey (fromIntegral _id) :: Key D.Book) $ toRecord domEntity

toRecord :: Book -> D.Book
toRecord (Book _id _title _author _content _year _user_id) = D.Book _title _author _content _year _user_id

toJson :: Entity D.Book -> Maybe Book
toJson (Entity key (D.Book _title _author _content _year _user_id)) =
    let vals = keyToValues key
    in processKeys vals

    where processKeys [pval] =
            let eitherId = fromPersistValue pval :: Either Text Int
            in case eitherId of
                Left _ -> Nothing
                Right _id -> Just $ Book (Just $ fromIntegral _id) _title _author _content _year _user_id
          processKeys _ = Nothing

