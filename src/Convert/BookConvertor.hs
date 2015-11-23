module Convert.BookConvertor where

import Prelude hiding (id)
import GHC.Generics (Generic)
import qualified Data.Aeson as A

import Database.Persist.MySQL
import Data.Text

import Db.Common
import qualified Db.Book as D
import Json.Book

toEntity domEntity =
    case id domEntity of
        Nothing -> Nothing
        Just _id -> Just $ Entity (toSqlKey (fromIntegral _id) :: Key D.Book) $ toRecord domEntity

toRecord domEntity = D.Book (title domEntity)
                          (author domEntity)
                          (content domEntity)
                          (year domEntity)
                          (user_id domEntity)

toJson (Entity key entity) =
    let vals = keyToValues key
    in processKeys vals
    
    where processKeys [pval] =
            let eitherId = fromPersistValue pval :: Either Text Int
            in case eitherId of
                Left _ -> Nothing
                Right _id -> Just $ Book (Just $ fromIntegral _id)
                                            (D.bookTitle entity)
                                            (D.bookAuthor entity)
                                            (D.bookContent entity)
                                            (D.bookYear entity)
                                            (D.bookUser_id entity)                
          processKeys _ = Nothing

