module Convert.ShelfConverter where

import Prelude hiding (id)

import Database.Persist.MySQL
import Data.Text

import qualified Db.Shelf as S
import Json.Shelf

toEntity :: Shelf -> Maybe (Entity S.Shelf)
toEntity domEntity =
    case id domEntity of
        Nothing -> Nothing
        Just _id -> Just $ Entity (toSqlKey (fromIntegral _id) :: Key S.Shelf) $ toRecord domEntity

toRecord :: Shelf -> S.Shelf
toRecord (Shelf _id _name _size) = S.Shelf _name _size

toJson :: Entity S.Shelf -> Maybe Shelf
toJson (Entity key (S.Shelf _name _size)) =
    let vals = keyToValues key
    in processKeys vals

    where processKeys [pval] =
            let eitherId = fromPersistValue pval :: Either Text Int
            in case eitherId of
                Left _ -> Nothing
                Right _id -> Just $ Shelf (Just $ fromIntegral _id) _name _size
          processKeys _ = Nothing

