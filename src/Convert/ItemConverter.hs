module Convert.ItemConverter where

import Prelude hiding (id)

import Database.Persist.MySQL
import Data.Text

import qualified Db.Item as I
import Json.Item

toEntity :: Item -> Maybe (Entity I.Item)
toEntity domEntity =
    case id domEntity of
        Nothing -> Nothing
        Just _id -> Just $ Entity (toSqlKey (fromIntegral _id) :: Key I.Item) $ toRecord domEntity

toRecord :: Item -> I.Item
toRecord (Item _id _name _info) = I.Item _name _info

toJson :: Entity I.Item -> Maybe Item
toJson (Entity key (I.Item _name _info)) =
    let vals = keyToValues key
    in processKeys vals

    where processKeys [pval] =
            let eitherId = fromPersistValue pval :: Either Text Int
            in case eitherId of
                Left _ -> Nothing
                Right _id -> Just $ Item (Just $ fromIntegral _id) _name _info
          processKeys _ = Nothing
