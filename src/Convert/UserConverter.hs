module Convert.UserConverter where

import Prelude hiding (id)

import Database.Persist.MySQL
import Data.Text

import qualified Db.User as D
import Json.User

toEntity :: User -> Maybe (Entity D.User)
toEntity (User maybeId _login _password _role) =
    case maybeId of
        Nothing -> Nothing
        Just _id -> Just $ Entity (toSqlKey (fromIntegral _id) :: Key D.User) $
                                D.User _login _password _role

toRecord :: User -> D.User
toRecord (User _ _login _password _role) = D.User _login _password _role

toJson :: Entity D.User -> Maybe User
toJson (Entity key (D.User _login _password _role)) =
    let vals = keyToValues key
    in processKeys vals

    where processKeys [pval] =
            let eitherId = fromPersistValue pval :: Either Text Int
            in case eitherId of
                Left _ -> Nothing
                Right _id -> Just $ User (Just $ fromIntegral _id) _login _password _role

          processKeys _ = Nothing

