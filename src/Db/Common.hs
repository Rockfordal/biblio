{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Db.Common where

import Control.Monad.IO.Class
import Database.Persist.MySQL

query :: MonadIO m => ConnectionPool -> SqlPersistT IO a -> m a
query pool query = liftIO $ runSqlPool query pool

toKey :: ToBackendKey SqlBackend record => Int -> Key record
toKey _id = toSqlKey $ fromIntegral _id

class DomainEntity a e where
    toEntity :: a -> Maybe (Entity e)
    toRecord :: a -> e
    toDomain :: Entity e -> Maybe a

