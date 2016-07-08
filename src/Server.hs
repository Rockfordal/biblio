{-# LANGUAGE RecordWildCards #-}
module Server where

import Database.Persist.MySQL
import Servant

import API
import Logic.Users
import Logic.Shelfs
import Logic.Items
-- import Logic.Books


userServer :: ConnectionPool -> String -> Server UserAPI
userServer pool salt = hello
                  :<|> helloUser pool salt
                  :<|> selectUsersAuth pool salt
                  -- :<|> selectUsers pool

shelfServer :: ConnectionPool -> String -> Server ShelfAPI
shelfServer pool salt = showShelf pool
                  :<|> selectShelfs pool
                  :<|> selectShelfs pool (Just "title") (Just "") (Just 0) (Just 10)
                  :<|> createShelf pool salt
                  :<|> updateShelf pool salt
                  :<|> deleteShelf pool salt

itemServer :: ConnectionPool -> String -> Server ItemAPI
itemServer pool salt = showItem pool
                  :<|> selectItems pool
                  :<|> selectItems pool (Just "title") (Just "") (Just 0) (Just 10)
                  :<|> createItem pool salt
                  :<|> updateItem pool salt
                  :<|> deleteItem pool salt

-- bookServer :: ConnectionPool -> String -> Server BookAPI
-- bookServer pool salt = showBook pool
--                   :<|> selectBooks pool
--                   :<|> selectBooks pool (Just "title") (Just "") (Just 0) (Just 10)
--                   :<|> createBook pool salt
--                   :<|> updateBook pool salt
--                   :<|> deleteBook pool salt

basicServer :: ConnectionPool -> String -> Server BasicAPI
basicServer pool salt = userServer  pool salt
                   :<|> shelfServer pool salt
                   :<|> itemServer  pool salt
                  -- :<|> bookServer pool salt
