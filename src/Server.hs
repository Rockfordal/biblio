{-# LANGUAGE RecordWildCards #-}
module Server where

import Database.Persist.MySQL
import Servant

import Logic.Users
import Logic.Books
import Logic.Shelfs
import API


userServer :: ConnectionPool -> String -> Server UserAPI
userServer pool salt = hello
                  :<|> helloUser pool salt
                  :<|> selectUsersAuth pool salt
                  -- :<|> selectUsers pool

bookServer :: ConnectionPool -> String -> Server BookAPI
bookServer pool salt = showBook pool
                  :<|> selectBooks pool
                  :<|> selectBooks pool (Just "title") (Just "") (Just 0) (Just 10)
                  :<|> createBook pool salt
                  :<|> updateBook pool salt
                  :<|> deleteBook pool salt

shelfServer :: ConnectionPool -> String -> Server ShelfAPI
shelfServer pool salt = showShelf pool
                  :<|> selectShelfs pool
                  :<|> selectShelfs pool (Just "title") (Just "") (Just 0) (Just 10)
                  :<|> createShelf pool salt
                  :<|> updateShelf pool salt
                  :<|> deleteShelf pool salt

basicServer :: ConnectionPool -> String -> Server BasicAPI
basicServer pool salt = userServer pool salt
                  :<|> bookServer pool salt
                  :<|> shelfServer pool salt
