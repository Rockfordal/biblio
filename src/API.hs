{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module API where

import Servant.API
import Typer
import Json.User
import Json.Book
import Data.Text
import Data.Word

type UserAPI = "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
               :<|> "hellouser" :> Header "Authorization" Text :> Get '[PlainText] String
               :<|> Header "Authorization" Text :> Get '[JSON] [User]

type BookAPI = Capture "id" Int :> Get '[JSON] (Maybe Book)
          :<|> "search" :> QueryParam "searchField" String :> QueryParam "searchStr" String
            :> QueryParam "offset" Word16 :> QueryParam "limit" Word16 :> Get '[JSON] [Book]
          :<|> Get '[JSON] [Book]
          :<|> Header "Authorization" Text :> ReqBody '[JSON] Book :> Post   '[JSON] String
          :<|> Header "Authorization" Text :> ReqBody '[JSON] Book :> Put    '[JSON] String
          :<|> Header "Authorization" Text :> Capture "id"    Int  :> Delete '[JSON] String

type UsersAPI = "users" :> UserAPI
type BooksAPI = "books" :> BookAPI

type BasicAPI = UsersAPI :<|> BooksAPI
