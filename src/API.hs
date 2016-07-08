{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module API where

import Servant.API
import Typer
import Json.User
import Json.Book
import Json.Shelf
import Data.Text
import Data.Word
import Data.Proxy

data API

type UserAPI = "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
          :<|> "hellouser" :> Header "Authorization" Text :> Get '[PlainText] String
          :<|> Header "Authorization" Text :> Get '[JSON] [User]
          -- :<|> "unsafesearch" :> QueryParam "searchField" String :> QueryParam "searchStr" String
          --   :> QueryParam "offset" Word16 :> QueryParam "limit" Word16 :> Get '[JSON] [User]

type BookAPI = Capture "id" Int :> Get '[JSON] (Maybe Book)
          :<|> "search" :> QueryParam "searchField" String :> QueryParam "searchStr" String
            :> QueryParam "offset" Word16 :> QueryParam "limit" Word16 :> Get '[JSON] [Book]
          :<|> Get '[JSON] [Book]
          :<|> Header "Authorization" Text :> ReqBody '[JSON] Book :> Post   '[JSON] String
          :<|> Header "Authorization" Text :> ReqBody '[JSON] Book :> Put    '[JSON] String
          :<|> Header "Authorization" Text :> Capture "id"    Int  :> Delete '[JSON] String

type ShelfAPI = Capture "id" Int :> Get '[JSON] (Maybe Shelf)
          :<|> "search" :> QueryParam "searchField" String :> QueryParam "searchStr" String
            :> QueryParam "offset" Word16 :> QueryParam "limit" Word16 :> Get '[JSON] [Shelf]
          :<|> Get '[JSON] [Shelf]
          :<|> Header "Authorization" Text :> ReqBody '[JSON] Shelf :> Post   '[JSON] String
          :<|> Header "Authorization" Text :> ReqBody '[JSON] Shelf :> Put    '[JSON] String
          :<|> Header "Authorization" Text :> Capture "id"    Int  :> Delete '[JSON] String

type UsersAPI  = "users" :> UserAPI
type BooksAPI  = "books" :> BookAPI
type ShelfsAPI = "shelfs" :> ShelfAPI

type BasicAPI = UsersAPI :<|> BooksAPI :<|> ShelfsAPI

userapi :: Proxy UserAPI
userapi =  Proxy

bookapi :: Proxy BookAPI
bookapi =  Proxy

shelfapi :: Proxy ShelfAPI
shelfapi =  Proxy

api :: Proxy API
api =  Proxy
