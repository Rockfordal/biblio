{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module API where

import Servant.API
import Typer
import Data.Text
import Data.Word
import Data.Proxy
import Json.User
import Json.Shelf
import Json.Item
-- import Json.Book

data API

type UserAPI = "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
          :<|> "hellouser" :> Header "Authorization" Text :> Get '[PlainText] String
          :<|> Header "Authorization" Text :> Get '[JSON] [User]
          -- :<|> "unsafesearch" :> QueryParam "searchField" String :> QueryParam "searchStr" String
          --   :> QueryParam "offset" Word16 :> QueryParam "limit" Word16 :> Get '[JSON] [User]

type ShelfAPI = Capture "id" Int :> Get '[JSON] (Maybe Shelf)
          :<|> "search" :> QueryParam "searchField" String :> QueryParam "searchStr" String
            :> QueryParam "offset" Word16 :> QueryParam "limit" Word16 :> Get '[JSON] [Shelf]
          :<|> Get '[JSON] [Shelf]
          :<|> Header "Authorization" Text :> ReqBody '[JSON] Shelf :> Post   '[JSON] String
          :<|> Header "Authorization" Text :> ReqBody '[JSON] Shelf :> Put    '[JSON] String
          :<|> Header "Authorization" Text :> Capture "id"    Int   :> Delete '[JSON] String

type ItemAPI = Capture "id" Int :> Get '[JSON] (Maybe Item)
          :<|> "search" :> QueryParam "searchField" String :> QueryParam "searchStr" String
            :> QueryParam "offset" Word16 :> QueryParam "limit" Word16 :> Get '[JSON] [Item]
          :<|> Get '[JSON] [Item]
          :<|> Header "Authorization" Text :> ReqBody '[JSON] Item  :> Post   '[JSON] String
          :<|> Header "Authorization" Text :> ReqBody '[JSON] Item  :> Put    '[JSON] String
          :<|> Header "Authorization" Text :> Capture "id"    Int   :> Delete '[JSON] String

-- type BookAPI = Capture "id" Int :> Get '[JSON] (Maybe Book)
--           :<|> "search" :> QueryParam "searchField" String :> QueryParam "searchStr" String
--             :> QueryParam "offset" Word16 :> QueryParam "limit" Word16 :> Get '[JSON] [Book]
--           :<|> Get '[JSON] [Book]
--           :<|> Header "Authorization" Text :> ReqBody '[JSON] Book :> Post   '[JSON] String
--           :<|> Header "Authorization" Text :> ReqBody '[JSON] Book :> Put    '[JSON] String
--           :<|> Header "Authorization" Text :> Capture "id"    Int  :> Delete '[JSON] String

type UsersAPI  = "admin" :> "users" :> UserAPI
type ShelfsAPI = "shelfs" :> ShelfAPI
type ItemsAPI  = "items" :> ItemAPI
-- type BooksAPI  = "books" :> BookAPI

type BasicAPI = UsersAPI :<|> ShelfsAPI :<|> ItemsAPI

userapi :: Proxy UserAPI
userapi =  Proxy

shelfapi :: Proxy ShelfAPI
shelfapi =  Proxy

itemapi :: Proxy ItemAPI
itemapi =  Proxy

-- bookapi :: Proxy BookAPI
-- bookapi =  Proxy

api :: Proxy API
api =  Proxy
