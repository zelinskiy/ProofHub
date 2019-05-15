module Api.Category (API, server) where

import Database.Persist.Postgresql
import Servant
import Data.Time.Clock
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Model
import Utils

type API =
        "list"
      :> Get '[JSON] [Entity Category]
    :<|>"new"
      :> ReqBody '[JSON] Category
      :> Post '[JSON] (Key Category)
    :<|> "delete"
      :> Capture "title" (Key Category)
      :> Delete '[JSON] ()
    :<|> "update"
      :> Capture "title" (Key Category)
      :> ReqBody '[JSON] Category
      :> Post '[JSON] ()
    
server :: PrivateServer API
server = listCategories
    :<|> newCategory
    :<|> deleteCategory
    :<|> updateCategory
  where
    listCategories =
      db $ selectList [] [ Asc CategoryTitle ]
    newCategory =
      db . insert
    deleteCategory =
      db . delete
    updateCategory ctitle =
      db . replace ctitle
