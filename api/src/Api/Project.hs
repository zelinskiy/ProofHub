module Api.Project (API, server) where

import Database.Persist.Postgresql
import Servant
import Data.Time.Clock
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Model
import Utils

type API =
        "search"
      :> QueryParam "user" String
      :> QueryParam "category" String
      :> QueryParam "contains" String
      :> Get '[JSON] [Entity Project]
    :<|> "new"
      :> ReqBody '[JSON] Project
      :> Post '[JSON] (Key Project)
    :<|> "get"
      :> Capture "id" (Key Project)
      :> Get '[JSON] (Maybe (Entity Project))
    :<|> "delete"
      :> Capture "id" (Key Project)
      :> Delete '[JSON] ()
    :<|> "update"
      :> Capture "id" (Key Project)
      :> ReqBody '[JSON] Project
      :> Post '[JSON] ()
    :<|> "author" :> "add" 
      :> Capture "uid" (Key User)
      :> Capture "pid" (Key Project)
      :> Get '[JSON] ()
    :<|> "author" :> "remove" 
      :> Capture "uid" (Key User)
      :> Capture "pid" (Key Project)
      :> Get '[JSON] ()  
    
server :: PrivateServer API
server = searchProjects
    :<|> newProject
    :<|> getProject
    :<|> deleteProject
    :<|> updateProject
    :<|> addAuthor
    :<|> removeAuthor
  where
    searchProjects mbUserEmail mbCategoryName mbContainsString =
      undefined
    newProject p =
      undefined
    getProject pid =
      undefined
    deleteProject pid =
      undefined
    updateProject pid p =
      undefined
    addAuthor uid pid =
      undefined
    removeAuthor uid pid =
      undefined
    
