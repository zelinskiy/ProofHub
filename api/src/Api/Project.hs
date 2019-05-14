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
      :> Get '[JSON] (Maybe Project)
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
    searchProjects mbUserEmail mbCategoryName mbContainsString = do
      me <- ask
      myProjectsIds <- db $ map (projectAuthorProjectId . entityVal)
        <$> selectList [ ProjectAuthorUserId ==. entityKey me ] []
      db $ selectList [ ProjectId <-. myProjectsIds ] []
    newProject p = do
      me <- ask
      pid <- db $ insert p
      db $ insert $ ProjectAuthor (entityKey me) pid True
      return pid
    getProject =
      db . get
    deleteProject =
      db . deleteCascade
    updateProject pid = do
      db . replace pid
    addAuthor uid pid = do
      db $ insert $ ProjectAuthor uid pid False
      return ()
    removeAuthor uid pid = do
      db $ delete $ ProjectAuthorKey uid pid
      return ()
    
