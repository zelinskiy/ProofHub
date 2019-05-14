module Api.Project (API, server) where

import Database.Persist.Postgresql
import Servant
import Data.Time.Clock
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad

import Model
import JsonModel
import Utils

-- TODO: abstract out boilerplate from get an search

type API =
        "search"
      :> QueryParam "user" String
      :> QueryParam "category" String
      :> QueryParam "contains" String
      :> Get '[JSON] [ExtProject]
    :<|> "new"
      :> ReqBody '[JSON] ExtProject
      :> Post '[JSON] (Key Project)
    :<|> "get"
      :> Capture "id" (Key Project)
      :> Get '[JSON] (Maybe ExtProject)
    :<|> "delete"
      :> Capture "id" (Key Project)
      :> Delete '[JSON] ()
    :<|> "update"
      :> Capture "id" (Key Project)
      :> ReqBody '[JSON] ExtProject
      :> Post '[JSON] ()
    
server :: PrivateServer API
server = searchProjects
    :<|> newProject
    :<|> getProject
    :<|> deleteProject
    :<|> updateProject
  where
    searchProjects mbUserEmail mbCategoryName mbContainsString = do
      me <- ask
      myProjectsIds <- db $ map (projectAuthorProjectId . entityVal)
        <$> selectList [ ProjectAuthorUserId ==. entityKey me ] []
      myProjects <- db $ selectList [ ProjectId <-. myProjectsIds ] []
      myExtProjects <- forM myProjects $ \p -> do
        pAuthors <- db $ map (projectAuthorUserId . entityVal)
          <$> selectList [ ProjectAuthorProjectId ==. entityKey p ] []
        pCats <- db $ map (projectCategoryCategoryId . entityVal)
          <$> selectList [ ProjectCategoryProjectId ==. entityKey p ] []
        return $ ExtProject p pAuthors pCats
      return myExtProjects
    newProject p = do
      me <- ask
      pid <- db $ insert $ entityVal $ project p
      db $ insert
        $ ProjectAuthor (entityKey me) pid True
      db $ insertMany
        $ map (\e -> ProjectAuthor e pid False)
        $ authors p
      db $ insertMany
        $ map (\c -> ProjectCategory pid c)
        $ categories p
      return pid
    getProject pid = do
      mbP <- db $ getEntity pid
      case mbP of
        Nothing -> return Nothing
        Just p -> do
          pAuthors <- db $ map (projectAuthorUserId . entityVal)
                      <$> selectList [ ProjectAuthorProjectId ==. entityKey p ] []
          pCats <- db $ map (projectCategoryCategoryId . entityVal)
                   <$> selectList [ ProjectCategoryProjectId ==. entityKey p ] []
          return $ Just $ ExtProject p pAuthors pCats
    deleteProject =
      db . deleteCascade
    updateProject pid extP =
      -- TODO: update Authors and Categories
      db $ replace pid $ entityVal $ project extP
    
