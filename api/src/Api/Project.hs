module Api.Project (API, server) where

import Database.Persist.Postgresql
import Servant
import Control.Monad.Trans.Reader
import Control.Monad
import Data.List (nub, isInfixOf)
import Data.List.Split

import Model
import JsonModel
import Utils

-- TODO: abstract out boilerplate from get an search

type API =
        "search"
      :> QueryParam "users" String
      :> QueryParam "categories" String
      :> QueryParam "provers" String
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
    searchProjects mbUserEmails mbCategoryNames mbProversList mbContainsString = do
      emailsFilter <-
            case mbUserEmails of
              Nothing -> return []
              Just emails -> do
                emailKeys <- db $ selectList [ UserEmail <-. splitOn "," emails ] []
                return [ ProjectAuthorUserId <-. map entityKey emailKeys ]
      catsFilter <-
            case mbCategoryNames of
              Nothing ->
                return []
              Just cats -> do
                catsKeys <- db $ selectList [ CategoryTitle <-. splitOn "," cats ] []
                return [ ProjectCategoryCategoryId <-. map entityKey catsKeys ]
      proversFilter <-
            case mbProversList of
              Nothing ->
                return []
              Just provers -> do
                proversKeys <- db $ selectList [ ProverTitle <-. splitOn "," provers ] []
                return [ ProjectProverId <-. map entityKey proversKeys ]
      let containsFilter (Entity _ p) =
            case mbContainsString of
              Nothing -> True
              Just q -> q `isInfixOf` projectTitle p
                || q `isInfixOf` projectShortDescription p
      -- me <- ask
      projects1 <- db $ map (projectAuthorProjectId . entityVal)
        <$> selectList emailsFilter []
      projects2 <- db $ map (projectCategoryProjectId . entityVal)
        <$> selectList catsFilter []
      let projectsIds = nub $ projects1 ++ projects2
      myProjects_ <- db $ selectList ([ ProjectId <-. projectsIds ] ++ proversFilter) [ Asc ProjectAdded ]
      let myProjects = filter containsFilter myProjects_
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
                      <$> selectList [ ProjectAuthorProjectId
                                       ==. entityKey p ] []
          pCats <- db $ map (projectCategoryCategoryId
                             . entityVal)
                   <$> selectList [ ProjectCategoryProjectId
                                    ==. entityKey p ] []
          return $ Just $ ExtProject p pAuthors pCats
    deleteProject =
      db . deleteCascade
    updateProject pid extP = do
      let p = entityVal $ project extP
      db $ deleteWhere [ProjectCategoryProjectId
                        ==. entityKey (project extP) ]
      db $ deleteWhere [ ProjectAuthorProjectId
                         ==. entityKey (project extP)
                       , ProjectAuthorOwner ==. False]
      owner <- db $ selectFirst [ ProjectAuthorProjectId
                                  ==. entityKey (project extP)
                                , ProjectAuthorOwner ==. True] []
      db $ insertMany
        $ map (\c -> ProjectCategory pid c)
        $ categories extP
      db $ insertMany
        $ map (\e -> ProjectAuthor e pid False)
        $ filter (\e -> Just e /= (projectAuthorUserId <$> entityVal <$> owner))
        $ authors extP
      db $ replace pid p
    
