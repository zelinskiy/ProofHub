module Api.Directory (API, server) where

import Database.Persist.Postgresql
import Servant
import Data.Time.Clock
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Model
import Utils

type API =
        "list"
      :> Capture "pid" (Key Project)
      :> Get '[JSON] [Entity Directory]
    :<|>"new"
      :> ReqBody '[JSON] Directory
      :> Post '[JSON] (Key Directory)
    :<|> "get"
      :> Capture "id" (Key Directory)
      :> Get '[JSON] (Maybe (Entity Directory))
    :<|> "delete"
      :> Capture "id" (Key Directory)
      :> Delete '[JSON] ()
    :<|> "update"
      :> Capture "id" (Key Directory)
      :> ReqBody '[JSON] Directory
      :> Post '[JSON] ()    
    
server :: PrivateServer API
server = listDirectories
    :<|> newDirectory
    :<|> getDirectory
    :<|> deleteDirectory
    :<|> updateDirectory
  where
    listDirectories pid =
      undefined
    newDirectory p =
      undefined
    getDirectory did =
      undefined
    deleteDirectory did =
      undefined
    updateDirectory did d =
      undefined
    
