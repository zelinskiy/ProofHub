module Api.Proof (API, server) where

import Database.Persist.Postgresql
import Servant
import Data.Time.Clock
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Model
import Utils

type API =
        "list"
      :> Capture "did" (Key Directory)
      :> Get '[JSON] [Entity Proof]
    :<|>"new"
      :> ReqBody '[JSON] Proof
      :> Post '[JSON] (Key Proof)
    :<|> "get"
      :> Capture "id" (Key Proof)
      :> Get '[JSON] (Maybe Proof)
    :<|> "delete"
      :> Capture "id" (Key Proof)
      :> Delete '[JSON] ()
    :<|> "update"
      :> Capture "id" (Key Proof)
      :> ReqBody '[JSON] Proof
      :> Post '[JSON] ()
    
server :: PrivateServer API
server = listProofs
    :<|> newProof
    :<|> getProof
    :<|> deleteProof
    :<|> updateProof
  where
    listProofs did =
      db $ selectList [ProofDirectoryId ==. did] []
    newProof =
      db . insert
    getProof =
      db . get
    deleteProof =
      db . delete
    updateProof did =
      db . replace did
    
