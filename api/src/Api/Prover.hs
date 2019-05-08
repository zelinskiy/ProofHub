module Api.Prover (API, server) where

import Database.Persist.Postgresql
import Servant
import Data.Time.Clock
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Model
import Utils

type API =
        "list"
      :> Get '[JSON] [Entity Prover]
    :<|>"new"
      :> ReqBody '[JSON] Prover
      :> Post '[JSON] (Key Prover)
    :<|> "delete"
      :> Capture "title" String
      :> Capture "version" String
      :> Delete '[JSON] ()
    :<|> "update"
      :> Capture "title" String
      :> Capture "version" String
      :> ReqBody '[JSON] Prover
      :> Post '[JSON] ()
    
server :: PrivateServer API
server = listProvers
    :<|> newProver
    :<|> deleteProver
    :<|> updateProver
  where
    listProvers =
      undefined
    newProver p =
      undefined
    deleteProver pname pversion =
      undefined
    updateProver pname pversion =
      undefined
