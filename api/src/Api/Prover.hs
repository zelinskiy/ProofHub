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
      :> Capture "title" (Key Prover)
      :> Delete '[JSON] ()
    :<|> "update"
      :> Capture "title" (Key Prover)
      :> ReqBody '[JSON] Prover
      :> Post '[JSON] ()
    
server :: PrivateServer API
server = listProvers
    :<|> newProver
    :<|> deleteProver
    :<|> updateProver
  where
    listProvers =
      db $ selectList [] [ Asc ProverTitle ]
    newProver =
      db . insert
    deleteProver =
      db . delete
    updateProver ptitle =
      db . replace ptitle
