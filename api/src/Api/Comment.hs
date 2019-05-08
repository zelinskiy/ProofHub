module Api.Comment (API, server) where

import Database.Persist.Postgresql
import Servant
import Data.Time.Clock
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Model
import Utils

type API =
        "list"
      :> Capture "pid" (Key Proof)
      :> Get '[JSON] [Entity Comment]
    :<|>"new"
      :> ReqBody '[JSON] Comment
      :> Post '[JSON] (Key Comment)
    :<|> "get"
      :> Capture "id" (Key Comment)
      :> Get '[JSON] (Maybe (Entity Comment))
    :<|> "delete"
      :> Capture "id" (Key Comment)
      :> Delete '[JSON] ()
    :<|> "update"
      :> Capture "id" (Key Comment)
      :> ReqBody '[JSON] Comment
      :> Post '[JSON] ()
    
server :: PrivateServer API
server = listComments
    :<|> newComment
    :<|> getComment
    :<|> deleteComment
    :<|> updateComment
  where
    listComments pid =
      undefined
    newComment c =
      undefined
    getComment cid =
      undefined
    deleteComment cid =
      undefined
    updateComment cid c =
      undefined
    
