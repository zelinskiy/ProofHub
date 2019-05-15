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
      :> Get '[JSON] (Maybe Comment)
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
      db $ selectList [CommentProofId ==. pid] [ Asc CommentId ]
    newComment =
      db . insert
    getComment =
      db . get
    deleteComment =
      db . deleteCascade
    updateComment cid =
      db . replace cid
    
