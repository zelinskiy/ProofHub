module Api.Main(API, server) where

import Database.Persist.Postgresql
import Servant
import Servant.Auth.Server
import Servant.Utils.StaticFiles

import qualified Api.Auth
import qualified Api.User
import qualified Api.Project
import qualified Api.Directory
import qualified Api.Proof
import qualified Api.Comment
import qualified Api.Category
import qualified Api.Prover
import qualified Api.File

import Model
import Utils

type API = 
       "public"   :> PublicApi
  :<|> "private"  :> Api.Auth.Private :> PrivateApi
  :<|> "static"   :> Raw
  :<|> "file"     :> Api.File.API
  
type PublicApi =
       "greeting" :> Get '[JSON] String
  :<|> "jwt"      :> Api.Auth.PublicAPI
  :<|> "user"     :> Api.User.PublicAPI

type PrivateApi =
       "admin"     :> Get '[JSON] String
  :<|> "user"      :> Api.User.API
  :<|> "project"   :> Api.Project.API
  :<|> "directory" :> Api.Directory.API
  :<|> "proof"     :> Api.Proof.API
  :<|> "comment"   :> Api.Comment.API
  :<|> "category"  :> Api.Category.API
  :<|> "prover"    :> Api.Prover.API
  
server :: ConnectionPool
       -> CookieSettings
       -> JWTSettings
       -> Server API
server p c jwt = publicServer p c jwt
            :<|> privateServer p
            :<|> serveDirectoryFileServer "static/"
            :<|> Api.File.server

publicServer :: ConnectionPool
             -> CookieSettings
             -> JWTSettings
             -> Server PublicApi
publicServer p c jwt = hoistServer (Proxy :: Proxy PublicApi) (publicToNormalH p) $
       return "Greetings!"
  :<|> Api.Auth.publicServer c jwt
  :<|> Api.User.publicServer  

privateServer :: ConnectionPool
              -> AuthResult (Entity User)
              -> Server PrivateApi
privateServer p (Authenticated u) =
  hoistServer (Proxy :: Proxy PrivateApi) (publicToNormalH p . privateToPublicH u) $
       return "Hello admin!"
       :<|> Api.User.server
       :<|> Api.Project.server
       :<|> Api.Directory.server
       :<|> Api.Proof.server
       :<|> Api.Comment.server
       :<|> Api.Category.server
       :<|> Api.Prover.server
privateServer _ _ = throwAll err401
