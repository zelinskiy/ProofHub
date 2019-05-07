module Api.Main(API, server) where

import Database.Persist.Postgresql
import Servant
import Servant.Auth.Server

import qualified Api.Auth
import qualified Api.User

import Model
import Utils

type API = 
       "public"   :> PublicApi
  :<|> "private"  :> Api.Auth.Private :> PrivateApi
  
type PublicApi =
       "greeting" :> Get '[JSON] String
  :<|> "jwt"      :> Api.Auth.PublicAPI
  :<|> "user"     :> Api.User.PublicAPI

type PrivateApi =
       "admin"      :> Get '[JSON] String
  :<|> "user"       :> Api.User.API
  
server :: ConnectionPool
       -> CookieSettings
       -> JWTSettings
       -> Server API
server p c jwt = publicServer p c jwt
                 :<|> privateServer p

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
privateServer _ _ = throwAll err401
