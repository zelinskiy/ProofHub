{-# LANGUAGE OverloadedStrings #-}

module App (app, startApp) where

import Network.Wai.Handler.Warp
import Database.Persist.Postgresql
import Servant
import Servant.Auth.Server
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Time
import Data.Text(Text)
import Network.Wai.Handler.WarpTLS
import Network.Wai.Middleware.Cors

import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding

import qualified Model
import qualified Api.Auth
import qualified Api.Main


postgresqlConnString :: BS.ByteString
postgresqlConnString = "host=localhost port=5432 user=test dbname=test password=test"

port :: Int
port = 8080

getConnectionPool :: IO ConnectionPool
getConnectionPool = do
  pool <- runStdoutLoggingT $
    createPostgresqlPool postgresqlConnString 5
  runSqlPool (runMigration Model.migrateAll) pool
  return pool

app :: IO Application
app = do
  pool <- getConnectionPool
  k <- generateKey
  let cs = defaultCookieSettings
         { cookieMaxAge = Just $ secondsToDiffTime 3600 }
      jwt = defaultJWTSettings k
      ctx = cs :. jwt :. EmptyContext
      api = Proxy :: Proxy Api.Main.API
  return $ serveWithContext api ctx (Api.Main.server pool cs jwt)


showLayout :: IO ()
showLayout = do
  pool <- getConnectionPool
  k <- generateKey
  let cs = defaultCookieSettings
         { cookieMaxAge = Just $ secondsToDiffTime 3600 }
      jwt = defaultJWTSettings k
      ctx = cs :. jwt :. EmptyContext
      api = Proxy :: Proxy Api.Main.API
  BS.putStrLn $ encodeUtf8 $ layoutWithContext api ctx


startApp :: IO ()
startApp = run port . myCors =<< app
  where
    myCors = cors (const $ Just policy)
    policy = CorsResourcePolicy
      { corsOrigins = Nothing
      , corsMethods = [ "GET"
                      , "HEAD"
                      , "POST"
                      , "DELETE"
                      , "PUT"
                      , "PATCH"]
      , corsRequestHeaders = [ "Accept"
                             , "Accept-Language"
                             , "Content-Language"
                             , "Access-Control-Allow-Origin"
                             , "Content-Type"
                             , "Set-Cookie"
                             , "Authorization"]
      , corsExposedHeaders = Just [ "Access-Control-Allow-Origin"
                                  , "Content-Type"
                                  , "Set-Cookie" ]
      , corsMaxAge = Nothing
      , corsVaryOrigin = False
      , corsRequireOrigin = False
      , corsIgnoreFailures = False }
