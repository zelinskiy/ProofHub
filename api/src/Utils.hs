{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Utils where

import Database.Persist.Sql
import Servant
import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans.Reader
import Control.Natural
import Control.Monad.Trans.Class

import Model

type PublicHandler = ReaderT ConnectionPool Handler
type PrivateHandler = ReaderT (Entity User) PublicHandler

type PublicServer api = ServerT api PublicHandler
type PrivateServer api = ServerT api PrivateHandler

privateToPublicH :: Entity User
                 -> (forall x. PrivateHandler x -> PublicHandler x)
privateToPublicH u = \h -> runReaderT h u

publicToNormalH :: ConnectionPool
                -> (forall x. PublicHandler x -> Handler x)
publicToNormalH p = \h -> runReaderT h p

-- There might be a hack with Type Families or else

db q = lift ask >>= liftIO . runSqlPersistMPool q

db2 q = ask >>= liftIO . runSqlPersistMPool q

hash :: String -> String
hash = BS.unpack . BS.take 20 . SHA256.hash . BS.pack

enterRole p = return $ checkRole p

checkRole p = NT $ \h -> do
  r <- userRole <$> entityVal <$> ask
  if p r then h else throwError err401

--vertical (componentwise) composition of NT
ver :: f :~> g -> g :~> h -> f :~> h
ver (NT eta) (NT eps) = NT $ eps . eta
