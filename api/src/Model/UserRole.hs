{-# LANGUAGE TemplateHaskell #-}
module Model.UserRole where

import Database.Persist.TH
import Prelude
import GHC.Generics
import Data.Aeson

data UserRole
  = Normal
  | Moderator
  | Admin
    deriving (Show, Read, Eq, Ord, Generic)
derivePersistField "UserRole"

instance ToJSON UserRole
instance FromJSON UserRole
