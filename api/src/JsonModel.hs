module JsonModel where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist
import Model (Project, User, Category)

data Login = Login
  { email :: String
  , pass :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Login
instance FromJSON Login

type RegisterData = Login

data ExtProject = ExtProject
  { project :: Entity Project
  , authors :: [Key User]
  , categories :: [Key Category]
  } deriving (Eq, Show, Generic)

instance ToJSON ExtProject
instance FromJSON ExtProject
