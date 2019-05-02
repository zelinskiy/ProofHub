module JsonModel where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data Login = Login
  { email :: String
  , pass :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Login
instance FromJSON Login

type RegisterData = Login
