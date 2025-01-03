module Domain.User where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

data User = User
  { userId :: Int,
    name :: String,
    age :: Int,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User
