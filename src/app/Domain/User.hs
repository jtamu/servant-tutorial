module Domain.User where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

data User = User
  { userId :: Int64,
    name :: String,
    age :: Int,
    email :: String,
    registrationDate :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User
