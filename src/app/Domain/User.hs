module Domain.User where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

data User = User
  { userId :: Int32,
    name :: String,
    age :: Int32,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User
