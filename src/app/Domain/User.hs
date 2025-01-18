{-# LANGUAGE TemplateHaskell #-}

module Domain.User where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

data User = User
  { _userId :: Int64,
    _name :: String,
    _age :: Int,
    _email :: String,
    _registrationDate :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User

$(makeLenses ''User)
