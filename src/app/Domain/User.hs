{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.User
  ( Age,
    makeAge,
    pattern Age,
    defaultAge,
    User (User, _userId, _name, _age, _email, _registrationDate),
    userId,
    name,
    age,
    email,
    registrationDate,
  )
where

import Config.Json (customOptions)
import Control.Lens.TH (makeLenses)
import Data.Aeson (ToJSON (toJSON), genericToJSON)
import Data.Int (Int64)
import Data.Map qualified as M (fromList)
import Data.Time.Calendar (Day)
import Data.Validation (Validation (Failure))
import Dto.ValidationError (ValidationError (ValidationError))
import GHC.Generics (Generic)

newtype Age = Age_ Int deriving (Show, Eq, Generic)

instance ToJSON Age

makeAge :: Int -> Validation ValidationError Age
makeAge n | n >= 18 = pure $ Age_ n
makeAge _ = Failure $ ValidationError $ M.fromList [("age", ["年齢は18歳以上で指定してください"])]

pattern Age :: Int -> Age
pattern Age n <- Age_ n

defaultAge :: Age
defaultAge = Age_ 0

data User = User
  { _userId :: Int64,
    _name :: String,
    _age :: Age,
    _email :: String,
    _registrationDate :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User where
  toJSON = genericToJSON customOptions

$(makeLenses ''User)
