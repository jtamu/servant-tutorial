{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Dto.User where

import Data.Aeson (FromJSON)
import Data.Int (Int32)
import Data.Time (Day)
import Database.HDBC.Query.TH (makeRelationalRecord)
import GHC.Generics (Generic)
import Prelude hiding (id)

data UserDto = UserDto
  { name :: String,
    age :: Int32,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserDto

$(makeRelationalRecord ''UserDto)