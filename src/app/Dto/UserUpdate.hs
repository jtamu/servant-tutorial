{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Dto.UserUpdate where

import Data.Aeson (FromJSON)
import Data.Int (Int32)
import Data.Time (Day)
import GHC.Generics (Generic)
import Prelude hiding (id)

data UserUpdateDto = UserUpdateDto
  { name :: Maybe String,
    age :: Maybe Int32,
    email :: Maybe String,
    registration_date :: Maybe Day
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserUpdateDto
