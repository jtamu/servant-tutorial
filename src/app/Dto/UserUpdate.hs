{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Dto.UserUpdate where

import Data.Aeson (FromJSON)
import Data.Time (Day)
import GHC.Generics (Generic)

data UserUpdateDto = UserUpdateDto
  { name :: Maybe String,
    age :: Maybe Int,
    email :: Maybe String,
    registrationDate :: Maybe Day
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserUpdateDto
