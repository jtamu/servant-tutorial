{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Dto.UserUpdate where

import Config.Json (customOptions)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (parseJSON), genericParseJSON)
import Data.Time (Day)
import GHC.Generics (Generic)

data UserUpdateDto = UserUpdateDto
  { _name :: Maybe String,
    _age :: Maybe Int,
    _email :: Maybe String,
    _registrationDate :: Maybe Day
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserUpdateDto where
  parseJSON = genericParseJSON customOptions

$(makeLenses ''UserUpdateDto)
