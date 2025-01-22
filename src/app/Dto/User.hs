{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Dto.User where

import Config.Json (customOptions)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (parseJSON), genericParseJSON)
import Data.Time (Day)
import GHC.Generics (Generic)
import Prelude hiding (id)

data UserDto = UserDto
  { _name :: Maybe String,
    _age :: Maybe Int,
    _email :: Maybe String,
    _registrationDate :: Maybe Day
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserDto where
  parseJSON = genericParseJSON customOptions

$(makeLenses ''UserDto)
