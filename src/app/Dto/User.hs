{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Dto.User where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (parseJSON), genericParseJSON)
import Data.Time (Day)
import Dto.CustomOptions (customOptions)
import GHC.Generics (Generic)
import Prelude hiding (id)

data UserDto = UserDto
  { _name :: String,
    _age :: Int,
    _email :: String,
    _registrationDate :: Day
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserDto where
  parseJSON = genericParseJSON customOptions

$(makeLenses ''UserDto)
