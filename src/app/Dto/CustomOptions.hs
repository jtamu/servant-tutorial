module Dto.CustomOptions where

import Data.Aeson (Options (fieldLabelModifier), defaultOptions)

customOptions :: Options
customOptions =
  defaultOptions
    { fieldLabelModifier = dropWhile (== '_')
    }
