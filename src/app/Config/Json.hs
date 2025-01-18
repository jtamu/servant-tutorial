module Config.Json where

import Data.Aeson (Options (fieldLabelModifier), defaultOptions)

customOptions :: Options
customOptions =
  defaultOptions
    { fieldLabelModifier = dropWhile (== '_')
    }
