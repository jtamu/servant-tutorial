module Dto.ErrorResponse where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

newtype ErrorResponse = ErrorResponse {message :: String} deriving (Show, Generic)

instance ToJSON ErrorResponse
