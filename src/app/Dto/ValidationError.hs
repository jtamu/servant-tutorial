module Dto.ValidationError where

import Data.Aeson (ToJSON)
import Data.Map qualified as M
import GHC.Generics (Generic)

newtype ValidationError = ValidationError (M.Map String [String]) deriving (Show, Generic)

instance ToJSON ValidationError

instance Semigroup ValidationError where
  (ValidationError m1) <> (ValidationError m2) = ValidationError $ m1 <> m2
