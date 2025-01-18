{-# LANGUAGE OverloadedRecordDot #-}

module Service.User where

import Control.Lens (modifying)
import Control.Monad.State (execState)
import Data.Maybe (fromMaybe)
import Domain.User qualified as Domain
import Dto.UserUpdate (UserUpdateDto (_age, _email, _name, _registrationDate))

update :: UserUpdateDto -> Domain.User -> Domain.User
update dto = execState $ do
  modifying Domain.name (\n -> fromMaybe n dto._name)
  modifying Domain.age (\a -> fromMaybe a dto._age)
  modifying Domain.email (\e -> fromMaybe e dto._email)
  modifying Domain.registrationDate (\r -> fromMaybe r dto._registrationDate)
