{-# LANGUAGE OverloadedRecordDot #-}

module Service.User where

import Control.Lens (modifying)
import Control.Monad.State (MonadTrans (lift), execState)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Domain.User qualified as Domain
import Dto.UserUpdate (UserUpdateDto (_age, _email, _name, _registrationDate))
import Repository.User (IUserRepository (get))
import Repository.User qualified as R (update)

update :: (IUserRepository r) => r -> Int64 -> UserUpdateDto -> MaybeT IO ()
update r uid dto = do
  user <- MaybeT $ get r uid
  lift $ R.update r (_update dto user)

_update :: UserUpdateDto -> Domain.User -> Domain.User
_update dto = execState $ do
  modifying Domain.name (\n -> fromMaybe n dto._name)
  modifying Domain.age (\a -> fromMaybe a dto._age)
  modifying Domain.email (\e -> fromMaybe e dto._email)
  modifying Domain.registrationDate (\r -> fromMaybe r dto._registrationDate)
