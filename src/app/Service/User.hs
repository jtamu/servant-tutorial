{-# LANGUAGE OverloadedRecordDot #-}

module Service.User where

import Control.Lens (modifying)
import Control.Monad.State (MonadTrans (lift), execState)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Domain.User qualified as Domain
import Dto.UserUpdate (UserUpdateDto (_age, _email, _name, _registrationDate))

update :: (Int64 -> IO (Maybe Domain.User)) -> (Domain.User -> IO ()) -> Int64 -> UserUpdateDto -> MaybeT IO ()
update getfn updatefn uid dto = do
  user <- MaybeT $ getfn uid
  lift $ updatefn $ _update dto user

_update :: UserUpdateDto -> Domain.User -> Domain.User
_update dto = execState $ do
  modifying Domain.name (\n -> fromMaybe n dto._name)
  modifying Domain.age (\a -> fromMaybe a dto._age)
  modifying Domain.email (\e -> fromMaybe e dto._email)
  modifying Domain.registrationDate (\r -> fromMaybe r dto._registrationDate)
