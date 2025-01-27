{-# LANGUAGE OverloadedRecordDot #-}

module Service.User where

import Control.Lens (modifying)
import Control.Monad.State (execState)
import Data.Int (Int64)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Validation (Validation (Failure, Success), bindValidation)
import Domain.User qualified as Domain
import Dto.UserUpdate (UserUpdateDto (_age, _email, _name, _registrationDate))
import Dto.ValidationError (ValidationError (ValidationError))
import Repository.User (UserRepository (get))
import Repository.User qualified as R (update)

update :: UserRepository -> Int64 -> UserUpdateDto -> IO (Validation ValidationError ())
update r uid dto = do
  maybeUser <- r.get uid
  let vuser = liftMaybe (ValidationError $ M.fromList [("user", ["該当するユーザは存在しません"])]) maybeUser
  mapM r.update $ vuser `bindValidation` _update dto

_update :: UserUpdateDto -> Domain.RegisteredUser -> Validation ValidationError Domain.RegisteredUser
_update dto user =
  updateUser <$> vAge
  where
    vAge = maybe (Success user._age) Domain.makeAge dto._age
    updateUser age =
      ( execState $ do
          modifying Domain.name (\n -> fromMaybe n dto._name)
          modifying Domain.age (const age)
          modifying Domain.email (\e -> fromMaybe e dto._email)
          modifying Domain.registrationDate (\r -> fromMaybe r dto._registrationDate)
      )
        user

liftMaybe :: b -> Maybe a -> Validation b a
liftMaybe b Nothing = Failure b
liftMaybe _ (Just x) = Success x
