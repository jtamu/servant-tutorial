{-# LANGUAGE OverloadedRecordDot #-}

module Service.User where

import Control.Lens (modifying)
import Control.Monad.State (execState)
import Data.Int (Int64)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Validation (Validation (Failure, Success))
import Domain.User qualified as Domain
import Dto.UserUpdate (UserUpdateDto (_age, _email, _name, _registrationDate))
import Dto.ValidationError (ValidationError (ValidationError))
import Repository.User (UserRepository (get))
import Repository.User qualified as R (update)

update :: UserRepository -> Int64 -> UserUpdateDto -> IO (Validation ValidationError ())
update r uid dto = do
  maybeUser <- r.get uid
  case maybeUser of
    Nothing -> return $ Failure $ ValidationError $ M.fromList [("user", ["該当するユーザは存在しません"])]
    (Just user) ->
      mapM r.update (_update dto user)

_update :: UserUpdateDto -> Domain.RegisteredUser -> Validation ValidationError Domain.RegisteredUser
_update dto user =
  let vAge = case dto._age of
        (Just a) -> Domain.makeAge a
        Nothing -> Success user._age
   in case vAge of
        (Success age) ->
          Success $
            ( execState $ do
                modifying Domain.name (\n -> fromMaybe n dto._name)
                modifying Domain.age (const age)
                modifying Domain.email (\e -> fromMaybe e dto._email)
                modifying Domain.registrationDate (\r -> fromMaybe r dto._registrationDate)
            )
              user
        (Failure err) -> Failure err
