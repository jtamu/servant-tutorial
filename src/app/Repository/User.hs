{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Repository.User where

import Data.Int (Int64)
import Data.Map qualified as M (fromList)
import Data.Validation (Validation (Failure, Success), orElse)
import Database.Persist
  ( Entity (Entity),
    getEntity,
    selectList,
  )
import Database.Persist qualified as P (PersistStoreWrite (delete, insert, replace))
import Database.Persist.Sql
  ( ConnectionPool,
    fromSqlKey,
    runSqlPool,
    toSqlKey,
  )
import Domain.User qualified as Domain
import Dto.User (UserDto (_age, _email, _name, _registrationDate))
import Dto.ValidationError (ValidationError (ValidationError))
import Repository.Schema (User (User, _userAge, _userEmail, _userName, _userRegistrationDate), UserId)

createUser :: ConnectionPool -> User -> IO UserId
createUser pool inserts = runSqlPool (P.insert inserts) pool

getAllUsers :: ConnectionPool -> IO [Entity User]
getAllUsers = runSqlPool $ selectList [] []

getUser :: ConnectionPool -> UserId -> IO (Maybe (Entity User))
getUser pool userId = runSqlPool (getEntity userId) pool

updateUser :: ConnectionPool -> Entity User -> IO ()
updateUser pool (Entity userId user) = runSqlPool (P.replace userId user) pool

deleteUser :: ConnectionPool -> UserId -> IO ()
deleteUser pool userId = runSqlPool (P.delete userId) pool

mapUserToDomain :: Entity User -> Domain.User
mapUserToDomain (Entity userId user) =
  Domain.User
    { Domain._userId = fromSqlKey userId,
      Domain._name = user._userName,
      Domain._age = Domain.makeAge user._userAge `orElse` Domain.defaultAge,
      Domain._email = user._userEmail,
      Domain._registrationDate = user._userRegistrationDate
    }

mapUserToEntity :: Domain.User -> Entity User
mapUserToEntity user =
  let (Domain.Age age) = user._age
      record =
        User
          { _userName = user._name,
            _userAge = age,
            _userEmail = user._email,
            _userRegistrationDate = user._registrationDate
          }
   in Entity (toSqlKey user._userId) record

mapDtoToUser :: UserDto -> Validation ValidationError User
mapDtoToUser user = User <$> validateExistence "name" user._name <*> validateExistence "age" user._age <*> validateExistence "email" user._email <*> validateExistence "registration_date" user._registrationDate

validateExistence :: String -> Maybe a -> Validation ValidationError a
validateExistence _ (Just x) = Success x
validateExistence colName Nothing = Failure $ ValidationError $ M.fromList [(colName, ["必須の項目です"])]

data UserRepository = UserRepository
  { create :: UserDto -> IO (Validation ValidationError UserId),
    getAll :: IO [Domain.User],
    get :: Int64 -> IO (Maybe Domain.User),
    update :: Domain.User -> IO (),
    delete :: Int64 -> IO ()
  }

userRepository :: ConnectionPool -> UserRepository
userRepository pool =
  UserRepository
    { create = mapM (createUser pool) . mapDtoToUser,
      getAll = do
        users <- getAllUsers pool
        return $ map mapUserToDomain users,
      get = \userId -> do
        maybeUser <- getUser pool (toSqlKey userId)
        return $ fmap mapUserToDomain maybeUser,
      update = updateUser pool . mapUserToEntity,
      delete = deleteUser pool . toSqlKey
    }
