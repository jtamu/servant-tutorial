{-# LANGUAGE OverloadedRecordDot #-}

module Repository.User where

import Control.Lens (modifying)
import Control.Monad.State (execState)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Database.Persist
  ( Entity (Entity),
    PersistStoreWrite (delete, insert, replace),
    getEntity,
    selectList,
  )
import Database.Persist.Sql
  ( ConnectionPool,
    fromSqlKey,
    runSqlPool,
    toSqlKey,
  )
import Domain.User qualified as Domain
import Dto.User (UserDto (_age, _email, _name, _registrationDate))
import Dto.UserUpdate (UserUpdateDto (_age, _email, _name, _registrationDate))
import Entity.User (User (User, _userAge, _userEmail, _userName, _userRegistrationDate), UserId)

createUser :: ConnectionPool -> User -> IO UserId
createUser pool inserts = runSqlPool (insert inserts) pool

getAllUsers :: ConnectionPool -> IO [Entity User]
getAllUsers = runSqlPool $ selectList [] []

getUser :: ConnectionPool -> UserId -> IO (Maybe (Entity User))
getUser pool userId = runSqlPool (getEntity userId) pool

updateUser :: ConnectionPool -> Entity User -> IO ()
updateUser pool (Entity userId user) = runSqlPool (replace userId user) pool

deleteUser :: ConnectionPool -> UserId -> IO ()
deleteUser pool userId = runSqlPool (delete userId) pool

mapUserToDomain :: Entity User -> Domain.User
mapUserToDomain (Entity userId user) =
  Domain.User
    { Domain._userId = fromSqlKey userId,
      Domain._name = user._userName,
      Domain._age = user._userAge,
      Domain._email = user._userEmail,
      Domain._registrationDate = user._userRegistrationDate
    }

mapUserToEntity :: Domain.User -> Entity User
mapUserToEntity user =
  let record =
        User
          { _userName = user._name,
            _userAge = user._age,
            _userEmail = user._email,
            _userRegistrationDate = user._registrationDate
          }
   in Entity (toSqlKey user._userId) record

mapDtoToUser :: UserDto -> User
mapDtoToUser user =
  User
    { _userName = user._name,
      _userAge = user._age,
      _userEmail = user._email,
      _userRegistrationDate = user._registrationDate
    }

createUser' :: ConnectionPool -> UserDto -> IO UserId
createUser' pool dto = createUser pool (mapDtoToUser dto)

getAllUsers' :: ConnectionPool -> IO [Domain.User]
getAllUsers' pool = do
  users <- getAllUsers pool
  return $ map mapUserToDomain users

getUser' :: ConnectionPool -> Int64 -> IO (Maybe Domain.User)
getUser' pool userId = do
  user <- getUser pool (toSqlKey userId)
  return $ fmap mapUserToDomain user

updateUser' :: ConnectionPool -> Domain.User -> IO ()
updateUser' pool user = updateUser pool (mapUserToEntity user)

updateUser'' :: UserUpdateDto -> Domain.User -> Domain.User
updateUser'' dto = execState $ do
  modifying Domain.name (\n -> fromMaybe n dto._name)
  modifying Domain.age (\a -> fromMaybe a dto._age)
  modifying Domain.email (\e -> fromMaybe e dto._email)
  modifying Domain.registrationDate (\r -> fromMaybe r dto._registrationDate)

deleteUser' :: ConnectionPool -> Int64 -> IO ()
deleteUser' pool userId = deleteUser pool (toSqlKey userId)
