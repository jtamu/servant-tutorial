{-# LANGUAGE OverloadedRecordDot #-}

module Repository.User where

import Data.Int (Int64)
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

create :: ConnectionPool -> UserDto -> IO UserId
create pool dto = createUser pool (mapDtoToUser dto)

getAll :: ConnectionPool -> IO [Domain.User]
getAll pool = do
  users <- getAllUsers pool
  return $ map mapUserToDomain users

get :: ConnectionPool -> Int64 -> IO (Maybe Domain.User)
get pool userId = do
  user <- getUser pool (toSqlKey userId)
  return $ fmap mapUserToDomain user

update :: ConnectionPool -> Domain.User -> IO ()
update pool user = updateUser pool (mapUserToEntity user)

delete :: ConnectionPool -> Int64 -> IO ()
delete pool userId = deleteUser pool (toSqlKey userId)
