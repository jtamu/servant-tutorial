{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.User where

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Calendar (Day)
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
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Domain.User qualified as DU
import Dto.User qualified as DTO
import Dto.UserUpdate qualified as UUDTO
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    name String
    age Int
    email String
    registrationDate Day
    deriving Show Generic
Country
   code Text
   name Text
   deriving Show Generic
|]

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

mapUserToDomain :: Entity User -> DU.User
mapUserToDomain (Entity userId user) =
  DU.User
    { DU.userId = fromSqlKey userId,
      DU.name = userName user,
      DU.age = userAge user,
      DU.email = userEmail user,
      DU.registrationDate = userRegistrationDate user
    }

mapUserToEntity :: DU.User -> Entity User
mapUserToEntity user =
  let record =
        User
          { userName = DU.name user,
            userAge = DU.age user,
            userEmail = DU.email user,
            userRegistrationDate = DU.registrationDate user
          }
   in Entity (toSqlKey (DU.userId user)) record

mapDtoToUser :: DTO.UserDto -> User
mapDtoToUser user =
  User
    { userName = DTO.name user,
      userAge = DTO.age user,
      userEmail = DTO.email user,
      userRegistrationDate = DTO.registrationDate user
    }

createUser' :: ConnectionPool -> DTO.UserDto -> IO UserId
createUser' pool dto = createUser pool (mapDtoToUser dto)

getAllUsers' :: ConnectionPool -> IO [DU.User]
getAllUsers' pool = do
  users <- getAllUsers pool
  return $ map mapUserToDomain users

getUser' :: ConnectionPool -> Int64 -> IO (Maybe DU.User)
getUser' pool userId = do
  user <- getUser pool (toSqlKey userId)
  return $ fmap mapUserToDomain user

updateUser' :: ConnectionPool -> DU.User -> IO ()
updateUser' pool user = updateUser pool (mapUserToEntity user)

updateUser'' :: DU.User -> UUDTO.UserUpdateDto -> DU.User
updateUser'' user dto =
  user
    { DU.name = fromMaybe (DU.name user) (UUDTO.name dto),
      DU.age = fromMaybe (DU.age user) (UUDTO.age dto),
      DU.email = fromMaybe (DU.email user) (UUDTO.email dto),
      DU.registrationDate = fromMaybe (DU.registrationDate user) (UUDTO.registrationDate dto)
    }

deleteUser' :: ConnectionPool -> Int64 -> IO ()
deleteUser' pool userId = deleteUser pool (toSqlKey userId)
