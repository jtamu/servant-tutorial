{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Entity.User where

import DB (connectPG)
import Data.Aeson (FromJSON)
import Data.Functor.ProductIsomorphic ((|$|), (|*|))
import Data.Int (Int32)
import Data.Time (Day)
import Database.HDBC (IConnection (commit))
import Database.HDBC.Query.TH (defineTableFromDB, makeRelationalRecord)
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Relational (Delete, Flat, Insert, Pi, Record, Relation, Update, deleteNoPH, insert, query, relation, updateNoPH, value, wheres, (.=.), (<-#))
import Database.Relational.Documentation (runDelete, runInsert, runUpdate, (!))
import Database.Relational.Type (relationalQuery)
import Domain.User qualified as Du (User (User, age, email, name, registration_date, userId))
import GHC.Generics (Generic)
import Prelude hiding (id)

$(defineTableFromDB connectPG driverPostgreSQL "public" "users" [''Show, ''Generic])

getAllUsers :: IO [Du.User]
getAllUsers = do
  conn <- connectPG
  us <- runQuery' conn (relationalQuery users) ()
  return $ map toDomain us

toDomain :: Users -> Du.User
toDomain u = Du.User {Du.userId = fromIntegral $ id u, Du.name = name u, Du.email = email u, Du.age = fromIntegral $ age u, Du.registration_date = registrationDate u}

getUser :: Int32 -> IO (Maybe Du.User)
getUser userId = do
  conn <- connectPG
  us <- runQuery' conn (relationalQuery $ getUserQuery userId) ()
  case us of
    (u : _) -> return $ Just $ toDomain u
    _ -> return Nothing

getUserQuery :: Int32 -> Relation () Users
getUserQuery userId = relation $ do
  u <- query users
  wheres $ (u ! id') .=. value (fromIntegral userId)
  return u

data UserDto = UserDto
  { insertName :: String,
    insertAge :: Int32,
    insertEmail :: String,
    insert_registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserDto

$(makeRelationalRecord ''UserDto)

toInsertData :: Pi Users UserDto
toInsertData = UserDto |$| #name |*| #age |*| #email |*| #registrationDate

insertUser :: Insert UserDto
insertUser = insert toInsertData

domainToDto :: Du.User -> UserDto
domainToDto user = UserDto {insertName = Du.name user, insertAge = fromIntegral $ Du.age user, insertEmail = Du.email user, insert_registration_date = Du.registration_date user}

createUser :: UserDto -> IO ()
createUser insertData = do
  conn <- connectPG
  _ <- runInsert conn insertUser insertData
  commit conn
  return ()

deleteUser' :: Int32 -> Delete ()
deleteUser' userId = deleteNoPH $ \(user :: Record Flat Users) -> do
  wheres $ user ! id' .=. value userId

deleteUser :: Int32 -> IO ()
deleteUser userId = do
  conn <- connectPG
  _ <- runDelete conn (deleteUser' userId) ()
  commit conn
  return ()

updateUser' :: Int32 -> UserDto -> Update ()
updateUser' userId updateData = updateNoPH $ \(user :: Record Flat Users) -> do
  #name <-# value (insertName updateData)
  #age <-# value (insertAge updateData)
  #email <-# value (insertEmail updateData)
  #registrationDate <-# value (insert_registration_date updateData)
  wheres $ user ! id' .=. value userId

updateUser :: Du.User -> IO ()
updateUser updateData = do
  conn <- connectPG
  _ <- runUpdate conn (updateUser' (Du.userId updateData) (domainToDto updateData)) ()
  commit conn
  return ()

updateUser'' :: Du.User -> UserDto -> Du.User
updateUser'' user dto = user {Du.name = insertName dto, Du.age = insertAge dto, Du.email = insertEmail dto, Du.registration_date = insert_registration_date dto}
