{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Entity.User where

import DB (connectPG)
import Data.Functor.ProductIsomorphic ((|$|), (|*|))
import Data.Int (Int32)
import Data.Time (Day)
import Database.HDBC.Query.TH (defineTableFromDB, makeRelationalRecord)
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Relational (Insert, Pi, Relation, insert, query, relation, value, wheres, (.=.))
import Database.Relational.Documentation (runInsert, (!))
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

getUser :: Int -> IO (Maybe Du.User)
getUser userId = do
  conn <- connectPG
  us <- runQuery' conn (relationalQuery $ getUserQuery userId) ()
  case us of
    (u : _) -> return $ Just $ toDomain u
    _ -> return Nothing

getUserQuery :: Int -> Relation () Users
getUserQuery userId = relation $ do
  u <- query users
  wheres $ (u ! id') .=. value (fromIntegral userId)
  return u

data InsertUser = InsertUser
  { insertName :: String,
    insertAge :: Int32,
    insertEmail :: String,
    insert_registration_date :: Day
  }
  deriving (Eq, Show, Generic)

$(makeRelationalRecord ''InsertUser)

toInsertData :: Pi Users InsertUser
toInsertData = InsertUser |$| #name |*| #age |*| #email |*| #registrationDate

insertUser :: Insert InsertUser
insertUser = insert toInsertData

createUser :: InsertUser -> IO ()
createUser insertData = do
  conn <- connectPG
  _ <- runInsert conn insertUser insertData
  return ()
