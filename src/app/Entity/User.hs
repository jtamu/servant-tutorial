{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Entity.User where

import DB (connectPG)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Relational (Relation, query, relation, value, wheres, (.=.))
import Database.Relational.Documentation ((!))
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
