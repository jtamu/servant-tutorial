{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module User where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Calendar (Day, fromGregorian)
import GHC.Generics (Generic)
import Servant (Capture, DeleteNoContent, Get, Handler, JSON, NoContent (NoContent), PostCreated, PutNoContent, ReqBody, Server, err404, (:<|>) ((:<|>)), (:>))

data User = User
  { userId :: Int,
    name :: String,
    age :: Int,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User

alice :: User
alice = User {userId = 1, name = "Hoge", age = 25, email = "alice@example.com", registration_date = fromGregorian 2020 1 1}

bob :: User
bob = User {userId = 2, name = "Bob", age = 30, email = "bob@example.com", registration_date = fromGregorian 2021 1 1}

users :: [User]
users = [alice, bob]

type UserAPI =
  Get '[JSON] [User]
    :<|> ReqBody '[JSON] User :> PostCreated '[JSON] NoContent
    :<|> Capture "userId" Int
      :> ( Get '[JSON] User
             :<|> ReqBody '[JSON] User :> PutNoContent
             :<|> DeleteNoContent
         )

createUserAPIHandler :: User -> Handler NoContent
createUserAPIHandler _ = return NoContent

getUserAPIHandler :: Int -> Handler User
getUserAPIHandler reqId = do
  let hitUsers = ([user | user <- users, userId user == reqId])
  case hitUsers of
    (x : _) -> return x
    _ -> throwError err404

updateUserAPIHandler :: Int -> User -> Handler NoContent
updateUserAPIHandler reqId _ = do
  let hitUsers = ([user | user <- users, userId user == reqId])
  case hitUsers of
    (_ : _) -> return NoContent
    _ -> throwError err404

deleteUserAPIHandler :: Int -> Handler NoContent
deleteUserAPIHandler reqId = do
  let hitUsers = ([user | user <- users, userId user == reqId])
  case hitUsers of
    (_ : _) -> return NoContent
    _ -> throwError err404

userServer :: Server UserAPI
userServer =
  return users
    :<|> createUserAPIHandler
    :<|> \reqId ->
      getUserAPIHandler reqId
        :<|> updateUserAPIHandler reqId
        :<|> deleteUserAPIHandler reqId
