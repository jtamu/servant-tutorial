{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Aeson (ToJSON)
import Data.Time.Calendar (Day, fromGregorian)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Get, JSON, Proxy (Proxy), Server, serve, (:>))

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

users :: [User]
users =
  [ User {name = "Alice", age = 25, email = "alice@example.com", registration_date = fromGregorian 2020 1 1},
    User {name = "Bob", age = 30, email = "bob@example.com", registration_date = fromGregorian 2021 1 1}
  ]

type UserAPI1 = "users" :> Get '[JSON] [User]

server :: Server UserAPI1
server = return users

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server

runServer :: IO ()
runServer = run 8080 app1
