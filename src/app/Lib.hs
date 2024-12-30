{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Aeson (ToJSON)
import Data.Time.Calendar (Day, fromGregorian)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Get, JSON, Proxy (Proxy), Server, serve, (:<|>) ((:<|>)), (:>))

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

alice :: User
alice = User {name = "Hoge", age = 25, email = "alice@example.com", registration_date = fromGregorian 2020 1 1}

bob :: User
bob = User {name = "Bob", age = 30, email = "bob@example.com", registration_date = fromGregorian 2021 1 1}

users :: [User]
users = [alice, bob]

type UserAPI1 =
  "users" :> Get '[JSON] [User]
    :<|> "alice" :> Get '[JSON] User
    :<|> "bob" :> Get '[JSON] User

server :: Server UserAPI1
server =
  return users
    :<|> return alice
    :<|> return bob

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server

runServer :: IO ()
runServer = run 8080 app1
