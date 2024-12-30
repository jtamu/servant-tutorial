{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Aeson (ToJSON)
import Data.Time.Calendar (Day, fromGregorian)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Capture, Get, Handler, JSON, Proxy (Proxy), QueryParam, Server, serve, (:<|>) ((:<|>)), (:>))

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

type API =
  "users" :> Get '[JSON] [User]
    :<|> "alice" :> Get '[JSON] User
    :<|> "bob" :> Get '[JSON] User
    :<|> "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage

data Position = Position
  { xCoord :: Int,
    yCoord :: Int
  }
  deriving (Generic)

instance ToJSON Position

positionAPIHandler :: Int -> Int -> Handler Position
positionAPIHandler x y = return $ Position x y

newtype HelloMessage = HelloMessage {msg :: String} deriving (Generic)

instance ToJSON HelloMessage

helloMessageAPIHandler :: Maybe String -> Handler HelloMessage
helloMessageAPIHandler (Just s) = return HelloMessage {msg = "hello, " ++ s}
helloMessageAPIHandler Nothing = return HelloMessage {msg = "hello, anonymous"}

server :: Server API
server =
  return users
    :<|> return alice
    :<|> return bob
    :<|> positionAPIHandler
    :<|> helloMessageAPIHandler

userAPI :: Proxy API
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server

runServer :: IO ()
runServer = run 8080 app1
