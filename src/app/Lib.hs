{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate)
import Data.Time.Calendar (Day, fromGregorian)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Capture, DeleteNoContent, Get, Handler, JSON, NoContent (NoContent), Post, PostCreated, Proxy (Proxy), PutNoContent, QueryParam, ReqBody, Server, err404, serve, (:<|>) ((:<|>)), (:>))

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

type API =
  "users" :> UserAPI
    :<|> "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
    :<|> "myfile" :> Get '[JSON] FileContent

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

data ClientInfo = ClientInfo
  { clientName :: String,
    clientEmail :: String,
    clientAge :: Int,
    clientInterestedIn :: [String]
  }
  deriving (Generic)

instance FromJSON ClientInfo

data Email = Email
  { from :: String,
    to :: String,
    subject :: String,
    body :: String
  }
  deriving (Generic)

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where
    from' = "great@company.com"
    to' = clientEmail c
    subject' = "Hey " ++ clientName c ++ ", we miss you!"
    body' =
      "Hi "
        ++ clientName c
        ++ ",\n\n"
        ++ "Since you've recently turned "
        ++ show (clientAge c)
        ++ ", have you checked out our latest "
        ++ intercalate ", " (clientInterestedIn c)
        ++ " products? Give us a visit!"

marketingAPIHandler :: ClientInfo -> Handler Email
marketingAPIHandler c = return $ emailForClient c

newtype FileContent = FileContent {content :: String} deriving (Generic)

instance ToJSON FileContent

fileContentHandler :: Handler FileContent
fileContentHandler = do
  content <- liftIO $ readFile "myfile.txt"
  return $ FileContent content

server :: Server API
server =
  userServer
    :<|> positionAPIHandler
    :<|> helloMessageAPIHandler
    :<|> marketingAPIHandler
    :<|> fileContentHandler

userServer :: Server UserAPI
userServer =
  return users
    :<|> createUserAPIHandler
    :<|> \reqId ->
      getUserAPIHandler reqId
        :<|> updateUserAPIHandler reqId
        :<|> deleteUserAPIHandler reqId

userAPI :: Proxy API
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server

runServer :: IO ()
runServer = run 8080 app1
