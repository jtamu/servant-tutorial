{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Capture, Get, Handler, JSON, Post, Proxy (Proxy), QueryParam, ReqBody, Server, serve, (:<|>) ((:<|>)), (:>))
import User (UserAPI, userServer)

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

type API =
  "users" :> UserAPI
    :<|> "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
    :<|> "myfile" :> Get '[JSON] FileContent

server :: Server API
server =
  userServer
    :<|> positionAPIHandler
    :<|> helloMessageAPIHandler
    :<|> marketingAPIHandler
    :<|> fileContentHandler

userAPI :: Proxy API
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server

runServer :: IO ()
runServer = run 8080 app1
