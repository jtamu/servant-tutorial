{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module User where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int32)
import Domain.User (User)
import Entity.User (deleteUser, getAllUsers, getUser, updateUser)
import Servant (Capture, DeleteNoContent, Get, Handler, JSON, NoContent (NoContent), PostCreated, PutNoContent, ReqBody, Server, err404, (:<|>) ((:<|>)), (:>))

type UserAPI =
  Get '[JSON] [User]
    :<|> ReqBody '[JSON] User :> PostCreated '[JSON] NoContent
    :<|> Capture "userId" Int32
      :> ( Get '[JSON] User
             :<|> ReqBody '[JSON] User :> PutNoContent
             :<|> DeleteNoContent
         )

getUsersAPIHandler :: Handler [User]
getUsersAPIHandler = liftIO getAllUsers

createUserAPIHandler :: User -> Handler NoContent
createUserAPIHandler _ = return NoContent

getUserAPIHandler :: Int32 -> Handler User
getUserAPIHandler reqId = do
  hitUsers <- liftIO $ getUser reqId
  case hitUsers of
    Just x -> return x
    Nothing -> throwError err404

updateUserAPIHandler :: Int32 -> User -> Handler NoContent
updateUserAPIHandler reqId user = do
  -- let hitUsers = ([user | user <- users, userId user == reqId])
  -- case hitUsers of
  --   (_ : _) -> return NoContent
  --   _ -> throwError err404
  liftIO $ updateUser reqId user
  return NoContent

deleteUserAPIHandler :: Int32 -> Handler NoContent
deleteUserAPIHandler reqId = do
  -- let hitUsers = ([user | user <- users, userId user == reqId])
  -- case hitUsers of
  --   (_ : _) -> return NoContent
  --   _ -> throwError err404
  liftIO $ deleteUser reqId
  return NoContent

userServer :: Server UserAPI
userServer =
  getUsersAPIHandler
    :<|> createUserAPIHandler
    :<|> \reqId ->
      getUserAPIHandler reqId
        :<|> updateUserAPIHandler reqId
        :<|> deleteUserAPIHandler reqId
