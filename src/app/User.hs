{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module User where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int64)
import Database.Persist.Postgresql (ConnectionPool)
import Domain.User (User)
import Dto.User (UserDto)
import Dto.UserUpdate (UserUpdateDto)
import Entity.User (createUser', deleteUser', getAllUsers', getUser', updateUser', updateUser'')
import Servant (Capture, DeleteNoContent, Get, Handler, JSON, NoContent (NoContent), PostCreated, PutNoContent, ReqBody, Server, err404, (:<|>) ((:<|>)), (:>))

type UserAPI =
  Get '[JSON] [User]
    :<|> ReqBody '[JSON] UserDto :> PostCreated '[JSON] NoContent
    :<|> Capture "userId" Int64
      :> ( Get '[JSON] User
             :<|> ReqBody '[JSON] UserUpdateDto :> PutNoContent
             :<|> DeleteNoContent
         )

getUsersAPIHandler :: ConnectionPool -> Handler [User]
getUsersAPIHandler pool = do
  liftIO $ getAllUsers' pool

createUserAPIHandler :: ConnectionPool -> UserDto -> Handler NoContent
createUserAPIHandler pool dto = do
  _ <- liftIO $ createUser' pool dto
  return NoContent

getUserAPIHandler :: ConnectionPool -> Int64 -> Handler User
getUserAPIHandler pool reqId = do
  hitUsers <- liftIO $ getUser' pool reqId
  case hitUsers of
    Just x -> return x
    Nothing -> throwError err404

updateUserAPIHandler :: ConnectionPool -> Int64 -> UserUpdateDto -> Handler NoContent
updateUserAPIHandler pool reqId updateData = do
  maybeUser <- liftIO $ getUser' pool reqId
  case maybeUser of
    Just user ->
      liftIO $ updateUser' pool (updateUser'' updateData user)
    Nothing ->
      throwError err404
  return NoContent

deleteUserAPIHandler :: ConnectionPool -> Int64 -> Handler NoContent
deleteUserAPIHandler pool reqId = do
  -- let hitUsers = ([user | user <- users, userId user == reqId])
  -- case hitUsers of
  --   (_ : _) -> return NoContent
  --   _ -> throwError err404
  liftIO $ deleteUser' pool reqId
  return NoContent

userServer :: ConnectionPool -> Server UserAPI
userServer pool =
  getUsersAPIHandler pool
    :<|> createUserAPIHandler pool
    :<|> \reqId ->
      getUserAPIHandler pool reqId
        :<|> updateUserAPIHandler pool reqId
        :<|> deleteUserAPIHandler pool reqId
