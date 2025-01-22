{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Controller.User where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (LoggingT, logInfo)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Aeson (encode)
import Data.Int (Int64)
import Data.Text (append, pack)
import Database.Persist.Postgresql (ConnectionPool)
import Domain.User (User)
import Dto.User (UserDto)
import Dto.UserUpdate (UserUpdateDto)
import Repository.User qualified as UserRepository (create, delete, get, getAll, update)
import Servant (Capture, DeleteNoContent, Get, Handler, HasServer (ServerT), JSON, NoContent (NoContent), PostCreated, PutNoContent, ReqBody, ServerError (errBody), err400, err404, (:<|>) ((:<|>)), (:>))
import Service.User qualified as UserService (update)

type UserAPI =
  Get '[JSON] [User]
    :<|> ReqBody '[JSON] UserDto :> PostCreated '[JSON] NoContent
    :<|> Capture "userId" Int64
      :> ( Get '[JSON] User
             :<|> ReqBody '[JSON] UserUpdateDto :> PutNoContent
             :<|> DeleteNoContent
         )

getUsersAPIHandler :: ConnectionPool -> LoggingT Handler [User]
getUsersAPIHandler pool = do
  liftIO $ UserRepository.getAll pool

createUserAPIHandler :: ConnectionPool -> UserDto -> LoggingT Handler NoContent
createUserAPIHandler pool dto = do
  uid <- liftIO $ UserRepository.create pool dto
  case uid of
    (Right _) -> return NoContent
    (Left e) -> throwError err400 {errBody = encode e}

getUserAPIHandler :: ConnectionPool -> Int64 -> LoggingT Handler User
getUserAPIHandler pool reqId = do
  hitUsers <- liftIO $ UserRepository.get pool reqId
  case hitUsers of
    (Just user) -> return user
    Nothing -> do
      $logInfo $ "リクエストされたユーザは存在しませんでした userId: " `append` pack (show reqId)
      throwError err404

updateUserAPIHandler :: ConnectionPool -> Int64 -> UserUpdateDto -> LoggingT Handler NoContent
updateUserAPIHandler pool reqId updateData = do
  result <- liftIO $ runMaybeT $ UserService.update (UserRepository.get pool) (UserRepository.update pool) reqId updateData
  case result of
    (Just _) -> return NoContent
    Nothing -> do
      $logInfo $ "リクエストされたユーザは存在しませんでした userId: " `append` pack (show reqId)
      throwError err404

deleteUserAPIHandler :: ConnectionPool -> Int64 -> LoggingT Handler NoContent
deleteUserAPIHandler pool reqId = do
  liftIO $ UserRepository.delete pool reqId
  return NoContent

userServer :: ConnectionPool -> ServerT UserAPI (LoggingT Handler)
userServer pool = do
  getUsersAPIHandler pool
    :<|> createUserAPIHandler pool
    :<|> \reqId ->
      getUserAPIHandler pool reqId
        :<|> updateUserAPIHandler pool reqId
        :<|> deleteUserAPIHandler pool reqId
