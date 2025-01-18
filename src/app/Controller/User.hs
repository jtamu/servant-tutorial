{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Controller.User where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Int (Int64)
import Database.Persist.Postgresql (ConnectionPool)
import Domain.User (User)
import Dto.User (UserDto)
import Dto.UserUpdate (UserUpdateDto)
import Repository.User qualified as UserRepository (create, delete, get, getAll, update)
import Servant (Capture, DeleteNoContent, Get, Handler, HasServer (ServerT), JSON, NoContent (NoContent), PostCreated, PutNoContent, ReqBody, err404, (:<|>) ((:<|>)), (:>))
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
  _ <- liftIO $ UserRepository.create pool dto
  return NoContent

getUserAPIHandler :: ConnectionPool -> Int64 -> LoggingT Handler User
getUserAPIHandler pool reqId = do
  hitUsers <- liftIO $ UserRepository.get pool reqId
  maybe (throwError err404) return hitUsers

updateUserAPIHandler :: ConnectionPool -> Int64 -> UserUpdateDto -> LoggingT Handler NoContent
updateUserAPIHandler pool reqId updateData = do
  result <- liftIO $ runMaybeT $ UserService.update (UserRepository.get pool) (UserRepository.update pool) reqId updateData
  maybe (throwError err404) (const $ return NoContent) result

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
