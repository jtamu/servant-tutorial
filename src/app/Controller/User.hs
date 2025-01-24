{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Controller.User where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (LoggingT, logInfo)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Aeson (encode)
import Data.Either.Validation (Validation (Failure, Success))
import Data.Int (Int64)
import Data.Text (append, pack)
import Domain.User (User)
import Dto.User (UserDto)
import Dto.UserUpdate (UserUpdateDto)
import Repository.User (UserRepository, create, delete, get, getAll)
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

getUsersAPIHandler :: UserRepository -> LoggingT Handler [User]
getUsersAPIHandler r = do
  liftIO r.getAll

createUserAPIHandler :: UserRepository -> UserDto -> LoggingT Handler NoContent
createUserAPIHandler r dto = do
  uid <- liftIO $ r.create dto
  case uid of
    (Success _) -> return NoContent
    (Failure e) -> throwError err400 {errBody = encode e}

getUserAPIHandler :: UserRepository -> Int64 -> LoggingT Handler User
getUserAPIHandler r reqId = do
  hitUsers <- liftIO $ r.get reqId
  case hitUsers of
    (Just user) -> return user
    Nothing -> do
      $logInfo $ "リクエストされたユーザは存在しませんでした userId: " `append` pack (show reqId)
      throwError err404

updateUserAPIHandler :: UserRepository -> Int64 -> UserUpdateDto -> LoggingT Handler NoContent
updateUserAPIHandler r reqId updateData = do
  result <- liftIO $ runMaybeT $ UserService.update r reqId updateData
  case result of
    (Just _) -> return NoContent
    Nothing -> do
      $logInfo $ "リクエストされたユーザは存在しませんでした userId: " `append` pack (show reqId)
      throwError err404

deleteUserAPIHandler :: UserRepository -> Int64 -> LoggingT Handler NoContent
deleteUserAPIHandler r reqId = do
  liftIO $ delete r reqId
  return NoContent

userServer :: UserRepository -> ServerT UserAPI (LoggingT Handler)
userServer r = do
  getUsersAPIHandler r
    :<|> createUserAPIHandler r
    :<|> \reqId ->
      getUserAPIHandler r reqId
        :<|> updateUserAPIHandler r reqId
        :<|> deleteUserAPIHandler r reqId
