module Main where

import Config.DB (doMigration, pgPool)
import Control.Monad.Logger (runStdoutLoggingT)
import Controller.Lib (API, server)
import Database.Persist.Sql (ConnectionPool)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Repository.Schema (migrateAll)
import Servant (Application, Proxy (Proxy), hoistServer, serve)

main :: IO ()
main = doMigration migrateAll >> runServer

userAPI :: Proxy API
userAPI = Proxy

app1 :: ConnectionPool -> Application
app1 pool = serve userAPI (hoistServer userAPI runStdoutLoggingT $ server pool)

runServer :: IO ()
runServer = withStdoutLogger $ \aplogger -> do
  pool <- pgPool
  runSettings (setPort 8080 $ setLogger aplogger defaultSettings) (app1 pool)
