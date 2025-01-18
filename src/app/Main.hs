module Main where

import Controller.Lib (API, server)
import Config.DB (doMigration, pgPool)
import Database.Persist.Sql (ConnectionPool)
import Repository.Schema (migrateAll)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Proxy (Proxy), serve)

main :: IO ()
main = doMigration migrateAll >> runServer

userAPI :: Proxy API
userAPI = Proxy

app1 :: ConnectionPool -> Application
app1 pool = serve userAPI (server pool)

runServer :: IO ()
runServer = do
  pool <- pgPool
  run 8080 (app1 pool)
