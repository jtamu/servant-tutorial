module Main where

import Controller.Lib (API, server)
import DB (doMigration, pgPool)
import Database.Persist.Sql (ConnectionPool)
import Entity.User (migrateAll)
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
