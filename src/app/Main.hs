module Main where

import DB (doMigration)
import Entity.User (migrateAll)
import Lib (runServer)

main :: IO ()
main = doMigration migrateAll >> runServer
