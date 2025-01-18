module Config.DB where

import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Yaml.Config
  ( loadYamlSettings,
    useEnv,
  )
import Database.Persist.Postgresql
  ( PostgresConf (pgConnStr, pgPoolSize),
    createPostgresqlPool,
    withPostgresqlConn,
  )
import Database.Persist.Sql
  ( ConnectionPool,
    Migration,
    runMigration,
  )

pgConf :: IO PostgresConf
pgConf = loadYamlSettings ["config/db.yml"] [] useEnv

pgPool :: IO ConnectionPool
pgPool = do
  conf <- pgConf
  runStdoutLoggingT $ createPostgresqlPool (pgConnStr conf) (pgPoolSize conf)

doMigration :: Migration -> IO ()
doMigration action = do
  conf <- pgConf
  runStdoutLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runReaderT $ runMigration action
