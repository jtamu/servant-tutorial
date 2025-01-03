module DB where

import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)

connectPG :: IO Connection
connectPG =
  connectPostgreSQL
    "host=db dbname=test user=user password=password sslmode=disable"
