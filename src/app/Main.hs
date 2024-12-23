{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

app :: Application
app _ respond = do
  putStrLn "I've done some IO here"
  respond $
    responseLBS
      status200
      [("Content-Type", "text/plain")]
      "Hello, World!"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Starting server at http://localhost:8080 ..."
  run 8080 app
