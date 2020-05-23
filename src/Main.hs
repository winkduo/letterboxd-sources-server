module Main where

import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Server (app)

main :: IO ()
main = do
  putStrLn "Starting the server on port 8080..."
  Warp.run 8080 $ logStdoutDev $ app
