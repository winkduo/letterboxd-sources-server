module Main where

import           AppEnv
import           Control.Monad.IO.Class         ( liftIO )
import           Network.Wai.Handler.Warp      as Warp
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev )
import           Server                         ( app )

main :: IO ()

main = do
  appEnv <- liftIO readAppEnv
  putStrLn "Starting the server on port 8080..."
  Warp.run 8080 $ logStdoutDev $ app appEnv
