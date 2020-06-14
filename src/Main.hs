module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           Server                         ( runServer )
import qualified Data.Text                     as T
import           System.Environment             ( getEnv )

main :: IO ()

main = do
  putStrLn "Starting the server on port 8080..."

  -- As a rule of thumb, every environment variable should be read here in main.
  putIOAPIToken <- T.pack <$> getEnv "PUT_IO_API_TOKEN"

  runServer putIOAPIToken
