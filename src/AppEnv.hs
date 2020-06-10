{-# LANGUAGE RecordWildCards #-}

module AppEnv where

import qualified Data.Text as T
import System.Environment (getEnv)

data AppEnv =
  AppEnv
    { putIOAPIToken :: T.Text
    }
  deriving (Show, Eq)

readAppEnv :: IO AppEnv
readAppEnv = do
  putIOAPIToken <- T.pack <$> getEnv "PUT_IO_API_TOKEN"
  pure AppEnv {..}
