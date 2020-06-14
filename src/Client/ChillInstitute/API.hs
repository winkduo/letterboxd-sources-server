{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.ChillInstitute.API where

import Client.ChillInstitute.Types
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import Data.Proxy (Proxy)
import qualified Data.Text as T
import Network.HTTP.Client
  ( Manager
  , httpLbs
  , managerModifyRequest
  , parseRequest
  , requestHeaders
  )
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Servant
import Servant.Client

type ChillInstituteAPI
   = "search" :> QueryParam' '[ Required] "keyword" T.Text :> QueryParam' '[ Required] "indexer" T.Text :> Get '[ JSON] [Movie] :<|> "indexers" :> Get '[ JSON] [Indexer]

api :: Proxy ChillInstituteAPI
api = Proxy

getMovies :: T.Text -> T.Text -> ClientM [Movie]
getIndexers :: ClientM [Indexer]
getMovies :<|> getIndexers = client api

getClientEnv :: IO ClientEnv
getClientEnv = do
  manager <-
    liftIO $
    newTlsManagerWith
      tlsManagerSettings
        { managerModifyRequest =
            \r -> do
              print r
              pure
                r {requestHeaders = [("referer", "https://chill.institute/")]}
        }
  baseUrl <- parseBaseUrl "https://chill.institute/api/v1"
  pure $ mkClientEnv manager baseUrl
