{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Client.ChillInstitute where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import Data.Proxy (Proxy)
import Movie
import Network.HTTP.Client
  ( Manager
  , httpLbs
  , managerModifyRequest
  , parseRequest
  , requestHeaders
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Servant
import Servant.Client

type ChillInstituteAPI
   = "search" :> QueryParam' '[ Required] "keyword" String :> QueryParam' '[ Required] "indexer" String :> Header "referer" String :> Get '[ JSON] [Movie]

api :: Proxy ChillInstituteAPI
api = Proxy

getMovies :: String -> String -> Maybe String -> ClientM [Movie]
getMovies = client api

getClientEnv :: IO ClientEnv
getClientEnv = do
  manager <- liftIO $ newTlsManager
  baseUrl <- parseBaseUrl "https://chill.institute/api/v1"
  pure $ mkClientEnv manager baseUrl
