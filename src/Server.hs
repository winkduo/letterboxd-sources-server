{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import           AppEnv
import qualified Client.ChillInstitute.API     as CIAPI
import           Client.ChillInstitute.Types
import           Client.PutIO.Types
import qualified Client.PutIO.API              as PutIOAPI
import           Control.Monad.Except           ( liftEither )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.TH                    as JSON
import Client.Util
import Data.Char
import           Data.Bifunctor                 ( first )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS8
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LBS8
import           Data.Functor                   ( ($>) )
import           Data.Proxy                     ( Proxy )
import qualified Data.Text                     as T
import           Data.Traversable               ( for )
import           Servant
import           Servant.Client                 ( ClientEnv
                                                , ClientM
                                                , runClientM
                                                )
import           Servant.Server
import           System.Environment             ( getEnv )
import GHC.Generics
import Network.Wai.Middleware.Cors
import Network.Wai
import Data.Aeson.Casing

data DownloadMovieReq = DownloadMovieReq { _dmrMagnetUrl :: T.Text } deriving (Show, Eq, Generic)

type API
     = "find_movie" :> QueryParam' '[ Required] "name" T.Text :> Get '[ JSON] [Movie]
  :<|> "download_movie" :> ReqBody '[JSON] DownloadMovieReq :> Post '[ JSON] Transfer

server :: AppEnv -> Server API
server AppEnv { putIOAPIToken } = findMovie :<|> downloadMovie
 where
  findMovie
    :: T.Text
    -> Handler [Movie]
  findMovie movie_name = do
    clientEnv <- liftIO CIAPI.getClientEnv
    indexers  <- callServantClient clientEnv CIAPI.getIndexers
    fmap mconcat $ for indexers $ \(Indexer iId _) -> do
      callServantClient clientEnv $ CIAPI.getMovies movie_name iId

  downloadMovie
    :: DownloadMovieReq
    -> Handler Transfer
  downloadMovie (DownloadMovieReq magnet_url) = do
    let reqBody = PutIOAPI.AddTransferReq magnet_url
    clientEnv <- liftIO $ PutIOAPI.getClientEnv putIOAPIToken
    PutIOAPI.AddTransferResponse transfer <- callServantClient clientEnv $ PutIOAPI.addTransfer reqBody
    pure transfer

callServantClient :: ClientEnv -> ClientM a -> Handler a
callServantClient clientEnv clientM = do
  liftIO (runClientM clientM clientEnv) >>= \case
    Left err -> do
      liftIO $ print err
      throwError (err500 { errBody = LBS8.pack (show err) })
    Right movies -> pure movies

myApi :: Proxy API
myApi = Proxy

app :: AppEnv -> Application
app appEnv = do
  allowCors $ serve myApi (server appEnv)

allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        }

$(JSON.deriveJSON (aesonPrefix snakeCase) ''DownloadMovieReq)
