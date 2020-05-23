{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Client.ChillInstitute.API (getClientEnv, getIndexers, getMovies)
import Client.ChillInstitute.Types
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor (($>))
import Data.Proxy (Proxy)
import qualified Data.Text as T
import Data.Traversable (for)
import Servant
import Servant.Client (ClientEnv, ClientM, runClientM)
import Servant.Server

type API = "find_movie" :> QueryParam "name" T.Text :> Get '[ JSON] [Movie] -- GET /find_movie

server :: Server API
server = findMovie
  where
    findMovie :: Maybe T.Text -> Handler [Movie]
    findMovie Nothing = pure []
    findMovie (Just movie_name) = do
      clientEnv <- liftIO getClientEnv
      indexers <- callChillInstitute clientEnv getIndexers
      fmap mconcat $
        for indexers $ \(Indexer iId _) -> do
          callChillInstitute clientEnv $ getMovies movie_name iId

callChillInstitute :: ClientEnv -> ClientM a -> Handler a
callChillInstitute clientEnv clientM = do
  liftIO (runClientM clientM clientEnv) >>= \case
    Left err -> throwError (err500 {errBody = LBS8.pack (show err)})
    Right movies -> pure movies

myApi :: Proxy API
myApi = Proxy

app :: Application
app = serve myApi server
