{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.PutIO.API where

import           Client.PutIO.Types
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.TH                 as JSON
import           Data.Proxy                     ( Proxy )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Network.HTTP.Client            ( Manager
                                                , httpLbs
                                                , managerModifyRequest
                                                , requestBody
                                                , RequestBody(..)
                                                , parseRequest
                                                , requestHeaders
                                                )
import           Network.HTTP.Client.TLS        ( newTlsManagerWith
                                                , tlsManagerSettings
                                                )
import           Servant
import           Servant.Client
import           GHC.Generics
import           Data.Char                      ( isUpper
                                                , toLower
                                                )
import           Client.Util                    ( uncapitalise )
import           Servant.API.ContentTypes
import           Web.Internal.FormUrlEncoded
import           Data.Aeson.Casing
import           Client.PutIO.Types

data AddTransferReq =
  AddTransferReq
    { url :: T.Text
    }
  deriving (Show, Eq, Generic)

instance ToForm AddTransferReq

data AddTransferResponse = AddTransferResponse { _atrTransfer :: Transfer } deriving (Show, Eq, Generic)

data Profile = Profile { _pUsername :: String } deriving (Show, Eq, Generic)

data UserInfo = UserInfo { _uiInfo :: Profile } deriving (Show, Eq, Generic)

type PutIOAPI
  = "transfers" :> "add" :> ReqBody '[FormUrlEncoded] AddTransferReq :> Post '[ JSON] AddTransferResponse :<|> "account" :> "info" :> Get '[JSON] UserInfo

type OAuth2Token = T.Text

api :: Proxy PutIOAPI
api = Proxy

addTransfer :: AddTransferReq -> ClientM AddTransferResponse
showProfile :: ClientM UserInfo
addTransfer :<|> showProfile = client api

getClientEnv :: OAuth2Token -> IO ClientEnv
getClientEnv token = do
  manager <- liftIO $ newTlsManagerWith tlsManagerSettings
    { managerModifyRequest =
      \r -> do
        print r
        case requestBody r of
          RequestBodyBS  b -> print b
          RequestBodyLBS b -> print b
        pure r
          { requestHeaders = [ ("Authorization", "token " <> T.encodeUtf8 token)
                             , ( "Content-Type"
                               , "application/x-www-form-urlencoded"
                               )
                             ]
          }
    }
  baseUrl <- parseBaseUrl "https://api.put.io/v2"
  pure $ mkClientEnv manager baseUrl

$(JSON.deriveJSON (aesonPrefix snakeCase) ''Profile)
$(JSON.deriveJSON (aesonPrefix snakeCase) ''UserInfo)
$(JSON.deriveJSON (aesonPrefix snakeCase) ''AddTransferResponse)
