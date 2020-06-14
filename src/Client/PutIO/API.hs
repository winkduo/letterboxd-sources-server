{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.PutIO.API
  ( module Client.PutIO.API
  , module Client.PutIO.API.File
  ) where

import Client.PutIO.API.File
import Client.PutIO.Types
import Client.PutIO.Types
import Client.Util
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import Data.Aeson.Casing
import qualified Data.Aeson.TH as JSON
import Data.Char (isUpper, toLower)
import Data.Proxy (Proxy)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
import Network.HTTP.Client
  ( Manager
  , RequestBody(..)
  , httpLbs
  , managerModifyRequest
  , parseRequest
  , requestBody
  , requestHeaders
  )
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Servant
import Servant.API.ContentTypes
import Servant.Client
import Web.Internal.FormUrlEncoded

data AddTransferReq =
  AddTransferReq
    { _atrUrl :: T.Text
    , _atrSaveParentId :: Maybe Integer
    , _atrCallbackUrl :: Maybe String
    }
  deriving (Show, Eq, Generic)

instance ToForm AddTransferReq where
  toForm = genericToForm formOptions

data TransferResponse =
  TransferResponse
    { _trTransfer :: Transfer
    }
  deriving (Show, Eq, Generic)

data TransfersResponse =
  TransfersResponse
    { _trTransfers :: [Transfer]
    }
  deriving (Show, Eq, Generic)

data Profile =
  Profile
    { _pUsername :: String
    }
  deriving (Show, Eq, Generic)

data UserInfo =
  UserInfo
    { _uiInfo :: Profile
    }
  deriving (Show, Eq, Generic)

data FileDownloadUrlResponse =
  FileDownloadUrlResponse
    { _fdurUrl :: String
    }
  deriving (Show, Eq, Generic)

type RequiredQueryParam = QueryParam' '[ Required, Strict]

type PutIOAPI
   = "transfers" :> Capture "id" Integer :> Get '[ JSON] TransferResponse :<|> "transfers" :> "list" :> Get '[ JSON] TransfersResponse :<|> "transfers" :> "add" :> ReqBody '[ FormUrlEncoded] AddTransferReq :> Post '[ JSON] TransferResponse :<|> "account" :> "info" :> Get '[ JSON] UserInfo :<|> "files" :> "search" :> RequiredQueryParam "query" String :> QueryParam "per_page" Integer :> Get '[ JSON] FilesResponse :<|> "files" :> Capture "id" Integer :> "url" :> Get '[ JSON] FileDownloadUrlResponse :<|> "files" :> "list" :> QueryParam "parent_id" Integer :> QueryParam "file_type" String :> RequiredQueryParam "stream_url" Bool :> RequiredQueryParam "hidden" Bool :> Get '[ JSON] FilesResponse :<|> "files" :> "create-folder" :> ReqBody '[ FormUrlEncoded] CreateFolderReq :> Post '[ JSON] FileResponse

type OAuth2Token = T.Text

data TransferWithLink =
  TransferWithLink
    { _twlTransfer :: Transfer
    , _twlUrl :: Maybe String
    }
  deriving (Show, Eq, Generic)

api :: Proxy PutIOAPI
api = Proxy

getTransfer :: Integer -> ClientM TransferResponse
listTransfers :: ClientM TransfersResponse
addTransfer :: AddTransferReq -> ClientM TransferResponse
showProfile :: ClientM UserInfo
searchFiles :: String -> Maybe Integer -> ClientM FilesResponse
getFileDownloadUrl :: Integer -> ClientM FileDownloadUrlResponse
listFiles ::
     Maybe Integer -> Maybe String -> Bool -> Bool -> ClientM FilesResponse
createFolder :: CreateFolderReq -> ClientM FileResponse
getTransfer :<|> listTransfers :<|> addTransfer :<|> showProfile :<|> searchFiles :<|> getFileDownloadUrl :<|> listFiles :<|> createFolder =
  client api

getClientEnv :: OAuth2Token -> IO ClientEnv
getClientEnv token = do
  manager <-
    liftIO $
    newTlsManagerWith
      tlsManagerSettings
        { managerModifyRequest =
            \r -> do
              print r
              case requestBody r of
                RequestBodyBS b -> print b
                RequestBodyLBS b -> print b
                _ -> pure ()
              pure
                r
                  { requestHeaders =
                      [ ("Authorization", "token " <> T.encodeUtf8 token)
                      , ("Content-Type", "application/x-www-form-urlencoded")
                      ]
                  }
        }
  baseUrl <- parseBaseUrl "https://api.put.io/v2"
  pure $ mkClientEnv manager baseUrl

$(JSON.deriveJSON (aesonPrefix snakeCase) ''Profile)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''UserInfo)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''TransferResponse)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''TransfersResponse)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''TransferWithLink)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''FileDownloadUrlResponse)
