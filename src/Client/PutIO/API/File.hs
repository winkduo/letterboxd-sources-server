{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.PutIO.API.File where

import Client.Util
import qualified Data.Aeson as JSON
import Data.Aeson.Casing
import qualified Data.Aeson.TH as JSON
import GHC.Generics
import Web.Internal.FormUrlEncoded

data SearchFilesReq =
  SearchFilesReq
    { _sfrQuery :: String
    , _sfrPerPage :: Maybe Integer
    }
  deriving (Show, Eq, Generic)

instance ToForm SearchFilesReq where
  toForm = genericToForm formOptions

data ListFilesReq =
  ListFilesReq
    { _lfrParentId :: Maybe Integer
    , _lfrFileType :: Maybe String
    , _lfrStreamUrl :: Bool -- Include stream urls
    , _lfrHidden :: Bool -- Include hidden (root-level default) files
    }
  deriving (Show, Eq, Generic)

instance ToForm ListFilesReq where
  toForm = genericToForm formOptions

data CreateFolderReq =
  CreateFolderReq
    { _cfrName :: String
    , _cfgParentId :: Integer
    }
  deriving (Show, Eq, Generic)

instance ToForm CreateFolderReq where
  toForm = genericToForm formOptions

data FilesResponse =
  FilesResponse
    { _frFiles :: [File]
    }
  deriving (Show, Eq, Generic)

data FileResponse =
  FileResponse
    { _frFile :: File
    }
  deriving (Show, Eq, Generic)

-- https://app.swaggerhub.com/apis-docs/putio/putio/2.7.2#/files/get_files_search
data File =
  File
    { _fContentType :: Maybe String
    , _fCrc32 :: Maybe String
    , _fCreatedAt :: Maybe String
    , _fId :: Integer
    , _fIsMp4Available :: Maybe Bool
    , _fIsShared :: Maybe Bool
    , _fName :: String
    , _fParentId :: Integer
    , _fScreenshot :: Maybe String
    , _fSize :: Maybe Integer
    , _fFileType :: String -- Consider parsing the enum.
    , _fExtension :: Maybe String
    , _fNeedConvert :: Maybe Bool
    , _fMp4Size :: Maybe Integer
    }
  deriving (Show, Eq, Generic)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''File)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''FilesResponse)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''FileResponse)
