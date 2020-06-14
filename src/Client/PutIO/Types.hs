{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.PutIO.Types where

import qualified Data.Aeson as JSON
import Data.Aeson.Casing
import qualified Data.Aeson.TH as JSON
import GHC.Generics

data Transfer =
  Transfer
    { _tAvailability :: Maybe Integer
    , _tCreatedAt :: Maybe String
    , _tCurrentRatio :: Maybe Double
    , _tDownloaded :: Maybe Double
    , _tUploaded :: Maybe Double
    , _tDownSpeed :: Maybe Integer
    , _tUpSpeed :: Maybe Integer
    , _tErrorMessage :: Maybe String
    , _tEstimatedTime :: Maybe Integer
    , _tFileId :: Maybe Integer
    , _tFinishedAt :: Maybe String
    , _tId :: Integer
    , _tIsPrivate :: Maybe Bool
    , _tName :: String
    , _tPeers :: Maybe Integer
    , _tPercentDone :: Maybe Integer
    , _tSaveParentId :: Integer
    , _tSecondsSeeding :: Maybe Integer
    , _tSize :: Maybe Integer
    , _tSource :: String
    , _tStatus :: Maybe String
    , _tSubscriptionId :: Maybe Integer
    , _tTrackerMessage :: Maybe String
    , _tType :: Maybe String
    }
  deriving (Show, Eq, Generic)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''Transfer)
