{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.PutIO.Types where

import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.TH                 as JSON
import           GHC.Generics
import           Data.Aeson.Casing

data Transfer = Transfer { _tAvailability :: Maybe Integer
                         , _tCreatedAt :: Maybe String
                         , _tCurrentRatio :: Maybe Integer
                         , _tDownloaded :: Maybe Integer
                         , _tUploaded :: Maybe Integer
                         , _tDownSpeed :: Maybe Integer
                         , _tUpSpeed :: Maybe Integer
                         , _tErrorMessage :: Maybe String
                         , _tEstimatedTime :: Maybe Integer
                         , _tFileId :: Maybe Integer
                         , _tFinishedAt :: Maybe String
                         , _tId :: Maybe Integer
                         , _tIsPrivate :: Maybe Bool
                         , _tName :: Maybe String
                         , _tPeers :: Maybe Integer
                         , _tPercentDone :: Maybe Integer
                         , _tSaveParentId :: Maybe Integer
                         , _tSecondsSeeding :: Maybe Integer
                         , _tSize :: Maybe Integer
                         , _tSource :: Maybe String
                         , _tStatus :: Maybe String
                         , _tSubscriptionId :: Maybe Integer
                         , _tTrackerMessage :: Maybe String
                         , _tType :: Maybe String
                         } deriving (Show, Eq, Generic)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''Transfer)
