{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.ChillInstitute.Types where

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import GHC.Generics

data Movie =
  Movie
    { _mId :: T.Text
    , _mLink :: T.Text
    , _mTitle :: T.Text
    }
  deriving (Eq, Show, Generic)

deriving instance JSON.ToJSON Movie

instance JSON.FromJSON Movie where
  parseJSON =
    JSON.withObject "Movie" $ \v ->
      Movie <$> v JSON..: "id" <*> v JSON..: "link" <*> v JSON..: "title"

data Indexer =
  Indexer
    { _iId :: T.Text
    , _iName :: T.Text
    }
  deriving (Eq, Show, Generic)

deriving instance JSON.ToJSON Indexer

instance JSON.FromJSON Indexer where
  parseJSON =
    JSON.withObject "Indexer" $ \v ->
      Indexer <$> v JSON..: "id" <*> v JSON..: "name"
