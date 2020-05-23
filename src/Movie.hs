{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Movie where

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
