{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.ChillInstitute.Types where

import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.TH                 as JSON
import           Client.Util
import           Data.Char                      ( isUpper )
import qualified Data.Text                     as T
import           GHC.Generics

data Movie =
  Movie
    { _mId :: T.Text
    , _mLink :: T.Text
    , _mTitle :: T.Text
    }
  deriving (Eq, Show, Generic)

data Indexer =
  Indexer
    { _iId :: T.Text
    , _iName :: T.Text
    }
  deriving (Eq, Show, Generic)

$(JSON.deriveJSON JSON.defaultOptions{JSON.fieldLabelModifier = uncapitalise . dropWhile (not . isUpper)} ''Movie)
$(JSON.deriveJSON JSON.defaultOptions{JSON.fieldLabelModifier = uncapitalise . dropWhile (not . isUpper)} ''Indexer)
