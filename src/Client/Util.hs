module Client.Util where

import           Data.Char

-- | Convert a word to title case by capitalising the first letter
uncapitalise :: String -> String
uncapitalise []       = []
uncapitalise (c : cs) = toLower c : cs
