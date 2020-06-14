module Client.Util where

import Data.Aeson.Casing
import Data.Char
import Web.Internal.FormUrlEncoded

-- | Convert a word to title case by capitalising the first letter
uncapitalise :: String -> String
uncapitalise [] = []
uncapitalise (c:cs) = toLower c : cs

dropPrefix :: String -> String
dropPrefix = dropWhile (not . isUpper)

formOptions :: FormOptions
formOptions = FormOptions $ snakeCase . dropPrefix

foldMapA :: (Monoid m, Applicative f) => (a -> f m) -> [a] -> f m
foldMapA f l = mconcat <$> traverse f l
