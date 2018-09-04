module Data.Aeson.TH.Extended
    ( module Data.Aeson.TH
    , options
    ) where

import           Data.Aeson.TH
import           Data.Char     (isUpper, toLower)

options :: Options
options = defaultOptions
    { fieldLabelModifier = dropPrefix
    }

dropPrefix :: String -> String
dropPrefix str = case break isUpper str of
    (_, (y : ys)) -> toLower y : ys
    _             -> str
