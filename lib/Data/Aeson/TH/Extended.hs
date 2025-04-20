module Data.Aeson.TH.Extended
    ( module Data.Aeson.TH
    , options
    ) where

import           Data.Aeson.TH
import           Data.Char     (isUpper, toLower, isLower)

options :: Options
options = defaultOptions
    { fieldLabelModifier = dropPrefix
    }

dropPrefix :: String -> String
dropPrefix str = case break isUpper str of
    (_, [])    -> str
    (_, field) -> case break isLower field of
        (_,          []) -> map toLower field
        ([],         _)  -> map toLower field
        (xs@[_],     ys) -> map toLower xs ++ ys
        (xs@(_ : _), ys) -> map toLower (init xs) ++ [last xs] ++ ys
