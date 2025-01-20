{-# LANGUAGE TemplateHaskell #-}
module Zureg.Web
    ( Config (..)
    ) where

import qualified Data.Aeson.TH.Extended as A
import qualified Data.Text              as T

data Config = Config
    { configHost :: !T.Text
    , configPort :: !Int
    } deriving (Show)

$(A.deriveJSON A.options ''Config)
