{-# LANGUAGE TemplateHaskell #-}
module Zureg.Web
    ( Config (..)
    ) where

import qualified Data.Aeson.TH.Extended as A

data Config = Config
    { configPort :: !Int
    } deriving (Show)

$(A.deriveJSON A.options ''Config)
