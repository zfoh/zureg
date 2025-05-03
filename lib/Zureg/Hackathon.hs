{-# LANGUAGE TemplateHaskell #-}
module Zureg.Hackathon
    ( Hackathon (..)
    ) where

import qualified Data.Aeson.TH.Extended as A
import qualified Data.Text              as T
import qualified Data.Time              as Time

data Hackathon = Hackathon
    {
    -- | Name of the Hackathon, e.g. "ZuriHac 2020"
      name           :: T.Text
    -- | Base URL, e.g. "https://zureg.zfoh.ch"
    , baseURL        :: T.Text
    -- | URL of the contact homepage, e.g. "https://zfoh.ch/zurihac2019/#contact"
    , contactURL     :: T.Text
    -- | Total capacity of the event
    , capacity       :: Int
    -- | When 'True', registrants can confirm their registration
    , confirmation   :: Maybe Time.Day
    -- | Email to send from
    , emailFrom      :: T.Text
    -- | When T-shirt order is sent.
    , tShirtDeadline :: Maybe Time.Day
    } deriving (Show)

$(A.deriveJSON A.defaultOptions ''Hackathon)
