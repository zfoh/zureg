module Zureg.Hackathon.Interface
    ( Hackathon (..)
    , hackathonFromEnv
    ) where

import qualified Data.Text as T
import qualified Data.Time as Time

data Hackathon = Hackathon
    {
    -- | Name of the Hackathon, e.g. "ZuriHac 2020"
      name           :: T.Text
    -- | Base URL, e.g. "https://zureg.zfoh.ch"
    , baseUrl        :: T.Text
    -- | URL of the contact homepage, e.g. "https://zfoh.ch/zurihac2019/#contact"
    , contactUrl     :: T.Text
    -- | Total capacity of the event
    , capacity       :: Int
    -- | When 'True', registrants can/must confirm their registration
    -- TODO: change this to `Maybe Date`?
    , confirmation   :: Bool
    -- | Email to send from
    , emailFrom      :: T.Text
    -- | When T-shirt order is sent.
    , tShirtDeadline :: Maybe Time.UTCTime
    -- | Secret for accessing the scanner page.
    , scannerSecret :: T.Text
    }

hackathonFromEnv :: IO Hackathon
hackathonFromEnv = pure undefined
