module Zureg.Hackathon.Interface
    ( Hackathon (..)
    , hackathonFromEnv
    ) where

import qualified Data.Aeson                  as A
import qualified Data.Time                         as Time
import qualified Data.Aeson.TH.Extended      as A
import qualified Data.Csv         as Csv
import qualified Data.Text        as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Digestive   as D
import qualified Zureg.Captcha    as Captcha
import qualified Zureg.Database   as Database
import qualified Zureg.Model      as Model
import qualified Zureg.SendEmail  as SendEmail

data Hackathon = Hackathon
    {
    -- | Name of the Hackathon, e.g. "ZuriHac 2020"
      name         :: T.Text
    -- | Base URL, e.g. "https://zureg.zfoh.ch"
    , baseUrl      :: T.Text
    -- | URL of the contact homepage, e.g. "https://zfoh.ch/zurihac2019/#contact"
    , contactUrl   :: T.Text
    -- | Total capacity of the event
    , capacity     :: Int
    -- | When 'True', registrants can/must confirm their registration
    -- TODO: change this to `Maybe Date`?
    , confirmation :: Bool
    -- | Email to send from
    , emailFrom :: T.Text
    -- | When T-shirt order is sent.
    , tShirtDeadline :: Maybe Time.UTCTime
    }

hackathonFromEnv :: IO Hackathon
hackathonFromEnv = pure undefined
