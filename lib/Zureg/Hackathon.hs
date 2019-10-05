{-# LANGUAGE TemplateHaskell #-}
module Zureg.Hackathon (
      Config (..)
    , Handle (..)
    ) where

import qualified Data.Aeson.TH.Extended as A
import qualified Data.Csv               as Csv
import qualified Data.Text              as T
import qualified Text.Blaze.Html5       as H
import qualified Text.Digestive         as D

data Config = Config
    { cName       :: !T.Text -- ^ Name of the Hackathon, e.g. "ZuriHac 2020"
    , cBaseUrl    :: !T.Text -- ^ Base URL, e.g. "https://zureg.zfoh.ch"
    , cContactUrl :: !T.Text -- ^ URL of the hackathon homepage, e.g. "https://zfoh.ch/zurihac2019/#contact"
    , cSlackUrl   :: !T.Text -- ^ Slack URL, e.g. "https://slack.zurihac.info/"
    , cWaitlist   :: !Bool   -- ^ When 'True', new registrants are added to the waitlist
    } deriving (Eq, Show)

$(A.deriveJSON A.options ''Config)

data Handle a = Handle
    { hConfig       :: Config
    , hRegisterForm :: D.Form H.Html IO a
    , hRegisterView :: D.View H.Html -> H.Html
    , hTicketView   :: a -> H.Html
    , hScanView     :: a -> H.Html
    , hCsvHeader    :: Csv.Header
    }