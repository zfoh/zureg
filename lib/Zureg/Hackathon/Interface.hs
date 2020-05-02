module Zureg.Hackathon.Interface
    ( Hackathon (..)
    ) where

import qualified Data.Csv         as Csv
import qualified Data.Text        as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Digestive   as D
import qualified Zureg.Database   as Database
import           Zureg.Model      (CustomEventHandler)
import qualified Zureg.ReCaptcha  as ReCaptcha
import qualified Zureg.SendEmail  as SendEmail

-- | A Hackathon.  This 'a' is the additional information that is specific to
-- this hackathon.  'e' are custom events in the event sourcing that modify
-- this additional info using a 'CustomEventHandler'.
data Hackathon e a = Hackathon
    {
    -- | Name of the Hackathon, e.g. "ZuriHac 2020"
      name            :: T.Text
    -- | Base URL, e.g. "https://zureg.zfoh.ch"
    , baseUrl         :: T.Text
    -- | URL of the contact homepage, e.g. "https://zfoh.ch/zurihac2019/#contact"
    , contactUrl      :: T.Text
    -- | Slack URL, e.g. "https://slack.zurihac.info/"
    , slackUrl        :: T.Text
    -- | Total capacity of the event
    , capacity        :: Int
    -- | When 'True', registrants can/must confirm their registration.
    , confirmation    :: Bool

    -- | Registration form
    , registerForm    :: D.Form H.Html IO a
    -- | Registration view
    , registerView    :: D.View H.Html -> H.Html
    -- | Ticket view
    , ticketView      :: a -> H.Html
    -- | Scan view
    , scanView        :: a -> H.Html
    -- | CSV header
    , csvHeader       :: Csv.Header

    -- | Database configuration
    , databaseConfig  :: Database.Config
    -- | Email sending configuration
    , sendEmailConfig :: SendEmail.Config
    -- | ReCaptcha configuration
    , reCaptchaConfig :: ReCaptcha.Config
    -- | Secret for the scanner
    , scannerSecret   :: T.Text

    -- | Process custom events to modify the registrant state
    , customEventHandler :: CustomEventHandler e a
    }
