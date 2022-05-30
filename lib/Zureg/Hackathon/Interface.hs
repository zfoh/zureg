module Zureg.Hackathon.Interface
    ( Hackathon (..)
    ) where

import qualified Data.Csv         as Csv
import qualified Data.Text        as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Digestive   as D
import qualified Zureg.Captcha    as Captcha
import qualified Zureg.Database   as Database
import qualified Zureg.Model      as Model
import qualified Zureg.SendEmail  as SendEmail

data Hackathon a = Hackathon
    {
    -- | Name of the Hackathon, e.g. "ZuriHac 2020"
      name                :: T.Text
    -- | Base URL, e.g. "https://zureg.zfoh.ch"
    , baseUrl             :: T.Text
    -- | URL of the contact homepage, e.g. "https://zfoh.ch/zurihac2019/#contact"
    , contactUrl          :: T.Text
    -- | Total capacity of the event
    , capacity            :: Int
    -- | When 'True', registrants can/must confirm their registration.
    , confirmation        :: Bool

    -- | Prompt for badge names in the registration
    , registerBadgeName   :: Bool
    -- | Prompt for affiliations in the registration
    , registerAffiliation :: Bool

    -- | Extra Hackathon-specific info form
    , registerForm        :: D.Form H.Html IO a
    -- | Extra Hackathon-specific info form view
    , registerView        :: D.View H.Html -> H.Html
    -- | Ticket view
    , ticketView          :: a -> H.Html
    -- | Scan view
    , scanView            :: Model.Registrant a -> H.Html
    -- | CSV header
    , csvHeader           :: Csv.Header

    -- | Database configuration
    , databaseConfig      :: Database.Config
    -- | Email sending configuration
    , sendEmailConfig     :: SendEmail.Config
    -- | Captcha configuration
    , captcha             :: Captcha.Handle
    -- | Secret for the scanner
    , scannerSecret       :: T.Text

    -- | Explaining paragraph about the chat.
    , chatExplanation     :: H.Html
    -- | Produce an URL to join the chat, e.g. a Slack link or a Discord invite.
    , chatUrl             :: IO T.Text
    }
