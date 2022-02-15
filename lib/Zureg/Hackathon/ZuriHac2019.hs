{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.ZuriHac2019
    ( newHackathon
    ) where

import qualified Data.Text                         as T
import           System.Environment                (getEnv)
import qualified Text.Blaze.Html5                  as H
import qualified Zureg.Captcha.ReCaptcha                     as ReCaptcha
import qualified Zureg.Database                    as Database
import           Zureg.Hackathon.Interface         (Hackathon)
import qualified Zureg.Hackathon.Interface         as Hackathon
import           Zureg.Hackathon.ZuriHac2019.Form  as ZH19
import           Zureg.Hackathon.ZuriHac2019.Model as ZH19
import           Zureg.Hackathon.ZuriHac2019.Views as ZH19
import qualified Zureg.SendEmail                   as SendEmail

newHackathon :: IO (Hackathon RegisterInfo)
newHackathon = do
    scannerSecret   <- T.pack <$> getEnv "ZUREG_SCANNER_SECRET"
    email           <- T.pack <$> getEnv "ZUREG_EMAIL"

    reCaptchaSecret <- T.pack <$> getEnv "ZUREG_RECAPTCHA_SECRET"
    captcha         <- ReCaptcha.new ReCaptcha.Config
        { ReCaptcha.cSiteKey   = "6LcVUm8UAAAAAL0ooPLkNT3O9oEXhGPK6kZ-hQk7"
        , ReCaptcha.cSecretKey = reCaptchaSecret
        }

    return Hackathon.Hackathon
        { Hackathon.name = "ZuriHac 2019"
        , Hackathon.baseUrl = "https://zureg.zfoh.ch"
        , Hackathon.contactUrl = "https://zfoh.ch/zurihac2019/#contact"
        , Hackathon.capacity = 500
        , Hackathon.confirmation = True

        , Hackathon.registerBadgeName = True
        , Hackathon.registerAffiliation = True

        , Hackathon.registerForm = ZH19.additionalInfoForm
        , Hackathon.registerView = ZH19.additionalInfoView
        , Hackathon.ticketView = ZH19.ticketView
        , Hackathon.scanView = ZH19.scanView
        , Hackathon.csvHeader = ZH19.csvHeader

        , Hackathon.databaseConfig = Database.defaultConfig
        , Hackathon.sendEmailConfig = SendEmail.Config
            { SendEmail.cFrom = "ZuriHac Registration Bot <" <> email <> ">"
            }
        , Hackathon.captcha = captcha
        , Hackathon.scannerSecret = scannerSecret
        , Hackathon.chatExplanation = H.p
            "You can join the Slack instance for ZuriHac 2019 here:"
        , Hackathon.chatUrl = pure "https://slack.zurihac.info/"
        }
