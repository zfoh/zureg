{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.ZuriHac2020
    ( newHackathon
    ) where

import qualified Data.Text                         as T
import           Data.Void                         (Void)
import           System.Environment                (getEnv)
import qualified Zureg.Database                    as Database
import           Zureg.Hackathon.Interface         (Hackathon)
import qualified Zureg.Hackathon.Interface         as Hackathon
import           Zureg.Hackathon.ZuriHac2020.Form  as ZH20
import           Zureg.Hackathon.ZuriHac2020.Model as ZH20
import           Zureg.Hackathon.ZuriHac2020.Views as ZH20
import qualified Zureg.ReCaptcha                   as ReCaptcha
import qualified Zureg.SendEmail                   as SendEmail

newHackathon :: IO (Hackathon Void RegisterInfo)
newHackathon = do
    scannerSecret   <- T.pack <$> getEnv "ZUREG_SCANNER_SECRET"
    reCaptchaSecret <- T.pack <$> getEnv "ZUREG_RECAPTCHA_SECRET"
    email           <- T.pack <$> getEnv "ZUREG_EMAIL"

    return Hackathon.Hackathon
        { Hackathon.name = "ZuriHac 2020"
        , Hackathon.baseUrl = "https://zureg.zfoh.ch"
        , Hackathon.contactUrl = "https://zfoh.ch/zurihac2020/#contact"
        , Hackathon.slackUrl = "https://slack.zurihac.info/"
        , Hackathon.capacity = 800
        , Hackathon.confirmation = False

        , Hackathon.registerForm = ZH20.additionalInfoForm
        , Hackathon.registerView = ZH20.additionalInfoView
        , Hackathon.ticketView = ZH20.ticketView
        , Hackathon.scanView = ZH20.scanView
        , Hackathon.csvHeader = ZH20.csvHeader

        , Hackathon.databaseConfig = Database.defaultConfig
        , Hackathon.sendEmailConfig = SendEmail.Config
            { SendEmail.cFrom = "ZuriHac Registration Bot <" <> email <> ">"
            }
        , Hackathon.reCaptchaConfig = ReCaptcha.Config
            { ReCaptcha.cEnabled   = True
            , ReCaptcha.cSiteKey   = "6LcVUm8UAAAAAL0ooPLkNT3O9oEXhGPK6kZ-hQk7"
            , ReCaptcha.cSecretKey = reCaptchaSecret
            }
        , Hackathon.scannerSecret = scannerSecret

        , Hackathon.customEventHandler = \_ e -> e
        }
