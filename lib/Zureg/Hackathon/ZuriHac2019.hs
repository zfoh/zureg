{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.ZuriHac2019
    ( newHackathon
    ) where

import qualified Data.Text                         as T
import           System.Environment                (getEnv)
import qualified Zureg.Database                    as Database
import           Zureg.Hackathon.Interface         (Hackathon)
import qualified Zureg.Hackathon.Interface         as Hackathon
import           Zureg.Hackathon.ZuriHac2019.Form  as ZH19
import           Zureg.Hackathon.ZuriHac2019.Model as ZH19
import           Zureg.Hackathon.ZuriHac2019.Views as ZH19
import qualified Zureg.ReCaptcha                   as ReCaptcha
import qualified Zureg.SendEmail                   as SendEmail

newHackathon :: IO (Hackathon RegisterInfo)
newHackathon = do
    scannerSecret   <- T.pack <$> getEnv "ZUREG_SCANNER_SECRET"
    reCaptchaSecret <- T.pack <$> getEnv "ZUREG_RECAPTCHA_SECRET"

    return Hackathon.Hackathon
        { Hackathon.name = "ZuriHac 2019"
        , Hackathon.baseUrl = "https://zureg.zfoh.ch"
        , Hackathon.contactUrl = "https://zfoh.ch/zurihac2019/#contact"
        , Hackathon.slackUrl = "https://slack.zurihac.info/"
        , Hackathon.waitlist = True

        , Hackathon.registerForm = ZH19.additionalInfoForm
        , Hackathon.registerView = ZH19.additionalInfoView
        , Hackathon.ticketView = ZH19.ticketView
        , Hackathon.scanView = ZH19.scanView
        , Hackathon.csvHeader = ZH19.csvHeader

        , Hackathon.databaseConfig = Database.defaultConfig
        , Hackathon.sendEmailConfig = SendEmail.Config
            { SendEmail.cFrom = "ZuriHac Registration Bot <zureg@zfoh.ch>"
            }
        , Hackathon.reCaptchaConfig = ReCaptcha.Config
            { ReCaptcha.cEnabled = True
            , ReCaptcha.cSiteKey = "6LcVUm8UAAAAAL0ooPLkNT3O9oEXhGPK6kZ-hQk7"
            , ReCaptcha.cSecretKey = reCaptchaSecret
            }
        , Hackathon.scannerSecret = scannerSecret
        }
