{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.ZuriHac2020
    ( newHackathon
    ) where

import qualified Data.Text                           as T
import           System.Environment                  (getEnv)
import qualified Text.Blaze.Html5                    as H
import qualified Zureg.Captcha.ReCaptcha             as ReCaptcha
import qualified Zureg.Database                      as Database
import           Zureg.Hackathon.Interface           (Hackathon)
import qualified Zureg.Hackathon.Interface           as Hackathon
import           Zureg.Hackathon.ZuriHac2020.Discord as Discord
import           Zureg.Hackathon.ZuriHac2020.Form    as ZH20
import           Zureg.Hackathon.ZuriHac2020.Model   as ZH20
import           Zureg.Hackathon.ZuriHac2020.Views   as ZH20
import qualified Zureg.SendEmail                     as SendEmail

newHackathon :: IO (Hackathon RegisterInfo)
newHackathon = do
    scannerSecret   <- T.pack <$> getEnv "ZUREG_SCANNER_SECRET"
    email           <- T.pack <$> getEnv "ZUREG_EMAIL"

    discord <- Discord.configFromEnv
    channel <- Discord.getWelcomeChannelId discord

    reCaptchaSecret <- T.pack <$> getEnv "ZUREG_RECAPTCHA_SECRET"
    captcha         <- ReCaptcha.new ReCaptcha.Config
        { ReCaptcha.cSiteKey   = "6LcVUm8UAAAAAL0ooPLkNT3O9oEXhGPK6kZ-hQk7"
        , ReCaptcha.cSecretKey = reCaptchaSecret
        }

    return Hackathon.emptyHackathon
        { Hackathon.name = "ZuriHac 2020"
        , Hackathon.baseUrl = "https://zureg.zfoh.ch"
        , Hackathon.contactUrl = "https://zfoh.ch/zurihac2020/#contact"
        , Hackathon.capacity = 3000
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
        , Hackathon.captcha = captcha
        , Hackathon.scannerSecret = scannerSecret
        , Hackathon.chatUrl = Discord.generateTempInviteUrl discord channel
        , Hackathon.chatExplanation = H.p $ do
            "ZuriHac 2020 will take place as an online event.  To "
            "coordinate the hackathon, we use Discord as a chat and voice "
            "platform.  You can join the Discord server here:"
        }
