{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.ZuriHac2022
    ( newHackathon
    ) where

import qualified Data.Text                           as T
import           System.Environment                  (getEnv)
import qualified Text.Blaze.Html5                    as H
import qualified Zureg.Database                      as Database
import           Zureg.Hackathon.Interface           (Hackathon)
import qualified Zureg.Hackathon.Interface           as Hackathon
import           Zureg.Hackathon.ZuriHac2020.Discord as Discord
import           Zureg.Hackathon.ZuriHac2022.Form    as ZH22
import           Zureg.Hackathon.ZuriHac2022.Model   as ZH22
import qualified Zureg.ReCaptcha                     as ReCaptcha
import qualified Zureg.SendEmail                     as SendEmail

newHackathon :: IO (Hackathon RegisterInfo)
newHackathon = do
    scannerSecret   <- T.pack <$> getEnv "ZUREG_SCANNER_SECRET"
    reCaptchaSecret <- T.pack <$> getEnv "ZUREG_RECAPTCHA_SECRET"
    email           <- T.pack <$> getEnv "ZUREG_EMAIL"

    discord <- Discord.configFromEnv
    channel <- Discord.getWelcomeChannelId discord

    return Hackathon.Hackathon
        { Hackathon.name = "ZuriHac 2022"
        , Hackathon.baseUrl = "https://zureg.zfoh.ch"
        , Hackathon.contactUrl = "https://zfoh.ch/zurihac2022/#contact"
        , Hackathon.capacity = 500
        , Hackathon.confirmation = False

        , Hackathon.registerBadgeName = False
        , Hackathon.registerAffiliation = False

        , Hackathon.registerForm = ZH22.additionalInfoForm
        , Hackathon.registerView = ZH22.additionalInfoView
        , Hackathon.ticketView = mempty
        , Hackathon.scanView = mempty
        , Hackathon.csvHeader = ZH22.csvHeader

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
        , Hackathon.chatUrl = Discord.generateTempInviteUrl discord channel
        , Hackathon.chatExplanation = H.p $ do
            "Even though it will hopefully take place as a physical event, "
            "ZuriHac 2022 will use Discord as a chat platform for "
            "coordination.  You can join the Discord server here:"
        }
