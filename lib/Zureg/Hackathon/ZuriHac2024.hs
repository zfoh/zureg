{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.ZuriHac2024
    ( newHackathon
    ) where

import qualified Data.Text                           as T
import           System.Environment                  (getEnv)
import qualified Text.Blaze.Html5                    as H
import qualified Zureg.Database                      as Database
import           Zureg.Hackathon.Interface           (Hackathon)
import qualified Zureg.Hackathon.Interface           as Hackathon
import           Zureg.Hackathon.ZuriHac2020.Discord as Discord
import           Zureg.Hackathon.ZuriHac2024.Form    as ZH24
import           Zureg.Hackathon.ZuriHac2024.Model   as ZH24
import           Zureg.Hackathon.ZuriHac2024.Views   as ZH24
import qualified Zureg.Captcha.HCaptcha              as HCaptcha
import qualified Zureg.SendEmail                     as SendEmail

newHackathon :: IO (Hackathon RegisterInfo)
newHackathon = do
    scannerSecret   <- T.pack <$> getEnv "ZUREG_SCANNER_SECRET"
    email           <- T.pack <$> getEnv "ZUREG_EMAIL"
    captcha         <- HCaptcha.configFromEnv >>= HCaptcha.new

    discord <- Discord.configFromEnv
    channel <- Discord.getWelcomeChannelId discord

    return Hackathon.Hackathon
        { Hackathon.name = "ZuriHac 2024"
        , Hackathon.baseUrl = "https://zureg.zfoh.ch"
        , Hackathon.contactUrl = "https://zfoh.ch/zurihac2024/#contact"
        , Hackathon.capacity = 500
        , Hackathon.confirmation = False

        , Hackathon.registerBadgeName = False
        , Hackathon.registerAffiliation = False

        , Hackathon.registerForm = ZH24.additionalInfoForm
        , Hackathon.registerView = ZH24.additionalInfoView
        , Hackathon.ticketView = mempty
        , Hackathon.scanView = ZH24.scanView
        , Hackathon.csvHeader = ZH24.csvHeader

        , Hackathon.databaseConfig = Database.defaultConfig
        , Hackathon.sendEmailConfig = SendEmail.Config
            { SendEmail.cFrom = "ZuriHac Registration Bot <" <> email <> ">"
            }
        , Hackathon.captcha = captcha
        , Hackathon.scannerSecret = scannerSecret
        , Hackathon.chatUrl = Discord.generateTempInviteUrl discord channel
        , Hackathon.chatExplanation = H.p $ do
            "ZuriHac 2024 will use Discord as a chat platform for "
            "coordination.  You can join the Discord server here:"
        }
