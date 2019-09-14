{-# LANGUAGE OverloadedStrings #-}
module ZuriHac2019 (withHandle) where

import           Zureg.Config      as Config
import           Zureg.Hackathon   as Hackathon
import           ZuriHac2019.Form  as ZH19
import           ZuriHac2019.Model as ZH19
import           ZuriHac2019.Views as ZH19


withHandle :: Config.Config -> (Hackathon.Handle ZH19.RegisterInfo -> IO a) -> IO a
withHandle config action = do
    hackathonConfig <- Config.section "hackathon" config
    let handle = Hackathon.Handle
            { hConfig = hackathonConfig
            , hRegisterForm = ZH19.additionalInfoForm
            , hRegisterView = ZH19.additionalInfoView hackathonConfig
            , hTicketView = ZH19.ticketView
            , hScanView = ZH19.scanView
            , hCsvHeader = ZH19.csvHeader
            }
    action handle