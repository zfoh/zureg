module ZuriHac2019.Hackathon (withHandle) where

import           Zureg.Hackathon   as Hackathon
import           ZuriHac2019.Form  as ZH19
import           ZuriHac2019.Model as ZH19
import           ZuriHac2019.Views as ZH19

withHandle :: Hackathon.Config -> (Hackathon.Handle ZH19.RegisterInfo -> IO a) -> IO a
withHandle config action = do
    let handle = Hackathon.Handle
            { hConfig = config
            , hRegisterForm = ZH19.additionalInfoForm
            , hRegisterView = ZH19.additionalInfoView config
            , hTicketView = ZH19.ticketView
            , hScanView = ZH19.scanView
            }
    in  action handle