module ZuriHac2019.Hackathon (
      zuriHac2019
    ) where

import           Zureg.Hackathon   as Hackathon
import           ZuriHac2019.Form  as ZH19
import           ZuriHac2019.Model as ZH19
import           ZuriHac2019.Views as ZH19

zuriHac2019 :: Hackathon.Config -> Hackathon.Handle ZH19.RegisterInfo
zuriHac2019 config = Hackathon.Handle
    { hConfig = config
    , hRegisterForm = ZH19.additionalInfoForm
    , hRegisterView = ZH19.additionalInfoView config
    , hTicketView = ZH19.ticketView
    , hScanView = ZH19.scanView
    }