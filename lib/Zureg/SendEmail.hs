-- | Sending an email.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.SendEmail
    ( Handle
    , withHandle
    , sendEmail
    ) where

import qualified Amazonka.Extended              as Amazonka
import qualified Amazonka.SES                   as SES
import qualified Amazonka.SES.Types.Body        as SES
import qualified Amazonka.SES.Types.Destination as SES
import           Control.Lens                   ((&), (.~))
import           Control.Monad                  (void)
import qualified Data.Aeson.TH.Extended         as A
import qualified Data.Text                      as T

data Handle = Handle
    { hAwsEnv :: !Amazonka.Env
    }

withHandle :: (Handle -> IO a) -> IO a
withHandle f = do
    hAwsEnv <- Amazonka.smartEnv
    f Handle {..}

sendEmail
    :: Handle
    -> T.Text  -- ^ From
    -> T.Text  -- ^ To
    -> T.Text  -- ^ Subject
    -> T.Text  -- ^ Body
    -> IO ()
sendEmail Handle {..} from to subject body =
    Amazonka.runResourceT $ void $ Amazonka.send hAwsEnv $ SES.newSendEmail
        from
        (SES.newDestination & SES.destination_toAddresses .~ Just [to])
        (SES.newMessage
            (SES.newContent subject)
            (SES.newBody & SES.body_text .~ Just (SES.newContent body)))
