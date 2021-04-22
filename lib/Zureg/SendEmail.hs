-- | Sending an email.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.SendEmail
    ( Config (..)
    , Handle
    , withHandle
    , sendEmail
    ) where

import           Control.Lens           ((&), (.~))
import           Control.Monad          (void)
import qualified Data.Aeson.TH.Extended as A
import qualified Data.Text              as T
import qualified Network.AWS.Extended   as Aws
import qualified Network.AWS.SES        as SES

data Config = Config
    { cFrom :: !T.Text
    }

data Handle = Handle
    { hConfig :: !Config
    , hAwsEnv :: !Aws.Env
    }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle hConfig f = do
    hAwsEnv <- Aws.smartEnv
    f Handle {..}

sendEmail
    :: Handle
    -> T.Text  -- ^ To
    -> T.Text  -- ^ Subject
    -> T.Text  -- ^ Body
    -> IO ()
sendEmail Handle {..} to subject body =
    Aws.runResourceT $ Aws.runAWS hAwsEnv $ void $ Aws.send $ SES.sendEmail
        (cFrom hConfig)
        (SES.destination & SES.dToAddresses .~ [to])
        (SES.message
            (SES.content subject)
            (SES.body & SES.bText .~ Just (SES.content body)))

$(A.deriveJSON A.options ''Config)
