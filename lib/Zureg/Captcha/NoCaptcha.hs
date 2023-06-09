module Zureg.Captcha.NoCaptcha
    ( module Zureg.Captcha
    , new
    ) where

import           Zureg.Captcha

new :: Handle
new = Handle
    { clientHtml = ClientHtml mempty mempty
    , verify     = \_ _ -> pure ()
    }
