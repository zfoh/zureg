module Zureg.Captcha.NoCaptcha
    ( module Zureg.Captcha
    , new
    ) where

import           Zureg.Captcha

new :: IO Handle
new = pure Handle
    { clientHtml = ClientHtml mempty mempty
    , verify     = \_ _ -> pure ()
    }
