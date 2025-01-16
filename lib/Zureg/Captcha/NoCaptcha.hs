module Zureg.Captcha.NoCaptcha
    ( new
    ) where

import           Zureg.Captcha.Internal

new :: IO Handle
new = pure Handle
    { clientHtml = ClientHtml mempty mempty
    , verify     = \_ _ -> pure ()
    }
