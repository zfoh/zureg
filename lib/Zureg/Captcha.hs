{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Captcha
    ( Config (..)
    , new
    , withHandle

    , CaptchaException (..)
    , Handle (..)
    , ClientHtml (..)
    ) where

import qualified Data.Aeson              as A
import qualified Data.Aeson.TH.Extended  as A
import qualified Data.Text               as T
import qualified Zureg.Captcha.HCaptcha  as HCaptcha
import           Zureg.Captcha.Internal
import qualified Zureg.Captcha.NoCaptcha as NoCaptcha
import qualified Zureg.Captcha.ReCaptcha as ReCaptcha

data Config
    = HCaptchaConfig HCaptcha.Config
    | ReCaptchaConfig ReCaptcha.Config
    | NoCaptchaConfig
    deriving (Show)

data ProbeType = ProbeType
    { ptType :: !T.Text
    }

new :: Config -> IO Handle
new (HCaptchaConfig c)  = HCaptcha.new c
new (ReCaptchaConfig c) = ReCaptcha.new c
new NoCaptchaConfig     = NoCaptcha.new

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle = (>>=) . new

$(A.deriveJSON A.options ''ProbeType)

instance A.FromJSON Config where
    parseJSON v = do
        probe <- A.parseJSON v
        case ptType probe of
            "off"       -> pure NoCaptchaConfig
            "hcaptcha"  -> HCaptchaConfig <$> A.parseJSON v
            "recaptcha" -> ReCaptchaConfig <$> A.parseJSON v
            ty          -> fail $ "unknown Captcha type: " ++ T.unpack ty
