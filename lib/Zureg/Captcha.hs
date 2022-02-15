{-# LANGUAGE OverloadedStrings #-}
module Zureg.Captcha
    ( CaptchaException (..)
    , Handle (..)
    , ClientHtml (..)
    ) where

import           Control.Exception   (Exception)
import qualified Data.Text           as T
import qualified Network.HTTP.Client as Http
import qualified Text.Blaze.Html5    as H

data CaptchaException
    = VerificationFailed [T.Text]

instance Show CaptchaException where
    show (VerificationFailed []) = "Captcha verification failed"
    show (VerificationFailed es) =
        "Captcha verification failed, error code: " ++
        T.unpack (T.intercalate ", " es)

instance Exception CaptchaException

data ClientHtml = ClientHtml
    { chScript :: !H.Html
    , chForm   :: !H.Html
    }

data Handle = Handle
    { clientHtml :: ClientHtml
    , verify     :: Http.Manager -> Maybe T.Text -> IO ()
    }
