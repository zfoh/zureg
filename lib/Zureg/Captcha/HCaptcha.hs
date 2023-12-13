-- | Very simple bindings to hcaptcha, adapted from the HCaptcha module.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Captcha.HCaptcha
    ( module Zureg.Captcha
    , Config (..)
    , configFromEnv
    , new
    ) where

import           Control.Exception           (throwIO)
import qualified Data.Aeson                  as A
import qualified Data.Aeson.TH.Extended      as A
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.Lazy              as TL
import qualified Data.URLEncoded             as UrlEncoded
import qualified Network.HTTP.Client         as Http
import           System.Environment          (getEnv)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Zureg.Captcha

data Config = Config
    { cSiteKey   :: !T.Text
    , cSecretKey :: !T.Text
    } deriving (Show)

configFromEnv :: IO Config
configFromEnv = Config
    <$> (T.pack <$> getEnv "ZUREG_HCAPTCHA_SITEKEY")
    <*> (T.pack <$> getEnv "ZUREG_HCAPTCHA_SECRET")

new :: Config -> IO Handle
new Config {..} = pure Handle
    { clientHtml = ClientHtml
        (H.script H.! A.src "https://js.hcaptcha.com/1/api.js"
            H.! A.async "async"
            H.! A.defer "defer" $ "")
        (H.div H.! A.class_ "h-captcha"
            H.! H.dataAttribute "sitekey" (H.toValue cSiteKey) $ "")

    , verify = \httpManager mbRequestBody -> do
        requestBody <- maybe bail return mbRequestBody
        params <- UrlEncoded.importString (TL.unpack requestBody)
        param <- maybe bail return (UrlEncoded.lookup paramName params)
        request0 <- Http.parseRequest "https://hcaptcha.com/siteverify"

        let request1 = Http.urlEncodedBody
                [ ("secret",   T.encodeUtf8 cSecretKey)
                , ("response", T.encodeUtf8 (T.pack param))
                ]
                request0

        response <- Http.httpLbs request1 httpManager
        case A.eitherDecode' (Http.responseBody response) of
            Right x | arSuccess x -> return ()
            Left _                -> throwIO $ VerificationFailed []
            Right x               -> throwIO $
                VerificationFailed $ arErrorCodes x
    }
  where
    bail      = throwIO $ VerificationFailed []
    paramName = "h-captcha-response" :: String

data ApiResponse = ApiResponse
    { arSuccess    :: !Bool
    , _arHostname  :: !T.Text
    , arErrorCodes :: [T.Text]
    }

instance A.FromJSON ApiResponse where
    parseJSON = A.withObject "FromJSON ApiResponse" $ \o -> ApiResponse
        <$> o A..:  "success"
        <*> o A..:  "hostname"
        <*> o A..:? "error-code" A..!= []

$(A.deriveJSON A.options ''Config)
