-- | Very simple bindings to google recaptcha.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.ReCaptcha
    ( Config (..)
    , defaultConfig

    , Handle (..)
    , withHandle

    , ClientHtml (..)
    , clientHtml

    , verify
    ) where

import           Control.Exception           (Exception, throwIO)
import qualified Data.Aeson                  as A
import qualified Data.FileEmbed              as Embed
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.URLEncoded             as UrlEncoded
import qualified Network.HTTP.Client         as Http
import qualified Network.HTTP.Client.TLS     as Http
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data ReCaptchaException
    = VerificationFailed [T.Text]

instance Show ReCaptchaException where
    show (VerificationFailed []) = "ReCaptcha verification failed"
    show (VerificationFailed es) =
        "ReCaptcha verification failed, error code: " ++
        T.unpack (T.intercalate ", " es)

instance Exception ReCaptchaException

data Config = Config
    { cEnabled   :: !Bool
    , cSiteKey   :: !T.Text
    , cSecretKey :: !T.Text
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { cEnabled   = True
    , cSiteKey   = "6LcVUm8UAAAAAL0ooPLkNT3O9oEXhGPK6kZ-hQk7"
    , cSecretKey = T.decodeUtf8 $(Embed.embedFile "deploy/recaptcha")
    }

data Handle = Handle
    { hConfig  :: !Config
    , hManager :: !Http.Manager
    }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle hConfig f = do
    hManager <- Http.newManager Http.tlsManagerSettings
    f Handle {..}

data ClientHtml = ClientHtml
    { chScript :: !H.Html
    , chForm   :: !H.Html
    }

clientHtml :: Handle -> ClientHtml
clientHtml Handle {..}
    | not (cEnabled hConfig) = ClientHtml mempty mempty
    | otherwise              = ClientHtml
        (H.script H.! A.src "https://www.google.com/recaptcha/api.js" $ "")
        (H.div H.! A.class_ "g-recaptcha"
            H.! H.dataAttribute "sitekey" (H.toValue $ cSiteKey hConfig) $ "")

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

verify :: Handle -> Maybe T.Text -> IO ()
verify Handle {..} _ | not (cEnabled hConfig) = return ()
verify Handle {..} mbRequestBody = do
    requestBody <- maybe bail return mbRequestBody
    params <- UrlEncoded.importString (T.unpack requestBody)
    param <- maybe bail return (UrlEncoded.lookup paramName params)
    request0 <- Http.parseRequest "https://www.google.com/recaptcha/api/siteverify"

    let request1 = Http.urlEncodedBody
            [ ("secret",   T.encodeUtf8 (cSecretKey hConfig))
            , ("response", T.encodeUtf8 (T.pack param))
            ]
            request0

    response <- Http.httpLbs request1 hManager
    case A.eitherDecode' (Http.responseBody response) of
        Right x | arSuccess x -> return ()
        Left _                -> throwIO $ VerificationFailed []
        Right x               -> throwIO $
            VerificationFailed $ arErrorCodes x
  where
    bail      = throwIO $ VerificationFailed []
    paramName = "g-recaptcha-response" :: String
