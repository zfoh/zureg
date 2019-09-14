{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Main.Lambda
    ( main
    , loadConfig
    ) where

import           Control.Applicative           (liftA2)
import           Control.Exception             (throwIO)
import           Control.Monad                 (when)
import qualified Data.Aeson                    as A
import qualified Data.Aeson.TH.Extended        as A
import           Data.Maybe                    (isNothing)
import qualified Data.Text                     as T
import qualified Data.Time                     as Time
import qualified Eventful                      as E
import           System.Environment            (getArgs, getProgName)
import           System.Exit                   (exitFailure)
import qualified System.IO                     as IO
import qualified Text.Blaze.Html.Renderer.Text as RenderHtml
import qualified Text.Blaze.Html5              as H
import qualified Text.Digestive                as D
import qualified Zureg.Config                  as Config
import qualified Zureg.Database                as Database
import           Zureg.Form
import qualified Zureg.Hackathon               as Hackathon
import           Zureg.Model
import qualified Zureg.ReCaptcha               as ReCaptcha
import qualified Zureg.SendEmail               as SendEmail
import           Zureg.SendEmail.Hardcoded
import qualified Zureg.Serverless              as Serverless
import qualified Zureg.Views                   as Views

data ScannerConfig = ScannerConfig
    { scSecret :: !T.Text
    } deriving (Show)

$(A.deriveJSON A.options ''ScannerConfig)

html :: H.Html -> IO Serverless.Response
html = return .  Serverless.responseHtml .
    Serverless.response200 . RenderHtml.renderHtml

loadConfig :: IO Config.Config
loadConfig = do
    progName <- getProgName
    args     <- getArgs
    case args of
        [configPath] -> Config.load configPath
        _            -> do
            IO.hPutStr IO.stderr $ "Usage: " ++ progName ++ " config.json"
            exitFailure

main :: forall a. (A.FromJSON a, A.ToJSON a)
     => Config.Config -> Hackathon.Handle a -> IO ()
main config hackathon = do
    dbConfig        <- Config.section "database" config
    rcConfig        <- Config.section "recaptcha" config
    emailConfig     <- Config.section "sendEmail" config
    scannerConfig   <- Config.section "scanner" config

    Database.withHandle dbConfig $ \db ->
        ReCaptcha.withHandle rcConfig $ \recaptcha ->
        SendEmail.withHandle emailConfig $ \sendEmail ->
        Serverless.main IO.stdin IO.stdout $ \req@Serverless.Request {..} ->
        case Serverless.requestPath req of
            ["register"] -> do
                when (reqHttpMethod == "POST") $
                    ReCaptcha.verify recaptcha (Serverless.reqBody req)
                (view, mbReg) <- Serverless.runForm req "register" $ D.checkM
                    "Email address already registered"
                    (fmap isNothing . Database.lookupEmail db . riEmail . fst)
                    (liftA2 (,)
                        registerForm
                        (Hackathon.hRegisterForm hackathon))

                case mbReg of
                    Nothing -> html $
                        Views.register hackathon (ReCaptcha.clientHtml recaptcha) view

                    Just (info, additionalInfo) | Hackathon.cWaitlist (Hackathon.hConfig hackathon) -> do
                        -- You're on the waitlist
                        uuid <- E.uuidNextRandom
                        time <- Time.getCurrentTime
                        let wlinfo = WaitlistInfo time
                        Database.writeEvents db uuid
                            [Register info additionalInfo, Waitlist wlinfo]
                        Database.putEmail db (riEmail info) uuid
                        sendWaitlistEmail sendEmail (Hackathon.hConfig hackathon) info uuid
                        html $ Views.registerWaitlist uuid info
                    Just (info, additionalInfo) -> do
                        -- Success registration
                        uuid <- E.uuidNextRandom
                        Database.writeEvents db uuid [Register info additionalInfo]
                        Database.putEmail db (riEmail info) uuid
                        sendRegisterSuccessEmail sendEmail (Hackathon.hConfig hackathon) info uuid
                        html $ Views.registerSuccess uuid info

            ["ticket"] | reqHttpMethod == "GET" -> do
                uuid <- getUuidParam req
                registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
                html $ Views.ticket hackathon registrant

            ["scanner"] | reqHttpMethod == "GET" ->
                scannerAuthorized req scannerConfig $
                html $ Views.scanner

            ["scan"] | reqHttpMethod == "GET" ->
                scannerAuthorized req scannerConfig $ do
                    time <- Time.getCurrentTime
                    uuid <- getUuidParam req
                    registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
                    Database.writeEvents db uuid [Scan $ ScanInfo time :: Event a]
                    html $ Views.scan hackathon registrant

            ["confirm"] -> do
                uuid <- getUuidParam req
                registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
                case rState registrant of 
                  Just Registered -> Database.writeEvents db uuid [Confirm :: Event a]
                  _               -> return ()
                return $ Serverless.response302 $ "ticket?uuid=" <> E.uuidToText uuid
                   
            ["cancel"] -> do
                (view, mbCancel) <- Serverless.runForm req "cancel" $
                    cancelForm (lookupUuidParam req)
                case mbCancel of
                    Just (uuid, True) -> do
                        registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
                        -- TODO: Check that not yet cancelled?
                        Database.writeEvents db uuid [Cancel :: Event a]
                        case rInfo registrant of
                            Nothing -> return ()
                            Just info ->  Database.deleteEmail db $ riEmail info
                        html Views.cancelSuccess
                    _ -> html $
                        Views.cancel (lookupUuidParam req) view

            _ -> throwIO $ Serverless.ServerlessException 404 $
                T.unpack reqPath ++ " not found"
  where
    lookupUuidParam :: Serverless.Request -> Maybe E.UUID
    lookupUuidParam =
        (>>= E.uuidFromText) .
        Serverless.requestLookupQueryStringParameter "uuid"

    getUuidParam :: Serverless.Request -> IO E.UUID
    getUuidParam req = maybe
        (throwIO $ Serverless.ServerlessException 400 "Missing uuid")
        return
        (lookupUuidParam req)

    scannerAuthorized request sc m =
        case Serverless.requestLookupQueryStringParameter "secret" request of
            Just s | s == scSecret sc -> m
            _                         -> throwIO $
                Serverless.ServerlessException 403
                "Wrong or missing secret for scanner access"
