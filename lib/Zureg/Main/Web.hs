{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Zureg.Main.Web
    ( main
    ) where

import           Control.Applicative                 (liftA2)
import           Control.Exception                   (throwIO)
import           Control.Monad                       (unless, when)
import qualified Data.Aeson                          as A
import           Data.Maybe                          (isNothing)
import qualified Data.Text                           as T
import qualified Data.Time                           as Time
import qualified Eventful                            as E
import qualified System.IO                           as IO
import qualified Text.Blaze.Html.Renderer.Text       as RenderHtml
import qualified Text.Blaze.Html5                    as H
import qualified Text.Digestive                      as D
import qualified Zureg.Database                      as Database
import           Zureg.Form
import           Zureg.Hackathon                     (Hackathon)
import qualified Zureg.Hackathon                     as Hackathon
import           Zureg.Model
import qualified Zureg.ReCaptcha                     as ReCaptcha
import qualified Zureg.SendEmail                     as SendEmail
import           Zureg.SendEmail.Hardcoded
import qualified Zureg.Serverless                    as Serverless
import qualified Zureg.Views                         as Views

html :: H.Html -> IO Serverless.Response
html = return .  Serverless.responseHtml .
    Serverless.response200 . RenderHtml.renderHtml

main :: forall a. (A.FromJSON a, A.ToJSON a) => Hackathon a -> IO ()
main hackathon =
    Database.withHandle (Hackathon.databaseConfig hackathon) $ \db ->
    ReCaptcha.withHandle (Hackathon.reCaptchaConfig hackathon) $ \recaptcha ->
    SendEmail.withHandle (Hackathon.sendEmailConfig hackathon) $ \sendEmail ->
    Serverless.main IO.stdin IO.stdout $ \req@Serverless.Request {..} ->
        case Serverless.requestPath req of
            ["register"] -> do
                when (reqHttpMethod == "POST") $
                    ReCaptcha.verify recaptcha (Serverless.reqBody req)
                (view, mbReg) <- Serverless.runForm req "register" $ D.checkM
                    "Email address already registered"
                    (fmap isNothing . Database.lookupEmail db . riEmail . fst)
                    (liftA2 (,)
                        (registerForm hackathon)
                        (Hackathon.registerForm hackathon))
                registrantsSummary <- Database.lookupRegistrantsSummary db
                let atCapacity = Database.rsAvailable registrantsSummary <= 0
                case mbReg of
                    Nothing -> html $
                        Views.register hackathon (ReCaptcha.clientHtml recaptcha) view

                    Just (info, additionalInfo) | atCapacity -> do
                        -- You're on the waitlist
                        uuid <- E.uuidNextRandom
                        time <- Time.getCurrentTime
                        let wlinfo = WaitlistInfo time
                        Database.writeEvents db uuid
                            [Register info additionalInfo, Waitlist wlinfo]
                        Database.putEmail db (riEmail info) uuid
                        sendWaitlistEmail sendEmail hackathon info uuid
                        html $ Views.registerWaitlist uuid info
                    Just (info, additionalInfo) -> do
                        -- Success registration
                        uuid <- E.uuidNextRandom
                        Database.writeEvents db uuid [Register info additionalInfo]
                        Database.putEmail db (riEmail info) uuid
                        sendRegisterSuccessEmail sendEmail hackathon info uuid
                        html $ Views.registerSuccess uuid info

            ["ticket"] | reqHttpMethod == "GET" -> do
                uuid <- getUuidParam req
                registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
                html $ Views.ticket hackathon registrant

            ["scanner"] | reqHttpMethod == "GET" ->
                scannerAuthorized req $
                html $ Views.scanner

            ["scan"] | reqHttpMethod == "GET" ->
                scannerAuthorized req $ do
                    time <- Time.getCurrentTime
                    uuid <- getUuidParam req
                    registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
                    Database.writeEvents db uuid [Scan $ ScanInfo time :: Event a]
                    html $ Views.scan hackathon registrant

            ["chat"] -> do
                time <- Time.getCurrentTime
                uuid <- getUuidParam req
                registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
                Database.writeEvents db uuid [Scan $ ScanInfo time :: Event a]
                unless (registrantCanJoinChat $ rState registrant) $ throwIO $
                    Serverless.ServerlessException 400
                    "Invalid registrant state"

                url <- Hackathon.chatUrl hackathon
                Database.writeEvents db uuid
                    [JoinChat $ JoinChatInfo time :: Event a]
                return $ Serverless.response302 url

            ["confirm"] | Hackathon.confirmation hackathon -> do
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

    scannerAuthorized request m =
        case Serverless.requestLookupQueryStringParameter "secret" request of
            Just s | s == Hackathon.scannerSecret hackathon -> m
            _                                               -> throwIO $
                Serverless.ServerlessException 403
                "Wrong or missing secret for scanner access"
