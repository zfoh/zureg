{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Zureg.Main.Web
    ( main
    , app
    ) where

import           Control.Concurrent                  (forkIO)
import           Control.Exception                   (throwIO)
import           Control.Monad                       (join, unless, void, when)
import           Data.Foldable                       (for_)
import           Data.Maybe                          (isJust)
import           Data.String                         (IsString (fromString))
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import qualified Data.Text.Lazy.Encoding             as TL
import           Data.UUID                           (UUID)
import qualified Data.UUID                           as UUID
import qualified Network.HTTP.Client                 as Http
import qualified Network.HTTP.Client.TLS             as Http
import qualified Network.HTTP.Types                  as Http
import qualified Network.Wai                         as Wai
import qualified Network.Wai.Handler.Warp            as Warp
import qualified System.IO                           as IO
import qualified Zureg.Captcha                       as Captcha
import qualified Zureg.Config                        as Config
import qualified Zureg.Database                      as Database
import           Zureg.Database.Models
import           Zureg.Form
import qualified Zureg.Hackathon                     as Hackathon
import qualified Zureg.Hackathon.ZuriHac2020.Discord as Discord
import           Zureg.Http
import           Zureg.Main.Janitor                  (popWaitlist)
import qualified Zureg.SendEmail                     as SendEmail
import           Zureg.SendEmail.Hardcoded
import qualified Zureg.Views                         as Views
import qualified Zureg.Web                           as Web

main :: IO ()
main = do
    config@Config.Config {..} <- Config.load
    IO.hPutStrLn IO.stderr $
        "Listening on " ++ T.unpack (Web.configHost configWeb) ++ ":" ++
        show (Web.configPort configWeb)
    Database.withHandle configDatabase $ \db -> do
        Database.migrate db
        let settings = Warp.setPort (Web.configPort configWeb) $
                Warp.setHost (fromString $ T.unpack $ Web.configHost configWeb) $
                Warp.setGracefulShutdownTimeout (Just 5) $
                Warp.defaultSettings
        app config db >>= Warp.runSettings settings

app :: Config.Config -> Database.Handle -> IO Wai.Application
app Config.Config {..} db =
    fmap httpExceptionMiddleware $
    Http.newManager Http.tlsManagerSettings >>= \httpManager ->
    SendEmail.withHandle configAws $ \sendEmail ->
    Captcha.withHandle configCaptcha $ \captcha ->
    pure $ \req respond -> case Wai.pathInfo req of
        ["register"] -> do
            reqBody <- TL.decodeUtf8 <$> Wai.strictRequestBody req
            when (Wai.requestMethod req == Http.methodPost) $ Captcha.verify
                captcha
                httpManager
                (Just reqBody)
            (view, mbReg) <- runForm req reqBody "register" registerForm
            case mbReg of
                Nothing -> respond . html $ Views.register
                    configHackathon
                    (Captcha.clientHtml captcha)
                    view
                Just (insert, mbProject) -> do
                    (registration, atCapacity) <- Database.withTransaction db $ \tx -> do
                        alreadyRegistered <- Database.selectRegistrationByEmail tx (irEmail insert)
                        when (isJust alreadyRegistered) $ throwIO $
                            HttpException 400
                            "email address already registered"
                        attending <- Database.selectAttending tx
                        let atCapacity = attending >= Hackathon.capacity configHackathon
                        registration <- Database.insertRegistration tx insert
                        registration' <- Database.setRegistrationState tx
                            (rUuid registration)
                            (if atCapacity then Waitlisted else Registered)
                        for_ mbProject $ \project -> Database.insertProject
                            tx (rUuid registration) project
                        pure (registration', atCapacity)

                    if atCapacity then do
                        -- You're on the waitlist
                        sendWaitlistEmail sendEmail configHackathon registration
                        respond . html $ Views.registerWaitlist registration
                    else do
                        -- Success registration
                        sendRegisterSuccessEmail
                            sendEmail configHackathon registration
                        respond . html $ Views.registerSuccess registration

        ["ticket"] | Wai.requestMethod req == Http.methodGet -> do
            uuid <- getUuidParam req
            mbRegistration <- Database.withTransaction db $ \tx ->
                Database.selectRegistration tx uuid
            case mbRegistration of
                Nothing -> throwIO $ HttpException 404 "registration not found"
                Just registration -> respond . html $
                    Views.ticket configHackathon registration

        ["scanner"] | Wai.requestMethod req == Http.methodGet  ->
            scannerAuthorized req $
            respond . html $ Views.scanner

        ["scan"] | Wai.requestMethod req == Http.methodGet ->
            scannerAuthorized req $ do
                uuid <- getUuidParam req
                registrant <- Database.withTransaction db $ \tx ->
                    Database.setRegistrationScanned tx uuid
                respond . html $ Views.scan configHackathon registrant

        ["chat"] -> do
            uuid <- getUuidParam req
            registration <- Database.withTransaction db $ \tx ->
                Database.selectRegistration tx uuid >>=
                maybe (throwIO $ HttpException 404 "registration not found") pure
            unless (registrantCanJoinChat $ rState registration) $ throwIO $
                HttpException 400
                "Invalid registrant state"

            welcomeChannel <- Discord.getWelcomeChannelId configDiscord
            url <- Discord.generateTempInviteUrl configDiscord welcomeChannel
            respond $ redirect url

        ["confirm"] | Hackathon.confirmation configHackathon -> do
            uuid <- getUuidParam req
            Database.withTransaction db $ \tx -> do
                registrant <- Database.selectRegistration tx uuid
                case rState <$> registrant of
                    Just Registered -> void $
                      Database.setRegistrationState tx uuid Confirmed
                    _ -> pure ()
            respond . redirect $ "ticket?uuid=" <> UUID.toText uuid

        ["cancel"] -> do
            reqBody <- TL.decodeUtf8 <$> Wai.strictRequestBody req
            (view, mbCancel) <- runForm req reqBody "cancel" $
                cancelForm (lookupUuidParam req)
            case mbCancel of
                Just (uuid, True) -> do
                    -- TODO: Check that not yet cancelled?
                    _ <- Database.withTransaction db $ \tx ->
                        Database.setRegistrationState tx uuid Cancelled
                    -- Pop waitlist in background
                    _ <- forkIO $ popWaitlist db sendEmail configHackathon
                    respond . html $ Views.cancelSuccess
                _ -> respond . html $
                    Views.cancel (lookupUuidParam req) view

        pathInfo -> throwIO $ HttpException 404 $
            T.unpack (T.intercalate "/" pathInfo) ++ " not found"
  where
    textParam k req = fmap T.decodeUtf8 . join . lookup k $ Wai.queryString req

    lookupUuidParam :: Wai.Request -> Maybe UUID
    lookupUuidParam = (>>= UUID.fromText) . textParam "uuid"

    getUuidParam :: Wai.Request -> IO UUID
    getUuidParam req = maybe
        (throwIO $ HttpException 400 "Missing uuid")
        return
        (lookupUuidParam req)

    scannerAuthorized request m = case textParam "secret" request of
        Just s | s == configScannerSecret -> m
        _                                 -> throwIO $
            HttpException 403
            "Wrong or missing secret for scanner access"
