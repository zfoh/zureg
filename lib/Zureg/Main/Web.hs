{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Zureg.Main.Web
    ( main
    , app
    ) where

import           Control.Applicative       (liftA2)
import           Control.Exception         (throwIO)
import           Control.Monad             (join, unless, when)
import qualified Data.Aeson                as A
import           Data.Maybe                (isNothing)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Lazy.Encoding   as TL
import qualified Data.Time                 as Time
import qualified Eventful                  as E
import qualified Network.HTTP.Client       as Http
import qualified Network.HTTP.Client.TLS   as Http
import qualified Network.HTTP.Types        as Http
import qualified Network.Wai               as Wai
import qualified Network.Wai.Handler.Warp  as Warp
import qualified Text.Digestive            as D
import qualified Zureg.Captcha             as Captcha
import qualified Zureg.Database            as Database
import           Zureg.Form
import qualified Zureg.Hackathon           as Hackathon
import           Zureg.Hackathon           (Hackathon)
import           Zureg.Http
import           Zureg.Model
import qualified Zureg.SendEmail           as SendEmail
import           Zureg.SendEmail.Hardcoded
import qualified Zureg.Views               as Views

main :: forall a. (A.FromJSON a, A.ToJSON a) => Hackathon a -> IO ()
main hackathon = app hackathon >>= Warp.run 8000

app :: forall a. (A.FromJSON a, A.ToJSON a) => Hackathon a -> IO Wai.Application
app hackathon =
    fmap httpExceptionMiddleware $
    Http.newManager Http.tlsManagerSettings >>= \httpManager ->
    Database.withHandle (Hackathon.databaseConfig hackathon) $ \db ->
    SendEmail.withHandle (Hackathon.sendEmailConfig hackathon) $ \sendEmail ->
    pure $ \req respond -> case Wai.pathInfo req of
        ["register"] -> do
            reqBody <- TL.decodeUtf8 <$> Wai.strictRequestBody req
            when (Wai.requestMethod req == Http.methodPost) $ Captcha.verify
                (Hackathon.captcha hackathon)
                httpManager
                (Just reqBody)
            (view, mbReg) <- runForm req reqBody "register" $ D.checkM
                "Email address already registered"
                (fmap isNothing . Database.lookupEmail db . riEmail . fst)
                (liftA2 (,)
                    (registerForm hackathon)
                    (Hackathon.registerForm hackathon))
            registrantsSummary <- Database.lookupRegistrantsSummary db
            let atCapacity = Database.rsAvailable registrantsSummary <= 0
            case mbReg of
                Nothing -> respond . html $ Views.register
                    hackathon
                    (Captcha.clientHtml $ Hackathon.captcha hackathon)
                    view

                Just (info, additionalInfo) | atCapacity -> do
                    -- You're on the waitlist
                    uuid <- E.uuidNextRandom
                    time <- Time.getCurrentTime
                    let wlinfo = WaitlistInfo time
                    Database.writeEvents db uuid
                        [Register info additionalInfo, Waitlist wlinfo]
                    Database.putEmail db (riEmail info) uuid
                    sendWaitlistEmail sendEmail hackathon info uuid
                    respond . html $ Views.registerWaitlist uuid info
                Just (info, additionalInfo) -> do
                    -- Success registration
                    uuid <- E.uuidNextRandom
                    Database.writeEvents db uuid [Register info additionalInfo]
                    Database.putEmail db (riEmail info) uuid
                    sendRegisterSuccessEmail sendEmail hackathon info uuid
                    respond . html $ Views.registerSuccess uuid info

        ["ticket"] | Wai.requestMethod req == Http.methodGet -> do
            uuid <- getUuidParam req
            registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
            respond . html $ Views.ticket hackathon registrant

        ["scanner"] | Wai.requestMethod req == Http.methodGet  ->
            scannerAuthorized req $
            respond . html $ Views.scanner

        ["scan"] | Wai.requestMethod req == Http.methodGet ->
            scannerAuthorized req $ do
                time <- Time.getCurrentTime
                uuid <- getUuidParam req
                registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
                Database.writeEvents db uuid [Scan $ ScanInfo time :: Event a]
                respond . html $ Views.scan hackathon registrant

        ["chat"] -> do
            time <- Time.getCurrentTime
            uuid <- getUuidParam req
            registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
            unless (registrantCanJoinChat $ rState registrant) $ throwIO $
                HttpException 400
                "Invalid registrant state"

            url <- Hackathon.chatUrl hackathon
            Database.writeEvents db uuid
                [JoinChat $ JoinChatInfo time :: Event a]
            respond $ redirect url

        ["confirm"] | Hackathon.confirmation hackathon -> do
            uuid <- getUuidParam req
            registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
            case rState registrant of
              Just Registered -> Database.writeEvents db uuid [Confirm :: Event a]
              _               -> return ()
            respond . redirect $ "ticket?uuid=" <> E.uuidToText uuid

        ["spam"] | Wai.requestMethod req == Http.methodPost -> do
            uuid <- getUuidParam req
            _    <- Database.getRegistrant db uuid :: IO (Registrant a)
            Database.writeEvents db uuid [MarkSpam :: Event a]
            respond . redirect $ "ticket?uuid=" <> E.uuidToText uuid

        ["cancel"] -> do
            reqBody <- TL.decodeUtf8 <$> Wai.strictRequestBody req
            (view, mbCancel) <- runForm req reqBody "cancel" $
                cancelForm (lookupUuidParam req)
            case mbCancel of
                Just (uuid, True) -> do
                    registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
                    -- TODO: Check that not yet cancelled?
                    Database.writeEvents db uuid [Cancel :: Event a]
                    case rInfo registrant of
                        Nothing   -> return ()
                        Just info ->  Database.deleteEmail db $ riEmail info
                    respond . html $ Views.cancelSuccess
                _ -> respond . html $
                    Views.cancel (lookupUuidParam req) view

        pathInfo -> throwIO $ HttpException 404 $
            T.unpack (T.intercalate "/" pathInfo) ++ " not found"
  where
    textParam k req = fmap T.decodeUtf8 . join . lookup k $ Wai.queryString req

    lookupUuidParam :: Wai.Request -> Maybe E.UUID
    lookupUuidParam = (>>= E.uuidFromText) . textParam "uuid"

    getUuidParam :: Wai.Request -> IO E.UUID
    getUuidParam req = maybe
        (throwIO $ HttpException 400 "Missing uuid")
        return
        (lookupUuidParam req)

    scannerAuthorized request m = case textParam "secret" request of
        Just s | s == Hackathon.scannerSecret hackathon -> m
        _                                               -> throwIO $
            HttpException 403
            "Wrong or missing secret for scanner access"
