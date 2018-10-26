{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.Main.Lambda
    ( main
    ) where

import           Control.Exception             (throwIO)
import           Control.Monad                 (when)
import           Data.Maybe                    (isNothing)
import qualified Data.Text                     as T
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
import           Zureg.Model
import qualified Zureg.ReCaptcha               as ReCaptcha
import qualified Zureg.Serverless              as Serverless
import qualified Zureg.Views                   as Views

html :: H.Html -> IO Serverless.Response
html = return .  Serverless.responseHtml .
    Serverless.response200 . RenderHtml.renderHtml

main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs

    config <- case args of
        [configPath] -> Config.load configPath
        _            -> do
            IO.hPutStr IO.stderr $ "Usage: " ++ progName ++ " config.json"
            exitFailure

    dbConfig <- Config.section config "database"
    rcConfig <- Config.section config "recaptcha"

    Database.withHandle dbConfig $ \db ->
        ReCaptcha.withHandle rcConfig $ \recaptcha ->
        Serverless.main IO.stdin IO.stdout $ \req@Serverless.Request {..} ->
        case Serverless.requestPath req of
            ["register"] -> do
                when (reqHttpMethod == "POST") $
                    ReCaptcha.verify recaptcha (Serverless.reqBody req)
                (view, mbReg) <- Serverless.runForm req "register" $ D.checkM
                    "Email address already registered"
                    (fmap isNothing . Database.lookupEmail db . riEmail)
                    registerForm

                case mbReg of
                    Nothing -> html $
                        Views.register (ReCaptcha.clientHtml recaptcha) view
                    Just info -> do
                        uuid <- E.uuidNextRandom
                        Database.writeEvents db uuid [Register info]
                        Database.putEmail db (riEmail info) uuid
                        html $ Views.registerSuccess uuid info

            ["ticket"] | reqHttpMethod == "GET" -> do
                uuid <- getUuidParam req
                registrant <- Database.getRegistrant db uuid
                html $ Views.ticket registrant

            ["scanner"] | reqHttpMethod == "GET" -> html $ Views.scanner

            ["scan"] | reqHttpMethod == "GET" -> do
                uuid <- getUuidParam req
                registrant <- Database.getRegistrant db uuid
                html $ Views.scan registrant

            ["cancel"] -> do
                (view, mbCancel) <- Serverless.runForm req "cancel" $
                    cancelForm (lookupUuidParam req)
                case mbCancel of
                    Just (uuid, True) -> do
                        -- TODO: Check that not yet cancelled?
                        Database.writeEvents db uuid [Cancel]
                        html Views.cancelSuccess
                    _ -> html $
                        Views.cancel view

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
