-- | Registration and cancellation forms using the `digestive-functors`
-- library.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.Form
    ( registerForm
    , registerView

    , cancelForm
    , cancelView
    ) where

import           Control.Monad               (when)
import qualified Data.Text                   as T
import qualified Data.Time                   as Time
import qualified Eventful                    as E
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive              as D
import qualified Text.Digestive.Blaze.Html5  as DH
import qualified Zureg.Captcha               as Captcha
import           Zureg.Hackathon             (Hackathon)
import qualified Zureg.Hackathon             as Hackathon
import           Zureg.Model

-- | The 'IO' in this type signature is because we want to get the registration
-- time.
registerForm :: Hackathon a -> D.Form H.Html IO RegisterInfo
registerForm h = RegisterInfo
    <$> "name" D..: (D.check "Name is required"
            (not . T.null . T.strip)
            (D.text Nothing))
    <*> (D.validate confirmEmailCheck $ (,)
            <$> "email" D..: simpleEmailCheck (T.strip <$> D.text Nothing)
            <*> "confirmEmail" D..: (T.strip <$> D.text Nothing))
    <*> D.monadic (Time.getCurrentTime >>= return . pure)
  where
    simpleEmailCheck = D.check "Invalid email address" $ \email ->
        case T.split (== '@') email of
            [_user, domain] -> T.count "." domain >= 1
            _               -> False

    confirmEmailCheck (x, y)
        | x == y    = D.Success x
        | otherwise = D.Error "Email confirmation doesn't match"

registerView :: Hackathon a -> Captcha.ClientHtml -> D.View H.Html -> H.Html
registerView h captchaHtml view = DH.form view "?" $ do
    H.h1 $ H.toHtml (Hackathon.name h) <> " registration"
    H.div H.! A.class_ "errors" $ DH.childErrorList "" view

    H.h2 $ "Basic information"
    DH.label "name" view $ H.strong "Full name"
    DH.inputText "name" view
    H.br

    DH.label "email" view $ H.strong "Email"
    H.p $ do
        "We will only use your email address to send you your ticket, as well "
        "as information about the event.  It is not shared with any other "
        "parties."
    DH.inputText "email" view
    H.br

    DH.label "confirmEmail" view $ H.strong "Confirm your email"
    H.p "We want to be sure we that we can email you your ticket."
    DH.inputText "confirmEmail" view
    H.br

    Hackathon.registerView h view
    H.br

    H.h2 $ "Captcha (sorry)"
    Captcha.chForm captchaHtml
    H.br

    DH.inputSubmit "Register"

cancelForm :: Monad m => Maybe E.UUID -> D.Form H.Html m (E.UUID, Bool)
cancelForm uuid = (,)
    <$> "uuid" D..: D.validate
            validateUuid (D.text (fmap E.uuidToText uuid))
    <*> "confirm" D..: D.bool Nothing

cancelView :: Maybe E.UUID -> D.View H.Html -> H.Html
cancelView mbUuid view = do
    DH.form view "cancel?" $ do
        DH.childErrorList "" view
        DH.inputCheckbox "confirm" view H.! A.class_ "checkbox"
        DH.label         "confirm" view "I am sure"
        DH.inputText "uuid" view H.! A.style "display: none"
        DH.inputSubmit "Cancel Registration"

    -- Simple button that acts like a "back to ticket".  That is why we need the
    -- UUID here.
    H.form H.! A.method "GET" H.! A.action "ticket" $ do
        H.input
            H.! A.style "display: none"
            H.! A.type_ "text"
            H.! A.name "uuid"
            H.! A.value (maybe "" (H.toValue . E.uuidToText) mbUuid)
        H.input H.! A.type_ "submit"
            H.! A.value "Take me back to my ticket"

validateUuid :: T.Text -> D.Result H.Html E.UUID
validateUuid txt = case E.uuidFromText txt of
    Nothing -> D.Error "Not a valid UUID"
    Just u  -> D.Success u
