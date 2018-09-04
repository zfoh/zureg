-- | Registration and cancellation forms using the `digestive-functors`
-- library.
{-# LANGUAGE OverloadedStrings #-}
module Zureg.Form
    ( registerForm
    , registerView

    , cancelForm
    , cancelView
    ) where

import           Data.Maybe                  (isNothing)
import qualified Data.Text                   as T
import qualified Eventful                    as E
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive              as D
import qualified Text.Digestive.Blaze.Html5  as DH
import           Zureg.Model
import qualified Zureg.ReCaptcha             as ReCaptcha

registerForm :: Monad m => D.Form H.Html m RegisterInfo
registerForm = tshirtCheck $ confirmEmailCheck $ RegisterInfo
    <$> "name" D..: D.text Nothing
    <*> "badge1" D..: optionalText
    <*> "badge2" D..: optionalText
    <*> "email" D..: simpleEmailCheck (T.strip <$> D.text Nothing)
    <*> "confirmEmail" D..: (T.strip <$> D.text Nothing)
    <*> "publicParticipation" D..: D.choice
            [(False, "No"), (True, "Yes")] Nothing
    <*> "tshirtModel" D..: D.choice
            [ (Just Female, "Female")
            , (Just Male,   "Male")
            , (Nothing,     "I don't want a T-Shirt")
            ] Nothing
    <*> "tshirtSize" D..: D.choice (
            [(Just s, H.toHtml $ show s) | s <- [minBound .. maxBound]] ++
            [(Nothing, "I don't want a T-Shirt")])
            Nothing

  where
    simpleEmailCheck = D.check "Invalid email address" $ \email ->
        case T.split (== '@') email of
            [_user, domain] -> T.count "." domain >= 1
            _               -> False

    confirmEmailCheck = D.check "Email confirmation doesn't match" $ \ri ->
        riEmail ri == riEmailConfirm ri

    tshirtCheck =
        D.check "Fill in both T-Shirt model and size or neither of the two" $
        \ri -> isNothing (riTShirtModel ri) == isNothing (riTShirtSize ri)

    optionalText =
        (\t -> let t' = T.strip t in if T.null t' then Nothing else Just t') <$>
        (D.text Nothing)

registerView :: ReCaptcha.ClientHtml -> D.View H.Html -> H.Html
registerView recaptcha view = DH.form view "?" $ do
    H.h1 "ZuriHac registration"
    DH.childErrorList "" view

    H.h2 "About you"

    DH.label "name" view "Name:"
    DH.inputText "name" view
    H.br

    DH.label "badge1" view "Name badge line 1 (e.g. affiliation, employer):"
    DH.inputText "badge1" view
    H.br

    DH.label "badge2" view
        "Name badge line 2 (e.g. nickname, \"ask me about lazy evaluation\"):"
    DH.inputText "badge2" view
    H.br

    DH.label "email" view "Email address:"
    DH.inputText "email" view
    H.br

    DH.label "confirmEmail" view "Confirm your email address:"
    DH.inputText "confirmEmail" view
    H.br

    DH.label "publicParticipation" view
        "Can we publish your name on a public list of attendees of this event?"
    DH.inputSelect "publicParticipation" view
    H.br

    H.h2 "T-Shirt"

    DH.label "tshirtModel" view "T-Shirt model:"
    DH.inputSelect "tshirtModel" view
    H.br

    DH.label "tshirtSize" view "T-Shirt size: (TODO link sizing chart)"
    DH.inputSelect "tshirtSize" view
    H.br

    ReCaptcha.chForm recaptcha

    DH.inputSubmit "Register"

cancelForm :: Monad m => Maybe E.UUID -> D.Form H.Html m (E.UUID, Bool)
cancelForm uuid = (,)
    <$> "uuid" D..: D.validate
            validateUuid (D.text (fmap E.uuidToText uuid))
    <*> "confirm" D..: D.bool Nothing

cancelView :: D.View H.Html -> H.Html
cancelView view = DH.form view "cancel?" $ do
    DH.childErrorList "" view
    DH.inputCheckbox "confirm" view H.! A.class_ "checkbox"
    DH.label         "confirm" view "I am sure"
    DH.inputText "uuid" view H.! A.style "display: none"
    DH.inputSubmit "Cancel"

validateUuid :: T.Text -> D.Result H.Html E.UUID
validateUuid txt = case E.uuidFromText txt of
    Nothing -> D.Error "Not a valid UUID"
    Just u  -> D.Success u
