-- | Registration and cancellation forms using the `digestive-functors`
-- library.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.Form
    ( ProjectForm (..)
    , registerForm
    , registerView

    , cancelForm
    , cancelView
    ) where

import           Control.Monad               (when)
import           Data.Maybe                  (isNothing)
import qualified Data.Text                   as T
import           Data.Time                   (UTCTime, utctDay)
import           Data.UUID                   (UUID)
import qualified Data.UUID                   as UUID
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive              as D
import qualified Text.Digestive.Blaze.Html5  as DH
import qualified Zureg.Captcha               as Captcha
import           Zureg.Database.Models
import qualified Zureg.Hackathon             as Hackathon
import           Zureg.Hackathon             (Hackathon)

data ProjectForm = ProjectForm
    { pfName                         :: Maybe T.Text
    , pfLink                         :: Maybe T.Text
    , pfShortDescription             :: Maybe T.Text
    , pfContributorLevelBeginner     :: !Bool
    , pfContributorLevelIntermediate :: !Bool
    , pfContributorLevelAdvanced     :: !Bool
    } deriving (Eq, Show)

-- | The 'IO' in this type signature is because we want to get the registration
-- time.
registerForm :: Monad m => D.Form H.Html m (InsertRegistration, Maybe ProjectForm)
registerForm = (,)
    <$> ("registration" D..: (InsertRegistration
        <$> "name" D..: (D.check "Name is required"
                (not . T.null . T.strip)
                (D.text Nothing))
        <*> "badgeName" D..: optionalText
        <*> (D.validate confirmEmailCheck $ (,)
                <$> "email" D..: simpleEmailCheck (T.strip <$> D.text Nothing)
                <*> "confirmEmail" D..: (T.strip <$> D.text Nothing))
        <*> "affiliation" D..: optionalText
        <*> ("tshirtSize" D..: D.choice (
                [(Just s, H.toHtml $ show s) | s <- [minBound .. maxBound]] ++
                [(Nothing, "I don't want a T-Shirt")])
                (Just (Just M)))
        <*> "region" D..: D.choice (
                (Nothing, "I'd rather not say") :
                [(Just s, H.toHtml $ show s) | s <- [minBound .. maxBound]])
                (Just Nothing)
        <*> "occupation" D..: D.choice
                [ (Nothing,       "I'd rather not say")
                , (Just Student,  "I am a student")
                , (Just Tech,     "I work in the tech sector")
                , (Just Academia, "I work in academia")
                , (Just Other,    "Other")
                ]
                (Just Nothing)
        <*> ("beginnerTrackInterest" D..: D.bool Nothing)))
    <*> ("project" D..: (fmap guardProject $ ProjectForm
            <$> "name" D..: optionalText
            <*> "website" D..: optionalText
            <*> "description" D..: optionalText
            <*> "contributorLevelBeginner" D..: D.bool Nothing
            <*> "contributorLevelIntermediate" D..: D.bool Nothing
            <*> "contributorLevelAdvanced" D..: D.bool Nothing))
  where
    simpleEmailCheck = D.check "Invalid email address" $ \email ->
        case T.split (== '@') email of
            [_user, domain] -> T.count "." domain >= 1
            _               -> False

    confirmEmailCheck (x, y)
        | x == y    = D.Success x
        | otherwise = D.Error "Email confirmation doesn't match"

    optionalText =
        (\t -> let t' = T.strip t in if T.null t' then Nothing else Just t') <$>
        (D.text Nothing)

    guardProject p
        | isNothing (pfName p) && isNothing (pfShortDescription p) = Nothing
        | otherwise                                                = Just p

registerView
    :: UTCTime -> Hackathon -> Captcha.ClientHtml -> D.View H.Html -> H.Html
registerView now h captchaHtml view = DH.form view "?" $ do
    H.h1 $ H.toHtml (Hackathon.name h) <> " registration"
    H.div H.! A.class_ "errors" $ DH.childErrorList "" view

    H.h2 $ "Basic information"
    DH.label "registration.name" view $ H.strong "Full name"
    DH.inputText "registration.name" view
    H.br

    DH.label "registration.badgeName" view $ H.strong "Name on badge (optional)"
    H.p $ do
        "Fill in this field if you would rather use a nickname on your "
        "badge.  By default we will use your full name."
    DH.inputText "registration.badgeName" view
    H.br

    DH.label "registration.email" view $ H.strong "Email"
    H.p $ do
        "We will only use your email address to send you your ticket, as well "
        "as information about the event.  It is not shared with any other "
        "parties."
    DH.inputText "registration.email" view
    H.br

    DH.label "registration.confirmEmail" view $ H.strong "Confirm your email"
    H.p "We want to be sure we that we can email you your ticket."
    DH.inputText "registration.confirmEmail" view
    H.br

    DH.label "registration.affiliation" view $ H.strong "Affiliation (optional)"
    H.p $ do
        "Affiliations that you want to display on your badge (e.g.: "
        "employer, university, open source project...)"
    DH.inputText "registration.affiliation" view
    H.br

    H.h2 "Optional information"

    H.p $ H.strong "T-Shirt"
    when (maybe False (utctDay now >=) $ Hackathon.tShirtDeadline h) $
        H.p $ H.strong $ do
            "Please note that we have ordered the T-Shirts and cannot guarantee "
            "that you will receive one if you register at this time."
    H.p $ "In what size would you like the free T-Shirt?"

    DH.label "registration.tshirtSize" view "Size"
    DH.inputSelect "registration.tshirtSize" view
    H.br

    H.p $ H.strong "Region"
    DH.label "registration.region" view $ do
        "From what area will you attend ZuriHac?  This is purely for our "
        "statistics."
    DH.inputSelect "registration.region" view
    H.br

    H.p $ H.strong "Occupation"
    DH.label "registration.occupation" view $
        "What is your occupation?  This is purely for our statistics."
    DH.inputSelect "registration.occupation" view
    H.br

    H.p $ H.strong "Beginner Track"
    DH.inputCheckbox "registration.beginnerTrackInterest" view H.! A.class_ "checkbox"
    DH.label "registration.beginnerTrackInterest" view $ do
        "I'm interested in attending the beginner track.  You don't need to "
        "commit to this, but it helps us gauge interest."
    H.br

    H.h2 "Project (optional)"
    H.p $ do
        "Do you have a project or an idea to hack on with others? Do you have "
        "something you want to teach people?"
    H.p $ do
        "We greatly appreciate projects. We have had very good experience with "
        "announcing the project early on the homepage, so that potential "
        "participants can prepare before the Hackathon.  Of course, we're also "
        "happy to add projects during the Hackathon itself, so if you're not "
        "sure yet, don't worry about it."
    DH.label "project.name" view "Project name"
    DH.inputText "project.name" view
    DH.label "project.website" view "Project website"
    DH.inputText "project.website" view
    DH.label "project.description" view "Project description"
    DH.inputText "project.description" view
    H.p "Recommended contributor level(s)"
    DH.inputCheckbox "project.contributorLevelBeginner" view H.! A.class_ "checkbox"
    DH.label "project.contributorLevelBeginner" view $ "Beginner"
    H.br
    DH.inputCheckbox "project.contributorLevelIntermediate" view H.! A.class_ "checkbox"
    DH.label "project.contributorLevelIntermediate" view $ "Intermediate"
    H.br
    DH.inputCheckbox "project.contributorLevelAdvanced" view H.! A.class_ "checkbox"
    DH.label "project.contributorLevelAdvanced" view $ "Advanced"

    H.h2 $ "Donation"
    H.p $ do
        "ZuriHac is free for all participants, but it does require "
        "resources to organize. "
        "Please consider making a donation here if you can afford it."
        " All donations go entirely towards event costs. "
        "We can accept "
        H.a H.! A.target "_blank" H.! A.href "https://zfoh.ch/donate" $
            "online payments using credit cards"
        " as well as "
        H.a H.! A.target "_blank" H.! A.href "https://zfoh.ch/#donations" $
            "wire transfers using IBAN"
        ". Please include in the message if you opt-in to being included in "
        "a thank-you list, and if so, under which name."

    H.h2 $ "Captcha (sorry)"
    Captcha.chForm captchaHtml
    H.br

    DH.inputSubmit "Register"

cancelForm :: Monad m => Maybe UUID -> D.Form H.Html m (UUID, Bool)
cancelForm uuid = (,)
    <$> "uuid" D..: D.validate
            validateUuid (D.text (fmap UUID.toText uuid))
    <*> "confirm" D..: D.bool Nothing

cancelView :: Maybe UUID -> D.View H.Html -> H.Html
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
            H.! A.value (maybe "" (H.toValue . UUID.toText) mbUuid)
        H.input H.! A.type_ "submit"
            H.! A.value "Take me back to my ticket"

validateUuid :: T.Text -> D.Result H.Html UUID
validateUuid txt = case UUID.fromText txt of
    Nothing -> D.Error "Not a valid UUID"
    Just u  -> D.Success u
