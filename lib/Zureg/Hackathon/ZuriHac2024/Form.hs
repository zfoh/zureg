{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.ZuriHac2024.Form
    ( additionalInfoView
    ) where

import qualified Data.Text                         as T
import qualified Text.Blaze.Html5                  as H
import qualified Text.Blaze.Html5.Attributes       as A
import qualified Text.Digestive                    as D
import qualified Text.Digestive.Blaze.Html5        as DH

additionalInfoView :: D.View H.Html -> H.Html
additionalInfoView view = do
    H.h2 "Optional information"

    H.p $ H.strong "T-Shirt"
    H.p $ H.strong $ do
        "Please note that we have ordered the T-Shirts and cannot guarantee "
        "that you will receive one if you register at this time."
    H.p $ "In what size would you like the free T-Shirt?"

    DH.label "tshirtSize" view "Size"
    DH.inputSelect "tshirtSize" view
    H.br

    H.p $ H.strong "Region"
    DH.label "region" view $ do
        "From what area will you attend ZuriHac?  This is purely for our "
        "statistics."
    DH.inputSelect "region" view
    H.br

    H.p $ H.strong "Occupation"
    DH.label "occupation" view $
        "What is your occupation?  This is purely for our statistics."
    DH.inputSelect "occupation" view
    H.br

    H.p $ H.strong "Beginner Track"
    DH.inputCheckbox "beginnerTrackInterest" view H.! A.class_ "checkbox"
    DH.label "beginnerTrackInterest" view $ do
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
    DH.inputCheckbox "project.contributorLevel.beginner" view H.! A.class_ "checkbox"
    DH.label "project.contributorLevel.beginner" view $ "Beginner"
    H.br
    DH.inputCheckbox "project.contributorLevel.intermediate" view H.! A.class_ "checkbox"
    DH.label "project.contributorLevel.intermediate" view $ "Intermediate"
    H.br
    DH.inputCheckbox "project.contributorLevel.advanced" view H.! A.class_ "checkbox"
    DH.label "project.contributorLevel.advanced" view $ "Advanced"
