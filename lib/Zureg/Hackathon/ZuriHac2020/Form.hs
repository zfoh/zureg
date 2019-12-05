{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.ZuriHac2020.Form
    ( additionalInfoForm
    , additionalInfoView
    ) where

import qualified Data.Text                         as T
import qualified Text.Blaze.Html5                  as H
import qualified Text.Blaze.Html5.Attributes       as A
import qualified Text.Digestive                    as D
import qualified Text.Digestive.Blaze.Html5        as DH
import           Zureg.Hackathon.ZuriHac2020.Model as ZH20

additionalInfoForm :: Monad m => D.Form H.Html m ZH20.RegisterInfo
additionalInfoForm = RegisterInfo
    <$> "askMeAbout" D..: optionalText
    <*> "hosting" D..: D.bool Nothing
    <*> "region" D..: D.choice (
            [(Just s, H.toHtml $ show s) | s <- [minBound .. maxBound]] ++
            [(Nothing, "I'd rather not say")])
            (Just Nothing)
    <*> "trackInterest" D..: (TrackInterest
            <$> "beginner" D..: D.bool Nothing
            <*> "intermediate" D..: D.bool Nothing
            <*> "advanced" D..: D.bool Nothing
            <*> "ghcDevOps" D..: D.bool Nothing)
    <*> ("tshirt" D..: (D.validate tshirtCheck $ (,)
            <$> "cut" D..: D.choice
                    [ (Nothing,     "I don't want a T-Shirt")
                    , (Just Female, "Female")
                    , (Just Male,   "Male")
                    ] (Just (Just Male))
            <*> "size" D..: D.choice (
                    [(Just s, H.toHtml $ show s) | s <- [minBound .. maxBound]] ++
                    [(Nothing, "I don't want a T-Shirt")])
                    (Just (Just M))))
    <*> ("project" D..: (Project
            <$> "name" D..: optionalText
            <*> "website" D..: optionalText
            <*> "description" D..: optionalText
            <*> ("contributorLevel" D..: (ContributorLevel
                    <$> "beginner" D..: D.bool Nothing
                    <*> "intermediate" D..: D.bool Nothing
                    <*> "advanced" D..: D.bool Nothing))))
  where
    tshirtCheck (Just c,  Just s)  = D.Success (Just (c, s))
    tshirtCheck (Nothing, Nothing) = D.Success Nothing
    tshirtCheck (_,       _)       = D.Error
        "Fill in both T-Shirt cut and size or neither of the two"

    optionalText =
        (\t -> let t' = T.strip t in if T.null t' then Nothing else Just t') <$>
        (D.text Nothing)

additionalInfoView :: D.View H.Html -> H.Html
additionalInfoView view = do
    H.h2 "Optional information"
    DH.label "askMeAbout" view $ H.strong "Ask me about"
    H.p $ do
        "Topic(s) that you want to display on your badge.  It's a good ice "
        "breaker for people who want to chat with you."
    DH.inputText "askMeAbout" view
    H.br

    H.p $ H.strong "Hosting Interest"
    H.p $ do
        "Zurich can be an expensive city, for example if you are a student or "
        "if you are employed in a country with lower wages. "
        "If you live near Zurich, you can help out by hosting someone on a "
        "couch or spare bedroom during the event."
    DH.inputCheckbox "hosting" view H.! A.class_ "checkbox"
    DH.label "hosting" view $ "I can host someone near Zurich"
    H.br

    H.p $ H.strong "Region"
    DH.label "region" view $ do
        "From what area will you travel to ZuriHac?  This is purely for our "
        "statistics."
    DH.inputSelect "region" view
    H.br

    H.h2 "Track Interest (optional)"
    H.p $ do
        "Let us know which track(s) you would participate in.  Note that we "
        "are still in the process of organizing these, so this serves as an "
        "indication for us, not a commitment on your part.  We may not "
        "organize all of these tracks depending on availability and interest."
    DH.inputCheckbox "trackInterest.beginner" view H.! A.class_ "checkbox"
    DH.label "trackInterest.beginner" view $ "Beginner Track"
    H.br
    DH.inputCheckbox "trackInterest.intermediate" view H.! A.class_ "checkbox"
    DH.label "trackInterest.intermediate" view $ "Intermediate Track"
    H.br
    DH.inputCheckbox "trackInterest.advanced" view H.! A.class_ "checkbox"
    DH.label "trackInterest.advanced" view $ "Advanced Track"
    H.br
    DH.inputCheckbox "trackInterest.ghcDevOps" view H.! A.class_ "checkbox"
    DH.label "trackInterest.ghcDevOps" view $ "GHC DevOps Track"
    H.br

    H.h2 "T-Shirt"
    H.p $ "In what size would you like the free T-Shirt?"
    H.p $ do
        "The sizes should be fairly standard. "
        "You can see the "
        H.a H.! A.href "https://zfoh.ch/images/zurihac2019/tshirts-sizing.png"
            H.! A.target "_blank" $
            "specifications here"
        "."

    DH.label "tshirt.cut" view "Cut"
    DH.inputSelect "tshirt.cut" view
    H.br
    DH.label "tshirt.size" view "Size"
    DH.inputSelect "tshirt.size" view
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
