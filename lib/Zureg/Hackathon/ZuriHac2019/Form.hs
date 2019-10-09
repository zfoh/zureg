{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.ZuriHac2019.Form
    ( additionalInfoForm
    , additionalInfoView
    ) where

import qualified Data.Text                         as T
import qualified Text.Blaze.Html5                  as H
import qualified Text.Blaze.Html5.Attributes       as A
import qualified Text.Digestive                    as D
import qualified Text.Digestive.Blaze.Html5        as DH
import           Zureg.Hackathon.ZuriHac2019.Model as ZH19

additionalInfoForm :: Monad m => D.Form H.Html m ZH19.RegisterInfo
additionalInfoForm = RegisterInfo
    <$> "trackInterest" D..: (TrackInterest
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
    <*> ("mentor" D..: D.bool Nothing)
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
    H.p $ H.strong "Track Interest (optional)"
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

    H.p $ H.strong "T-Shirt"

    H.p $ H.strong $ do
        "Please note that we have ordered the T-Shirts and cannot guarantee "
        "that you will receive one if you register at this time."

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

    H.p $ H.strong "Mentors (optional)"
    H.p $ do
        "Every year, we are looking for volunteers to help Haskell newcomers "
        "with questions. The idea is that people can call on your help if you "
        "happen to be around, however you won't be busy with this full-time. "
        "You will get a different t-shirt so that people can visibly identify "
        "you as a mentor."
    DH.inputCheckbox "mentor" view H.! A.class_ "checkbox"
    DH.label "mentor" view $ "I want to be a mentor"
    H.br

    H.p $ H.strong "Project (optional)"
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
