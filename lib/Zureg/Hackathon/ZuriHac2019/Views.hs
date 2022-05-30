{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.Hackathon.ZuriHac2019.Views
    ( ticketView
    , scanView
    ) where

import           Data.List                         (intercalate)
import qualified Text.Blaze.Html5                  as H

import qualified Zureg.Hackathon.ZuriHac2019.Model as ZH19
import           Zureg.Model

ticketView :: ZH19.RegisterInfo -> H.Html
ticketView ZH19.RegisterInfo {..} = do
    case riTShirt of
        Just rTShirt ->
            "T-Shirt: "
            <> (H.toHtml.show $ fst rTShirt) <> ", "
            <> (H.toHtml.show $ snd rTShirt)
            <>  case riMentor of
                    True  -> ", Mentorshirt"  <> H.br
                    False -> H.br
        Nothing -> mempty
    "Track interest(s): "
    H.toHtml $ intercalate ", " $
        ["Beginner" | ZH19.tiBeginner tiTrackInterest]
        ++ ["Intermediate" | ZH19.tiIntermediate tiTrackInterest]
        ++ ["Advanced" | ZH19.tiAdvanced tiTrackInterest]
        ++ ["GHC DevOps" | ZH19.tiGhcDevOps tiTrackInterest]

scanView :: Registrant ZH19.RegisterInfo -> H.Html
scanView Registrant {..} = case rAdditionalInfo of
    Nothing -> mempty
    Just ZH19.RegisterInfo {..} -> case riTShirt of
        Nothing      -> "No T-Shirt"
        Just rTShirt -> "T-Shirt: " <> H.strong (
            (H.toHtml.show $ fst rTShirt) <> ", " <>
            (H.toHtml.show $ snd rTShirt) <> ", " <>
            (if riMentor then "Navy" else "Espresso"))
