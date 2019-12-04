{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.Hackathon.ZuriHac2020.Views
    ( ticketView
    , scanView
    ) where

import           Data.List                         (intercalate)
import qualified Text.Blaze.Html5                  as H

import           Zureg.Hackathon.ZuriHac2020.Model as ZH20

ticketView :: ZH20.RegisterInfo -> H.Html
ticketView RegisterInfo {..} = do
    case riTShirt of
        Just rTShirt ->
            "T-Shirt: "
            <> (H.toHtml.show $ fst rTShirt) <> ", "
            <> (H.toHtml.show $ snd rTShirt)
        Nothing -> mempty
    "Track interest(s): "
    H.toHtml $ intercalate ", " $
        ["Beginner" | tiBeginner tiTrackInterest]
        ++ ["Intermediate" | tiIntermediate tiTrackInterest]
        ++ ["Advanced" | tiAdvanced tiTrackInterest]
        ++ ["GHC DevOps" | tiGhcDevOps tiTrackInterest]

scanView :: ZH20.RegisterInfo -> H.Html
scanView RegisterInfo {..} = case riTShirt of
    Nothing      -> "No T-Shirt"
    Just rTShirt -> "T-Shirt: " <> H.strong (
        (H.toHtml.show $ fst rTShirt) <> ", " <>
        (H.toHtml.show $ snd rTShirt))
