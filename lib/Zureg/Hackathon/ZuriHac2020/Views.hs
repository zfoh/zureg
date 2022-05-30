{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.Hackathon.ZuriHac2020.Views
    ( ticketView
    , scanView
    ) where

import           Data.List                         (intercalate)
import qualified Text.Blaze.Html5                  as H

import qualified Zureg.Hackathon.ZuriHac2020.Model as ZH20
import           Zureg.Model

ticketView :: ZH20.RegisterInfo -> H.Html
ticketView ZH20.RegisterInfo {..} = do
    case riTShirt of
        Just rTShirt ->
            "T-Shirt: "
            <> (H.toHtml.show $ fst rTShirt) <> ", "
            <> (H.toHtml.show $ snd rTShirt) <> H.br
        Nothing -> mempty
    "Track interest(s): "
    H.toHtml $ intercalate ", " $
        ["Beginner" | ZH20.tiBeginner riTrackInterest]
        ++ ["Intermediate" | ZH20.tiIntermediate riTrackInterest]
        ++ ["Advanced" | ZH20.tiAdvanced riTrackInterest]
        ++ ["GHC DevOps" | ZH20.tiGhcDevOps riTrackInterest]

scanView :: Registrant ZH20.RegisterInfo -> H.Html
scanView Registrant {..} = case rAdditionalInfo of
    Nothing -> mempty
    Just ZH20.RegisterInfo {..} -> case riTShirt of
        Nothing      -> "No T-Shirt"
        Just rTShirt -> "T-Shirt: " <> H.strong (
            (H.toHtml.show $ fst rTShirt) <> ", " <>
            (H.toHtml.show $ snd rTShirt))
