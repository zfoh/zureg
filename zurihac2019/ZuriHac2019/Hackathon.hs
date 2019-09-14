{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module ZuriHac2019.Hackathon (
      zuriHac2019
    ) where

import           Data.List         (intercalate)
import qualified Text.Blaze.Html5            as H

import           Zureg.Hackathon   as Hackathon
import           ZuriHac2019.Model as ZuriHac2019

zuriHac2019 :: Hackathon.Config -> Hackathon.Handle ZuriHac2019.RegisterInfo
zuriHac2019 config = Hackathon.Handle
    { hConfig = config
    , hTicketView = \RegisterInfo {..} -> do
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
            ["Beginner" | tiBeginner tiTrackInterest]
            ++ ["Intermediate" | tiIntermediate tiTrackInterest]
            ++ ["Advanced" | tiAdvanced tiTrackInterest]
            ++ ["GHC DevOps" | tiGhcDevOps tiTrackInterest]
    , hScanView = \RegisterInfo {..} -> case riTShirt of
        Nothing      -> "No T-Shirt"
        Just rTShirt -> "T-Shirt: " <> H.strong (
            (H.toHtml.show $ fst rTShirt) <> ", " <>
            (H.toHtml.show $ snd rTShirt) <> ", " <>
            (if riMentor then "Navy" else "Espresso"))
    }