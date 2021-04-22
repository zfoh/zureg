{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.Hackathon.ZuriHac2021.Views
    ( ticketView
    , scanView
    ) where

import           Data.List                         (intercalate)
import qualified Text.Blaze.Html5                  as H

import           Zureg.Hackathon.ZuriHac2021.Model as ZH21

ticketView :: ZH21.RegisterInfo -> H.Html
ticketView RegisterInfo {..} = do
    "Track interest(s): "
    H.toHtml $ intercalate ", " $
        ["Beginner" | tiBeginner riTrackInterest]
        ++ ["Intermediate" | tiIntermediate riTrackInterest]
        ++ ["Advanced" | tiAdvanced riTrackInterest]
        ++ ["GHC DevOps" | tiGhcDevOps riTrackInterest]

scanView :: ZH21.RegisterInfo -> H.Html
scanView _ = mempty
