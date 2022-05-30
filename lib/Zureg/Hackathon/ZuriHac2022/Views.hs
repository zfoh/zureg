{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.Hackathon.ZuriHac2022.Views
    ( scanView
    ) where

import qualified Data.Time                         as Time
import qualified Text.Blaze.Html5                  as H

import qualified Zureg.Hackathon.ZuriHac2022.Model as ZH22
import           Zureg.Model

tShirtDeadline :: Time.UTCTime
tShirtDeadline = Time.UTCTime (Time.fromGregorian 2022 5 4) (10 * 3600 + 19 * 60)

scanView :: Registrant ZH22.RegisterInfo -> H.Html
scanView r@Registrant {..} = case rAdditionalInfo of
    Nothing                -> mempty
    Just ZH22.RegisterInfo {..} -> case riTShirt of
        Nothing                   -> "No T-Shirt"
        Just ZH22.TShirtInfo {..} -> do
            case registrantRegisteredAt r of
                Just at | at >= tShirtDeadline ->
                    H.p $ H.strong "Pick up T-Shirt later"
                _ -> do
                    "T-Shirt: "
                    H.strong $ do
                        H.toHtml (show tsiCut)
                        ", "
                        H.toHtml (show tsiSize)
