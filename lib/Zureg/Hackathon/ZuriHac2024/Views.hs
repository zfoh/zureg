{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.Hackathon.ZuriHac2024.Views
    ( scanView
    ) where

import qualified Data.Time                         as Time
import qualified Text.Blaze.Html5                  as H

import qualified Zureg.Hackathon.ZuriHac2024.Model as ZH24
import           Zureg.Model

tShirtDeadline :: Time.UTCTime
tShirtDeadline = Time.UTCTime (Time.fromGregorian 2024 5 5) (12 * 3600)

scanView :: Registrant ZH24.RegisterInfo -> H.Html
scanView r@Registrant {..} = case rAdditionalInfo of
    Nothing                -> mempty
    Just ZH24.RegisterInfo {..} -> case riTShirtSize of
        Nothing   -> "No T-Shirt"
        Just size -> case registrantRegisteredAt r of
            Just at | at >= tShirtDeadline ->
                H.p $ H.strong "Pick up T-Shirt later"
            _ -> do
                "T-Shirt: "
                H.strong $ H.toHtml (show size)
