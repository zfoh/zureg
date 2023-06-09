{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.Hackathon.ZuriHac2023.Views
    ( scanView
    , badgeView
    ) where

import qualified Data.Time                         as Time
import qualified Text.Blaze.Html5                  as H
import qualified Text.Blaze.Html5.Attributes       as HA

import qualified Zureg.Hackathon.ZuriHac2023.Model as ZH23
import           Zureg.Model

tShirtDeadline :: Time.UTCTime
tShirtDeadline = Time.UTCTime (Time.fromGregorian 2023 5 5) (12 * 3600)

scanView :: Registrant ZH23.RegisterInfo -> H.Html
scanView r@Registrant {..} = case rAdditionalInfo of
    Nothing                -> mempty
    Just ZH23.RegisterInfo {..} -> case riTShirtSize of
        Nothing   -> "No T-Shirt"
        Just size -> case registrantRegisteredAt r of
            Just at | at >= tShirtDeadline ->
                H.p $ H.strong "Pick up T-Shirt later"
            _ -> do
                "T-Shirt: "
                H.strong $ H.toHtml (show size)

badgeView :: Registrant ZH23.RegisterInfo -> Maybe H.Html
badgeView Registrant {..} = do
    info <- rInfo
    pure $ do
        H.toHtml $ riName info
        H.br
        H.img H.! HA.style "width: 45%; margin: 0.em" H.! HA.src "./badges.svg"
