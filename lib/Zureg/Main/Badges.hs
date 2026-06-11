{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.Badges
    ( Badge (..)
    , registrantToBadge
    , main
    ) where

import qualified Data.Aeson                      as A
import           Data.Char                       (toLower)
import           Data.Foldable                   (for_)
import           Data.List                       (sortOn)
import           Data.Maybe                      (fromMaybe, mapMaybe)
import qualified Data.Text                       as T
import qualified Options.Applicative             as OA
import qualified Text.Blaze.Html.Renderer.Pretty as H
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA
import           Zureg.Database.Models
import           Zureg.Main.Export               hiding (main)

newtype Badge = Badge {unBadge :: String}

registrantToBadge :: ExportRegistration -> Maybe Badge
registrantToBadge r
    | erState r `elem` [Confirmed, Registered] = Just $ Badge $
        T.unpack $ fromMaybe (erName registrant) (erBadgeName registrant)
    | otherwise = Nothing
  where
    registrant = erRegistrant r

-- | For 2023, we used 21 70mm 42.4mm
data Options = Options
    { oBadgesPerPage :: Int
    , oBadgeWidth    :: String
    , oBadgeHeight   :: String
    , oExport        :: FilePath
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.option OA.auto (
        OA.long    "badges-per-page" <>
        OA.metavar "NUM")
    <*> OA.strOption (
        OA.long    "badge-width" <>
        OA.help    "width of a badge, e.g. '70mm'" <>
        OA.metavar "CSS")
    <*> OA.strOption (
        OA.long    "badge-height" <>
        OA.help    "width of a badge, e.g. '42.4mm'" <>
        OA.metavar "CSS")
    <*> OA.strArgument (
        OA.help    "json file containing registration data" <>
        OA.metavar "INPUT")

renderBadges :: Options -> [Badge] -> H.Html
renderBadges options badges = H.docTypeHtml $ do
    H.head $ do
        H.style H.! HA.type_ "text/css" H.! HA.media "print" $ do
            "@page {"
            "    size: auto;"
            "    margin: 0mm;"
            "}"
        H.style H.! HA.type_ "text/css" $ do
            ":root {"
            "    --badge-width: " <> H.toHtml (oBadgeWidth options) <> ";"
            "    --badge-height: " <> H.toHtml (oBadgeHeight options) <> ";"
            "    --badge-margin-top: 8mm;"
            "    --badge-margin-side: 5mm;"
            "}"
            "body {"
            "    font-size: 5mm;"
            "    font-family: sans;"
            "    font-stretch: condensed;"
            "    font-weight: bold;"
            "    margin: 0px;"
            "    padding: 0px;"
            "}"
            ".page {"
            "    page-break-after: always;"
            "    display: flex;"
            "    flex-wrap: wrap;"
            "}"
            ".badge {"
            "    width: calc(var(--badge-width) - 2 * var(--badge-margin-side));"
            "    height: calc(var(--badge-height) - var(--badge-margin-top));"
            "    padding-top: var(--badge-margin-top);"
            "    padding-left: var(--badge-margin-side);"
            "    padding-right: var(--badge-margin-side);"
            "    text-align: center;"
            "}"
    H.body $ for_ (pages (oBadgesPerPage options) badges) $ \page ->
        H.div H.! HA.class_ "page" $
            for_ page $ \(Badge badge) -> H.div H.! HA.class_ "badge" $
                H.span $ H.toHtml badge

pages :: Int -> [a] -> [[a]]
pages n ls = case splitAt n ls of
    ([],   _)  -> []
    (page, []) -> [page]
    (page, t)  -> page : pages n t

main :: IO ()
main = do
    opts <- OA.execParser $ OA.info (parseOptions OA.<**> OA.helper) OA.fullDesc

    registrantsOrError <- A.eitherDecodeFileStrict (oExport opts)
    registrants <- either (fail . show) return registrantsOrError
        :: IO [ExportRegistration]
    putStrLn $ H.renderHtml $ renderBadges opts $
        sortOn (map toLower . unBadge) $
        mapMaybe registrantToBadge registrants

