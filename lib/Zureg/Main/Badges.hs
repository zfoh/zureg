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
import           Data.Maybe                      (mapMaybe)
import qualified Data.Text                       as T
import           System.Environment              (getArgs, getProgName)
import           System.Exit                     (exitFailure)
import qualified System.IO                       as IO
import qualified Text.Blaze.Html.Renderer.Pretty as H
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA
import           Text.Read                       (readMaybe)
import           Zureg.Hackathon                 (Hackathon)
import           Zureg.Model

newtype Badge = Badge {unBadge :: String}

registrantToBadge :: Registrant -> Maybe Badge
registrantToBadge r
    | rState r `elem` map Just [Confirmed, Registered] =
        Badge . T.unpack . riName <$> rInfo r
    | otherwise = Nothing

-- | For 2023, we used 21 70mm 42.4mm
data Options = Options
    { oPerPage     :: Int
    , oBadgeWidth  :: String
    , oBadgeHeight :: String
    } deriving (Show)

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
    H.body $ for_ (pages (oPerPage options) badges) $ \page ->
        H.div H.! HA.class_ "page" $
            for_ page $ \(Badge badge) -> H.div H.! HA.class_ "badge" $
                H.span $ H.toHtml badge

pages :: Int -> [a] -> [[a]]
pages n ls = case splitAt n ls of
    ([],   _)  -> []
    (page, []) -> [page]
    (page, t)  -> page : pages n t

main :: Hackathon -> IO ()
main _ = do
    progName <- getProgName
    args     <- getArgs

    case args of
        [perPageStr, badgeWidth, badgeHeight, exportPath] | Just perPage <- readMaybe perPageStr -> do
            let options = Options
                    { oPerPage     = perPage
                    , oBadgeWidth  = badgeWidth
                    , oBadgeHeight = badgeHeight
                    }
            registrantsOrError <- A.eitherDecodeFileStrict exportPath
            registrants <- either (fail . show) return registrantsOrError
                :: IO [Registrant]
            putStrLn $ H.renderHtml $ renderBadges options $
                sortOn (map toLower . unBadge) $
                mapMaybe registrantToBadge registrants
        _ -> do
            IO.hPutStr IO.stderr $ unlines
                [ "Usage: " ++ progName ++ " badges-per-page badge-width badge-height export.json"
                , ""
                , "export.json is a list of registrants as obtained by the"
                , "export tool."
                ]
            exitFailure
