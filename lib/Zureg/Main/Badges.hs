{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.Badges
    ( main
    ) where

import qualified Data.Aeson                      as A
import           Data.Foldable                   (for_)
import           Data.Maybe                      (mapMaybe)
import           System.Environment              (getArgs, getProgName)
import           System.Exit                     (exitFailure)
import qualified System.IO                       as IO
import qualified Text.Blaze.Html                 as H
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA
import           Text.Read                       (readMaybe)
import qualified Zureg.Hackathon                 as Hackathon
import           Zureg.Hackathon                 (Hackathon)
import           Zureg.Model

html
    :: forall a. Hackathon a
    -> Int -> String -> String -> [Registrant a] -> H.Html
html hackathon badgesPerPage badgeWidth badgeHeight registrants = H.docTypeHtml $ do
    H.head $ do
        H.style H.! HA.type_ "text/css" H.! HA.media "print" $ do
            "@page {"
            "    size: auto;"
            "    margin: 0mm;"
            "}"
        H.style H.! HA.type_ "text/css" $ do
            ":root {"
            "    --badge-width: " <> H.toHtml badgeWidth <> ";"
            "    --badge-height: " <> H.toHtml badgeHeight <> ";"
            "    --badge-margin-side: 0.5cm;"
            "}"

            "body {"
            "    font-size: 0.5cm;"
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
            "    break-inside: avoid;"

            "    width: calc(var(--badge-width) - 2 * var(--badge-margin-side));"
            "    height: var(--badge-height);"

            "    padding-left: var(--badge-margin-side);"
            "    padding-right: var(--badge-margin-side);"

            "    text-align: center;"

            "    print-color-adjust: exact !important;"
            "}"
    H.body $ for_ (paginate badgesPerPage badges) $ \page -> do
        H.div H.! HA.class_ "page" $ do
            for_ page $ \badge -> do
                H.div H.! HA.class_ "badge" $ badge
  where
    badges        = mapMaybe (Hackathon.badgeView hackathon) registrants
    paginate n xs = case splitAt n xs of
        (page, []) -> [page]
        (page, ys) -> page : paginate n ys


main :: forall a. (A.FromJSON a, A.ToJSON a) => Hackathon a -> IO ()
main hackathon = do
    progName <- getProgName
    args     <- getArgs

    case args of
        [exportPath, badgesPerPage, badgeWidth, badgeHeight] -> do
            registrantsOrError <- A.eitherDecodeFileStrict exportPath
            registrants <- either (fail . show) return registrantsOrError
                :: IO [Registrant a]

            badgesPerPage' <- maybe (error "read badgesPerPage") pure $
                readMaybe badgesPerPage

            putStr $ Pretty.renderHtml $ html hackathon
                badgesPerPage' badgeWidth badgeHeight registrants

        _ -> do
            IO.hPutStr IO.stderr $ unlines
                [ "Usage: " ++ progName ++
                    " export.json badges-per-page badge-width badge-height"
                , ""
                , "export.json is a list of registrants as obtained by the"
                , "export tool."
                ]
            exitFailure
