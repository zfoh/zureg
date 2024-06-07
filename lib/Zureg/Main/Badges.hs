{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.Badges
    ( Badge (..)
    , registrantToBadge
    , main
    ) where

import qualified Data.Aeson                      as A
import           Data.Foldable                   (for_)
import           Data.Maybe                      (mapMaybe)
import qualified Data.Text                       as T
import           System.Environment              (getArgs, getProgName)
import           System.Exit                     (exitFailure)
import qualified System.IO                       as IO
import qualified Text.Blaze.Html.Renderer.Pretty as H
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA
import           Zureg.Hackathon                 (Hackathon)
import           Zureg.Model

newtype Badge = Badge String

registrantToBadge :: Registrant a -> Maybe Badge
registrantToBadge r
    | rState r `elem` map Just [Confirmed, Registered] =
        Badge . T.unpack . riName <$> rInfo r
    | otherwise = Nothing

renderBadges :: [Badge] -> H.Html
renderBadges badges = H.docTypeHtml $ do
    H.head $ do
        H.style H.! HA.type_ "text/css" H.! HA.media "print" $ do
            "@page {"
            "    size: auto;"
            "    margin: 0mm;"
            "}"
        H.style H.! HA.type_ "text/css" $ do
            ":root {"
            "    --badge-width: 70mm;"
            "    --badge-height: 42.4mm;"
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
            "    width: calc(var(--badge-width) - 2 * var(--badge-margin-side));"
            "    height: var(--badge-height);"
            "    padding-left: var(--badge-margin-side);"
            "    padding-right: var(--badge-margin-side);"
            "    text-align: center;"
            "}"
    H.body $ do
        for_ (pages 21 badges) $ \page -> H.div H.! HA.class_ "page" $ do
            for_ page $ \(Badge badge) -> H.div H.! HA.class_ "badge" $ do
                H.toHtml badge

pages :: Int -> [a] -> [[a]]
pages n ls = case splitAt n ls of
    ([],   _)  -> []
    (page, []) -> [page]
    (page, t)  -> page : pages n t

main :: forall a. A.FromJSON a => Hackathon a -> IO ()
main _ = do
    progName <- getProgName
    args     <- getArgs

    case args of
        [exportPath] -> do
            registrantsOrError <- A.eitherDecodeFileStrict exportPath
            registrants <- either (fail . show) return registrantsOrError
                :: IO [Registrant a]
            putStrLn $ H.renderHtml $ renderBadges $
                mapMaybe registrantToBadge registrants
        _ -> do
            IO.hPutStr IO.stderr $ unlines
                [ "Usage: " ++ progName ++ " export.json"
                , ""
                , "export.json is a list of registrants as obtained by the"
                , "export tool."
                ]
            exitFailure
