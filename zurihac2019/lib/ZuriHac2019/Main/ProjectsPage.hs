-- | Generate the projects page for the main website.  Ugly.
{-# LANGUAGE OverloadedStrings #-}
module ZuriHac2019.Main.ProjectsPage
    ( main
    ) where

import qualified Data.Aeson                      as A
import qualified Data.List                       as L
import           Data.Maybe                      (fromMaybe, maybeToList)
import qualified Data.Text                       as T
import           System.Environment              (getArgs, getProgName)
import qualified System.IO                       as IO
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA
import           Zureg.Model                     as Zureg
import           ZuriHac2019.Model               as ZH19

renderProject :: Zureg.RegisterInfo -> ZH19.Project -> Maybe H.Html
renderProject contact project = do
    name <- pName project
    return $ do
        case pWebsite project of
            Nothing -> H.p H.! HA.class_ "name" $ H.toHtml name
            Just web -> H.a H.! HA.class_ "name" H.! HA.href (H.toValue web) $
                H.toHtml name
        H.p $ do
            H.strong "Contributor levels:"
            " "
            mconcat $ L.intersperse ", " $
                ["Beginner"     | clBeginner     (pContributorLevel project)] ++
                ["Intermediate" | clIntermediate (pContributorLevel project)] ++
                ["Advanced"     | clAdvanced     (pContributorLevel project)]
        H.p $ do
            H.strong "Contact: "
            " "
            H.toHtml $ fromMaybe (riName contact) (riBadgeName contact)

        maybe mempty (H.p . H.toHtml) (pShortDescription project)

renderProjects :: [Registrant ZH19.RegisterInfo] -> H.Html
renderProjects registrants = H.ul H.! HA.class_ "projects" $ mconcat $ do
    registrant <- L.sortOn sortKey registrants
    contact    <- maybeToList $ rInfo registrant
    project    <- maybeToList $ riProject <$> rAdditionalInfo registrant
    html       <- maybeToList $ renderProject contact project
    return $ H.li html
  where
    sortKey r = rAdditionalInfo r >>= fmap T.toLower . pName . riProject

main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs

    case args of
        [exportPath] -> do
            registrantsOrError <- A.eitherDecodeFileStrict exportPath
            registrants <- either (fail . show) return registrantsOrError
                :: IO [Registrant ZH19.RegisterInfo]

            putStr $ Pretty.renderHtml $ renderProjects registrants
        _ -> do
            IO.hPutStr IO.stderr $ unlines
                [ "Usage: " ++ progName ++ " export.json"
                , ""
                , "export.json is a list of registrants as obtained by the"
                , "export tool."
                ]
