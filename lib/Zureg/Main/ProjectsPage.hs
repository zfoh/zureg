-- | Generate the projects page for the main website.
module Zureg.Main.ProjectsPage
    ( main
    ) where

import qualified Data.Aeson         as A
import           System.Environment (getArgs, getProgName)
import qualified System.IO          as IO
import           Zureg.Model

main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs

    case args of
        [exportPath] -> do
            registrantsOrError <- A.eitherDecodeFileStrict exportPath
            registrants <- either (fail . show) return registrantsOrError
                :: IO [Registrant]

            print registrants
        _ -> do
            IO.hPutStr IO.stderr $ unlines
                [ "Usage: " ++ progName ++ " export.json"
                , ""
                , "export.json is a list of registrants as obtained by the"
                , "export tool."
                ]
