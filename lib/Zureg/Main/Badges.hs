{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.Badges
    ( Badge
    , previewBadge
    , registrantToBadge

    , main
    ) where

import           Control.Monad        (guard)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import           Data.Maybe           (maybeToList)
import           Data.Maybe           (fromMaybe, mapMaybe)
import qualified Data.Text            as T
import           System.Environment   (getArgs, getProgName)
import           System.Exit          (exitFailure)
import qualified System.IO            as IO
import           Zureg.Hackathon      (Hackathon)
import           Zureg.Model

data Badge = Badge
    { bLine1 :: T.Text
    , bLine2 :: Maybe T.Text
    }

previewBadge :: Badge -> T.Text
previewBadge Badge {..} = T.intercalate ", " $
    [bLine1] ++ maybeToList bLine2

badgeCsvHeader :: Csv.Header
badgeCsvHeader = Csv.header ["Line 1", "Line 2", "Line 3"]

instance Csv.ToNamedRecord Badge where
    toNamedRecord Badge {..} = Csv.namedRecord
        [ "Line 1" Csv..= bLine1
        , "Line 2" Csv..= bLine2
        ]

registrantToBadge :: Registrant a -> Maybe Badge
registrantToBadge Registrant {..} = do
    state <- rState
    guard $ state `elem` [Confirmed, Registered]
    RegisterInfo {..} <- rInfo
    let bLine1 = fromMaybe riName riBadgeName
        bLine2 = riAffiliation
    pure Badge {..}

main :: forall e a. A.FromJSON a => Hackathon e a -> IO ()
main _ = do
    progName <- getProgName
    args     <- getArgs

    case args of
        [exportPath] -> do
            registrantsOrError <- A.eitherDecodeFileStrict exportPath
            registrants <- either (fail . show) return registrantsOrError
                :: IO [Registrant a]

            BL.putStr $ Csv.encodeByName badgeCsvHeader $
                mapMaybe registrantToBadge registrants

        _ -> do
            IO.hPutStr IO.stderr $ unlines
                [ "Usage: " ++ progName ++ " export.json"
                , ""
                , "export.json is a list of registrants as obtained by the"
                , "export tool."
                ]
            exitFailure
