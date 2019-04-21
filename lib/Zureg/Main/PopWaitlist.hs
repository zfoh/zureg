{-# LANGUAGE OverloadedStrings #-}
module Zureg.Main.PopWaitlist
    ( main
    ) where

import qualified Data.Text          as T
import qualified Data.Time          as Time
import qualified Eventful           as E
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure)
import qualified System.IO          as IO
import qualified Zureg.Config       as Config
import qualified Zureg.Database     as Database
import           Zureg.Model

main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs

    config   <- Config.load "zureg.json"
    dbConfig <- Config.section config "database"

    case args of
        [uuidstr] -> Database.withHandle dbConfig $ \db -> do
            uuid <- maybe (fail "could not parse uuid") return
                (E.uuidFromText $ T.pack uuidstr)
            registrant <- Database.getRegistrant db uuid
            event <- PopWaitlist . PopWaitlistInfo <$> Time.getCurrentTime
            let registrant' = E.projectionEventHandler
                    (registrantProjection uuid) registrant event
            if registrant == registrant'
                then fail "patch did not apply"
                else Database.writeEvents db uuid [event]
        _ -> do
            IO.hPutStr IO.stderr $ unlines
                [ "Usage: " ++ progName ++ " <uuid>"
                , ""
                , "Pop a registrant from the waiting list"
                ]
            exitFailure
