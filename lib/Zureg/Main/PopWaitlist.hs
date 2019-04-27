{-# LANGUAGE OverloadedStrings #-}
module Zureg.Main.PopWaitlist
    ( main
    ) where

import qualified Data.Text                 as T
import qualified Data.Time                 as Time
import qualified Eventful                  as E
import           System.Environment        (getArgs, getProgName)
import           System.Exit               (exitFailure)
import qualified System.IO                 as IO
import qualified Zureg.Config              as Config
import qualified Zureg.Database            as Database
import           Zureg.Model
import qualified Zureg.SendEmail           as SendEmail
import           Zureg.SendEmail.Hardcoded

main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs

    config      <- Config.load "zureg.json"
    dbConfig    <- Config.section config "database"
    emailConfig <- Config.section config "sendEmail"

    case args of
        [uuidstr] ->
            Database.withHandle dbConfig $ \db ->
            SendEmail.withHandle emailConfig $ \mailer -> do
                uuid <- maybe (fail "could not parse uuid") return
                    (E.uuidFromText $ T.pack uuidstr)
                registrant <- Database.getRegistrant db uuid
                event <- PopWaitlist . PopWaitlistInfo <$> Time.getCurrentTime
                let registrant' = E.projectionEventHandler
                        (registrantProjection uuid) registrant event

                -- Sanity checks
                rinfo <- case rInfo registrant' of
                    Nothing                       -> fail "missing info?"
                    _ | registrant == registrant' -> fail "not waitlisted?"
                    Just i                        -> return i

                IO.hPutStrLn IO.stderr "Writing event..."
                Database.writeEvents db uuid [event]
                IO.hPutStrLn IO.stderr "Sending email..."
                sendPopWaitlistEmail mailer rinfo uuid
                IO.hPutStrLn IO.stderr "OK"
        _ -> do
            IO.hPutStr IO.stderr $ unlines
                [ "Usage: " ++ progName ++ " <uuid>"
                , ""
                , "Pop a registrant from the waiting list"
                ]
            exitFailure
