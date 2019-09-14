{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.PopWaitlist
    ( main
    ) where

import           Control.Monad             (forM, when, forM_)
import qualified Data.Aeson                as A
import qualified Data.Text                 as T
import qualified Data.Time                 as Time
import qualified Eventful                  as E
import           System.Environment        (getArgs, getProgName)
import           System.Exit               (exitFailure)
import qualified System.IO                 as IO
import qualified Zureg.Config              as Config
import qualified Zureg.Database            as Database
import qualified Zureg.Hackathon           as Hackathon
import           Zureg.Model
import qualified Zureg.SendEmail           as SendEmail
import           Zureg.SendEmail.Hardcoded

main :: forall a. (Eq a, A.FromJSON a, A.ToJSON a) => Hackathon.Handle a -> Config.Config -> IO ()
main hackathon config = do
    progName <- getProgName
    args     <- getArgs

    dbConfig    <- Config.section config "database"
    emailConfig <- Config.section config "sendEmail"

    uuids <- forM args $
        maybe (fail "could not parse uuid") return .  E.uuidFromText . T.pack

    when (null args) $ do
        IO.hPutStr IO.stderr $ unlines
            [ "Usage: " ++ progName ++ " [uuids...]"
            , ""
            , "Pop registrants from the waiting list"
            ]
        exitFailure

    Database.withHandle dbConfig $ \db ->
        SendEmail.withHandle emailConfig $ \mailer ->
        forM_ uuids $ \uuid -> do
            registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
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
            IO.hPutStrLn IO.stderr $
                "Mailing " ++ T.unpack (riEmail rinfo) ++ "..."
            sendPopWaitlistEmail mailer (Hackathon.hConfig hackathon) rinfo uuid
            IO.hPutStrLn IO.stderr "OK"
