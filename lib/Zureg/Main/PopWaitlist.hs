{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.PopWaitlist
    ( main
    , popWaitinglistUUIDs
    ) where

import           Control.Monad             (forM, forM_, when)
import qualified Data.Aeson                as A
import qualified Data.Text                 as T
import qualified Data.Time                 as Time
import qualified Eventful                  as E
import           System.Environment        (getArgs, getProgName)
import           System.Exit               (exitFailure)
import qualified System.IO                 as IO
import qualified Zureg.Database            as Database
import           Zureg.Hackathon           (Hackathon (..))
import           Zureg.Model
import qualified Zureg.SendEmail           as SendEmail
import           Zureg.SendEmail.Hardcoded

popWaitinglistUUIDs
    :: forall e a. (Eq a, A.FromJSON a, A.ToJSON a, A.FromJSON e, A.ToJSON e)
    => Hackathon e a
    -> [E.UUID]
    -> IO ()
popWaitinglistUUIDs hackathon@Hackathon{..} uuids =
    Database.withHandle databaseConfig $ \db ->
    SendEmail.withHandle sendEmailConfig $ \mailer ->
    forM_ uuids $ \uuid -> do
        registrant <- Database.getRegistrant db customEventHandler uuid
            :: IO (Registrant a)
        event <- PopWaitlist . PopWaitlistInfo <$> Time.getCurrentTime
        let registrant' = E.projectionEventHandler
                (registrantProjection customEventHandler uuid) registrant event

        -- Sanity checks
        rinfo <- case rInfo registrant' of
            Nothing                       -> fail "missing info?"
            _ | registrant == registrant' -> fail "not waitlisted?"
            Just i                        -> return i

        IO.hPutStrLn IO.stderr "Writing event..."
        Database.writeEvents db uuid [event]
        IO.hPutStrLn IO.stderr $
            "Mailing " ++ T.unpack (riEmail rinfo) ++ "..."
        sendPopWaitlistEmail mailer hackathon rinfo uuid
        IO.hPutStrLn IO.stderr "OK"


main
    :: forall e a. (Eq a, A.FromJSON a, A.ToJSON a, A.FromJSON e, A.ToJSON e)
    => Hackathon e a -> IO ()
main hackathon@Hackathon {..} = do
    progName <- getProgName
    args     <- getArgs

    uuids <- forM args $
        maybe (fail "could not parse uuid") return .  E.uuidFromText . T.pack

    when (null args) $ do
        IO.hPutStr IO.stderr $ unlines
            [ "Usage: " ++ progName ++ " [uuids...]"
            , ""
            , "Pop registrants from the waiting list"
            ]
        exitFailure

    popWaitinglistUUIDs hackathon uuids
