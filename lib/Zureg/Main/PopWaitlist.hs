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
import           Data.UUID                 (UUID)
import qualified Data.UUID                 as UUID
import           System.Environment        (getArgs, getProgName)
import           System.Exit               (exitFailure)
import qualified System.IO                 as IO
import qualified Zureg.Database            as Database
import           Zureg.Hackathon           (Hackathon (..))
import           Zureg.Model
import qualified Zureg.SendEmail           as SendEmail
import           Zureg.SendEmail.Hardcoded

popWaitinglistUUIDs :: forall a. (Eq a, A.FromJSON a, A.ToJSON a)
                    => Hackathon a
                    -> [UUID]
                    -> IO ()
popWaitinglistUUIDs hackathon@Hackathon{..} uuids =
    Database.withHandle databaseConfig $ \db ->
    SendEmail.withHandle sendEmailConfig $ \mailer ->
    forM_ uuids $ \uuid -> do
        registrant <- Database.getRegistrant db uuid :: IO (Registrant a)
        event <- PopWaitlist . PopWaitlistInfo <$> Time.getCurrentTime
        let registrant' = registrant
        {-
        let registrant' = E.projectionEventHandler
                (registrantProjection uuid) registrant event
        -}

        -- Sanity checks
        rinfo <- case rInfo registrant' of
            Nothing                       -> fail "missing info?"
            _ | registrant == registrant' -> fail "not waitlisted?"
            Just i                        -> return i

        IO.hPutStrLn IO.stderr "Writing event..."
        Database.writeEvents db uuid [event :: Event a]
        IO.hPutStrLn IO.stderr $
            "Mailing " ++ T.unpack (riEmail rinfo) ++ "..."
        sendPopWaitlistEmail mailer hackathon rinfo uuid
        IO.hPutStrLn IO.stderr "OK"


main :: forall a. (Eq a, A.FromJSON a, A.ToJSON a) => Hackathon a -> IO ()
main hackathon = do
    progName <- getProgName
    args     <- getArgs

    uuids <- forM args $
        maybe (fail "could not parse uuid") return . UUID.fromText . T.pack

    when (null args) $ do
        IO.hPutStr IO.stderr $ unlines
            [ "Usage: " ++ progName ++ " [uuids...]"
            , ""
            , "Pop registrants from the waiting list"
            ]
        exitFailure

    popWaitinglistUUIDs hackathon uuids
