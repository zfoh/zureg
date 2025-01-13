{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.PopWaitlist
    ( main
    , popWaitinglistUUIDs
    ) where

import           Control.Monad             (forM, forM_, when)
import qualified Data.Text                 as T
import           Data.UUID                 (UUID)
import qualified Data.UUID                 as UUID
import           System.Environment        (getArgs, getProgName)
import           System.Exit               (exitFailure)
import qualified System.IO                 as IO
import qualified Zureg.Database            as Database
import           Zureg.Database.Models
import           Zureg.Hackathon           (Hackathon (..))
import qualified Zureg.SendEmail           as SendEmail
import           Zureg.SendEmail.Hardcoded

popWaitinglistUUIDs :: Database.Config
                    -> Hackathon
                    -> [UUID]
                    -> IO ()
popWaitinglistUUIDs dbConfig hackathon uuids =
    Database.withHandle dbConfig $ \db ->
    SendEmail.withHandle $ \mailer ->
    forM_ uuids $ \uuid -> do
        registrant <- Database.setRegistrationState db uuid Registered
        IO.hPutStrLn IO.stderr $
            "Mailing " ++ T.unpack (rEmail registrant) ++ "..."
        sendPopWaitlistEmail mailer hackathon registrant uuid
        IO.hPutStrLn IO.stderr "OK"

main :: Hackathon -> IO ()
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

    dbConfig <- Database.configFromEnv

    popWaitinglistUUIDs dbConfig hackathon uuids
