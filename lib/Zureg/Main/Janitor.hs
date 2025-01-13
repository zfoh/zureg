{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.Janitor
    ( main
    , app
    ) where

import           Control.Monad          (guard)
import qualified Data.Aeson             as A
import           Data.List              (sortOn)
import           Data.Maybe
import           Data.UUID              (UUID)
import qualified Zureg.Database         as Database
import           Zureg.Database.Models
import qualified Zureg.Hackathon        as Hackathon
import           Zureg.Hackathon        (Hackathon)
import           Zureg.Main.PopWaitlist (popWaitinglistUUIDs)

countByState :: (RegistrationState -> Bool) -> [Registration] -> Int
countByState f registrants = length $ filter f $ map rState registrants

isWaiting :: RegistrationState -> Bool
isWaiting Waitlisted = True
isWaiting _          = False

isConfirmed :: RegistrationState -> Bool
isConfirmed Confirmed = True
isConfirmed _         = False

isAttending :: RegistrationState -> Bool
isAttending Confirmed  = True
isAttending Registered = True
isAttending _          = False

main :: Hackathon -> IO ()
main hackathon = do
    dbConfig <- Database.configFromEnv
    app dbConfig hackathon A.Null >>= print

app :: Database.Config -> Hackathon -> A.Value
    -> IO Database.RegistrantsSummary
app dbConfig hackathon _event =
    Database.withHandle dbConfig $ \db -> do
    uuids       <- Database.getRegistrantUuids db
    registrants <- mapM (Database.getRegistrant db) uuids :: IO [Registration]

    let capacity   = Hackathon.capacity hackathon
        attending  = countByState isAttending registrants
        freeSpaces = capacity - attending
        waitingRegistrants = waitingListUUIDs registrants
        registrantsToPop = take freeSpaces waitingRegistrants
        freeSpacesLeft = freeSpaces - length registrantsToPop
        scanned = length $ filter rScanned registrants
        spam = countByState (== Spam) registrants

    popWaitinglistUUIDs dbConfig hackathon registrantsToPop

    let summary = Database.RegistrantsSummary
            { Database.rsTotal     = length registrants
            , Database.rsWaiting   = countByState isWaiting registrants
            , Database.rsConfirmed = countByState isConfirmed registrants
            , Database.rsAttending = attending
            , Database.rsAvailable = freeSpacesLeft
            , Database.rsScanned   = scanned
            , Database.rsSpam      = spam
            }

    Database.putRegistrantsSummary db summary
    pure summary

waitingListUUIDs :: [Registration] -> [UUID]
waitingListUUIDs = map rUuid
                 . sortOn rRegisteredAt
                 . mapMaybe (\r -> do
                                let s = rState r
                                guard $ isWaiting s
                                return r)
