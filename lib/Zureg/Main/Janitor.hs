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
import qualified Data.Time              as Time
import           Data.UUID              (UUID)
import qualified Zureg.Database         as Database
import qualified Zureg.Hackathon        as Hackathon
import           Zureg.Hackathon        (Hackathon)
import           Zureg.Main.PopWaitlist (popWaitinglistUUIDs)
import           Zureg.Model

countByState ::(RegisterState -> Bool) -> [Registrant] -> Int
countByState f registrants = length $ filter f $ mapMaybe rState registrants

isWaiting :: RegisterState -> Bool
isWaiting Waitlisted = True
isWaiting _          = False

isConfirmed :: RegisterState -> Bool
isConfirmed Confirmed = True
isConfirmed _         = False

isAttending :: RegisterState -> Bool
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
    registrants <- mapM (Database.getRegistrant db) uuids :: IO [Registrant]

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

-- This is to put Nothings to the end of a sorted list
newtype Fifo = Fifo (Maybe Time.UTCTime) deriving Eq

instance Ord Fifo where
    compare (Fifo Nothing)  (Fifo Nothing)  = EQ
    compare (Fifo Nothing)  (Fifo (Just _)) = GT
    compare (Fifo (Just _)) (Fifo Nothing)  = LT
    compare (Fifo (Just x)) (Fifo (Just y)) = compare x y

waitingListUUIDs :: [Registrant] -> [UUID]
waitingListUUIDs = map rUuid
                 . sortOn (Fifo . fmap riRegisteredAt . rInfo)
                 . mapMaybe (\r -> do
                                s <- rState r
                                guard $ isWaiting s
                                return r)
