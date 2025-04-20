-- | Storing the registrants in a DynamoDB table.  Uses the `Eventful` library.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators               #-}
module Zureg.Database
    ( Config (..)
    , Handle
    , withHandle
    , Transaction
    , withTransaction

    -- New stuff
    , migrate
    , insertRegistration
    , selectRegistration
    , selectRegistrationByEmail
    , selectAttending
    , selectWaitlist
    , setRegistrationState
    , setRegistrationScanned
    , insertProject
    , selectRegistrationsWithProjects
    ) where

import           Control.Exception          (Exception)
import           Control.Monad              (void)
import qualified Data.Text                  as T
import           Data.UUID                  (UUID)
import qualified Database.PostgreSQL.Simple as Pg
import           Zureg.Database.Internal
import           Zureg.Database.Migrations
import           Zureg.Database.Models

data DatabaseException
    = WriterException
    | DecodeException String
    | NotFoundException String
    deriving (Show)

instance Exception DatabaseException

insertRegistration :: Transaction -> InsertRegistration -> IO Registration
insertRegistration (Transaction conn) ir = do
    rows <- Pg.query conn
        "INSERT INTO registrations (\n\
        \    name,\n\
        \    badge_name,\n\
        \    email,\n\
        \    affiliation,\n\
        \    tshirt_size,\n\
        \    region,\n\
        \    occupation,\n\
        \    beginner_track_interest\n\
        \) VALUES (?, ?, ?, ?, ?, ?, ?, ?)\n\
        \RETURNING *"
        ir
    case rows of
        [registration] -> pure registration
        _              -> fail "insertRegistration: expected one row"

selectRegistration :: Transaction -> UUID -> IO (Maybe Registration)
selectRegistration (Transaction conn) uuid = do
    rows <- Pg.query conn
        "SELECT * FROM registrations WHERE id = ?"
        (Pg.Only uuid)
    case rows of
        [registration] -> pure $ Just registration
        []             -> pure Nothing
        _              -> fail
            "selectRegistration: expected one or zero rows"

selectRegistrationByEmail :: Transaction -> T.Text -> IO (Maybe Registration)
selectRegistrationByEmail (Transaction conn) email = do
    rows <- Pg.query conn
        "SELECT * FROM registrations WHERE email = ?"
        (Pg.Only email)
    case rows of
        [registration] -> pure $ Just registration
        []             -> pure Nothing
        _              -> fail
            "selectRegistrationByEmail: expected one or zero rows"

selectAttending :: Transaction -> IO Int
selectAttending (Transaction conn) = do
    rows <- Pg.query conn
        "SELECT COUNT(*) FROM registrations WHERE state = ? OR state = ?"
        (Registered, Confirmed) :: IO [Pg.Only Int]
    case rows of
        [Pg.Only c] -> pure c
        _           -> fail "selectAttending: expected one row"

-- | Select all the attendees on the waiting list in the order they joined.
selectWaitlist :: Transaction -> IO [Registration]
selectWaitlist (Transaction conn) = Pg.query conn
    "SELECT * FROM registrations WHERE state = ?\n\
    \ORDER BY registered_at ASC"
    (Pg.Only Waitlisted)

setRegistrationState :: Transaction -> UUID -> RegistrationState -> IO Registration
setRegistrationState (Transaction conn) uuid state = do
    rows <- Pg.query conn
        "UPDATE registrations SET state = ? WHERE id = ? RETURNING *"
        (state, uuid)
    case rows of
        [registration] -> pure registration
        _              -> fail "setRegistrationState: expected one row"

setRegistrationScanned :: Transaction -> UUID -> IO Registration
setRegistrationScanned (Transaction conn) uuid = do
    rows <- Pg.query conn
        "UPDATE registrations SET scanned_at = NOW() WHERE id = ? RETURNING *"
        (Pg.Only uuid)
    case rows of
        [registration] -> pure registration
        _              -> fail "setRegistrationScanned: expected one row"

insertProject :: Transaction -> Project -> IO ()
insertProject (Transaction conn) project = void $ Pg.execute conn
    "INSERT INTO projects VALUES (?, ?, ?, ?, ?, ?, ?)" project

selectRegistrationsWithProjects
    :: Transaction -> IO [(Registration, Maybe Project)]
selectRegistrationsWithProjects (Transaction conn) = fmap (fmap toTuple) $
    Pg.query_ conn
    "SELECT registrations.*, projects.* FROM registrations \n\
    \LEFT JOIN projects ON registrations.id = projects.registration_id"
  where
    toTuple (x Pg.:. y) = (x, y)
