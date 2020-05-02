-- | Basic datatypes and operations in our event sourcing.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Zureg.Model
    ( RegisterInfo (..)
    , WaitlistInfo (..)
    , PopWaitlistInfo (..)
    , ScanInfo (..)
    , UncancelInfo (..)
    , Event (..)

    , RegisterState (..)
    , Registrant (..)
    , CustomEventHandler
    , registrantProjection

    , parseRegisterState
    , registrantRegisteredAt
    ) where

import qualified Data.Aeson.TH.Extended as A
import qualified Data.List              as L
import qualified Data.Text              as T
import qualified Data.Time              as Time
import qualified Eventful               as E
import           Text.Read              (readMaybe)

--------------------------------------------------------------------------------
-- Events

data RegisterInfo = RegisterInfo
    { riName         :: !T.Text
    , riBadgeName    :: !(Maybe T.Text)
    , riEmail        :: !T.Text
    , riAffiliation  :: !(Maybe T.Text)
    , riRegisteredAt :: !Time.UTCTime
    } deriving (Eq, Show)

data WaitlistInfo = WaitlistInfo
    { wiWaitlistedAt :: !Time.UTCTime
    } deriving (Eq, Show)

data PopWaitlistInfo = PopWaitlistInfo
    { pwiPoppedAt :: !Time.UTCTime
    } deriving (Eq, Show)

data ScanInfo = ScanInfo
    { siScannedAt :: !Time.UTCTime
    } deriving (Eq, Show)

data UncancelInfo = UncancelInfo
    { uiUncanceledAt :: !Time.UTCTime
    } deriving (Eq, Show)

data Event e a
    = Register RegisterInfo a
    | Waitlist WaitlistInfo
    | PopWaitlist PopWaitlistInfo
    | Scan ScanInfo
    | Confirm
    | Cancel
    | Uncancel UncancelInfo
    | Custom e  -- Can be used for Hackathon-specific events.
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- State

data RegisterState = Registered | Confirmed | Cancelled | Waitlisted
    deriving (Bounded, Enum, Eq, Read, Show)

data Registrant a = Registrant
    { rUuid           :: E.UUID
    , rInfo           :: Maybe RegisterInfo
    , rAdditionalInfo :: Maybe a
    , rState          :: Maybe RegisterState
    , rScanned        :: Bool
    } deriving (Eq, Show)

type CustomEventHandler e a = e -> Maybe a -> Maybe a

registrantProjection
    :: CustomEventHandler e a  -- ^ Process Hackathon-specific events.
    -> E.UUID                  -- ^ UUID for the registrant.
    -> E.Projection (Registrant a) (Event e a)
registrantProjection f uuid = E.Projection
    { E.projectionSeed         = Registrant uuid Nothing Nothing Nothing False
    , E.projectionEventHandler = \registrant event -> case event of
        Cancel     -> registrant {rState = Just Cancelled}
        Confirm    -> case rState registrant of
                        Just Registered -> registrant {rState = Just Confirmed}
                        _               -> registrant
        Register i a -> registrant {rInfo = Just i, rAdditionalInfo = Just a, rState = Just Registered}
        Waitlist _ -> registrant {rState = Just Waitlisted}
        PopWaitlist _ | Just Waitlisted <- rState registrant ->
            registrant {rState = Just Registered}
        Scan _ -> registrant {rScanned = True}
        Uncancel _ | Just Cancelled <- rState registrant ->
            registrant {rState = Just Registered}
        Custom e ->
            registrant {rAdditionalInfo = f e $ rAdditionalInfo registrant}
        _ -> registrant
    }

$(A.deriveJSON A.options ''RegisterInfo)
$(A.deriveJSON A.options ''WaitlistInfo)
$(A.deriveJSON A.options ''PopWaitlistInfo)
$(A.deriveJSON A.options ''ScanInfo)
$(A.deriveJSON A.options ''UncancelInfo)
$(A.deriveJSON A.options ''Event)
$(A.deriveJSON A.options ''RegisterState)
$(A.deriveJSON A.options ''Registrant)

--------------------------------------------------------------------------------

parseRegisterState :: String -> Either String RegisterState
parseRegisterState str = case readMaybe str of
    Just rs -> return rs
    Nothing -> Left $
        "Can't parse register state, try one of: " ++
        L.intercalate ", " (map show [minBound :: RegisterState .. maxBound])

registrantRegisteredAt :: Registrant a -> Maybe Time.UTCTime
registrantRegisteredAt registrant = riRegisteredAt <$> rInfo registrant
