-- | Basic datatypes and operations in our event sourcing.
{-# LANGUAGE TemplateHaskell #-}

module Zureg.Model
    ( TShirtCut (..)
    , TShirtSize (..)
    , TrackInterest (..)
    , ContributorLevel (..)
    , Project (..)
    , RegisterInfo (..)
    , WaitlistInfo (..)
    , PopWaitlistInfo (..)
    , Event (..)

    , RegisterState (..)
    , Registrant (..)
    , registrantProjection
    ) where

import qualified Data.Aeson.TH.Extended as A
import qualified Data.Text              as T
import qualified Data.Time              as Time
import qualified Eventful               as E

--------------------------------------------------------------------------------
-- Events

data TShirtCut = Female | Male deriving (Bounded, Enum, Eq, Show)

data TShirtSize = S | M | L | XL | XXL deriving (Bounded, Enum, Eq, Show)

data TrackInterest = TrackInterest
    { tiBeginner     :: !Bool
    , tiIntermediate :: !Bool
    , tiAdvanced     :: !Bool
    , tiGhcDevOps    :: !Bool
    } deriving (Show)

data ContributorLevel = ContributorLevel
    { clBeginner     :: !Bool
    , clIntermediate :: !Bool
    , clAdvanced     :: !Bool
    } deriving (Show)

data Project = Project
    { pName             :: !(Maybe T.Text)
    , pWebsite          :: !(Maybe T.Text)
    , pShortDescription :: !(Maybe T.Text)
    , pContributorLevel :: !ContributorLevel
    } deriving (Show)

data RegisterInfo = RegisterInfo
    { riName          :: !T.Text
    , riBadgeName     :: !(Maybe T.Text)
    , riEmail         :: !T.Text
    , riAffiliation   :: !(Maybe T.Text)
    , riAskMeAbout    :: !(Maybe T.Text)
    , tiTrackInterest :: !TrackInterest
    , riTShirt        :: !(Maybe (TShirtCut, TShirtSize))
    , riMentor        :: !Bool
    , riProject       :: !Project
    -- | This field is optional because we only added after registration had
    -- been opened.
    , riRegisteredAt  :: !(Maybe Time.UTCTime)
    } deriving (Show)

data WaitlistInfo = WaitlistInfo
    { wiWaitlistedAt :: !Time.UTCTime
    } deriving (Show)

data PopWaitlistInfo = PopWaitlistInfo
    { pwiPoppedAt :: !Time.UTCTime
    } deriving (Show)

data Event
    = Register RegisterInfo
    | Waitlist WaitlistInfo
    | PopWaitlist PopWaitlistInfo
    | Confirm
    | Cancel
    deriving (Show)

--------------------------------------------------------------------------------
-- State

data RegisterState = Registered | Confirmed | Cancelled | Waitlisted
    deriving (Eq, Show)

data Registrant = Registrant
    { rUuid  :: E.UUID
    , rInfo  :: Maybe RegisterInfo
    , rState :: Maybe RegisterState
    } deriving (Show)

registrantProjection :: E.UUID -> E.Projection Registrant Event
registrantProjection uuid = E.Projection
    { E.projectionSeed         = Registrant uuid Nothing Nothing
    , E.projectionEventHandler = \registrant event -> case event of
        Cancel     -> registrant {rState = Just Cancelled}
        Confirm    -> registrant {rState = Just Confirmed}
        Register i -> registrant {rInfo = Just i, rState = Just Registered}
        Waitlist _ -> registrant {rState = Just Waitlisted}
        PopWaitlist _ | Just Waitlisted <- rState registrant ->
            registrant {rState = Just Registered}
        _ -> registrant
    }

$(A.deriveJSON A.options ''TShirtSize)
$(A.deriveJSON A.options ''TShirtCut)
$(A.deriveJSON A.options ''TrackInterest)
$(A.deriveJSON A.options ''ContributorLevel)
$(A.deriveJSON A.options ''Project)
$(A.deriveJSON A.options ''RegisterInfo)
$(A.deriveJSON A.options ''WaitlistInfo)
$(A.deriveJSON A.options ''PopWaitlistInfo)
$(A.deriveJSON A.options ''Event)
$(A.deriveJSON A.options ''RegisterState)
$(A.deriveJSON A.options ''Registrant)
