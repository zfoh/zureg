-- | Basic datatypes and operations in our event sourcing.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Zureg.Model
    ( Hackathon (..)

    , TShirtCut (..)
    , TShirtSize (..)
    , TrackInterest (..)
    , ContributorLevel (..)
    , Project (..)
    , RegisterInfo (..)
    , WaitlistInfo (..)
    , PopWaitlistInfo (..)
    , ScanInfo (..)
    , Event (..)

    , RegisterState (..)
    , Registrant (..)
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
-- Hackathon

data Hackathon = Hackathon
    { hName       :: !T.Text -- ^ Name of the Hackathon, e.g. "ZuriHac 2020"
    , hBaseUrl    :: !T.Text -- ^ Base URL, e.g. "https://zureg.zfoh.ch"
    , hContactUrl :: !T.Text -- ^ URL of the hackathon homepage, e.g. "https://zfoh.ch/zurihac2019/#contact"
    , hSlackUrl   :: !T.Text -- ^ Slack URL, e.g. "https://slack.zurihac.info/"
    , hWaitlist   :: !Bool   -- ^ When 'True', new registrants are added to the waitlist
    } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Events

data TShirtCut = Female | Male deriving (Bounded, Enum, Eq, Show)

data TShirtSize = S | M | L | XL | XXL deriving (Bounded, Enum, Eq, Show)

data TrackInterest = TrackInterest
    { tiBeginner     :: !Bool
    , tiIntermediate :: !Bool
    , tiAdvanced     :: !Bool
    , tiGhcDevOps    :: !Bool
    } deriving (Eq, Show)

data ContributorLevel = ContributorLevel
    { clBeginner     :: !Bool
    , clIntermediate :: !Bool
    , clAdvanced     :: !Bool
    } deriving (Eq, Show)

data Project = Project
    { pName             :: !(Maybe T.Text)
    , pWebsite          :: !(Maybe T.Text)
    , pShortDescription :: !(Maybe T.Text)
    , pContributorLevel :: !ContributorLevel
    } deriving (Eq, Show)

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
    , riRegisteredAt  :: !Time.UTCTime
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

data Event
    = Register RegisterInfo
    | Waitlist WaitlistInfo
    | PopWaitlist PopWaitlistInfo
    | Scan ScanInfo
    | Confirm
    | Cancel
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- State

data RegisterState = Registered | Confirmed | Cancelled | Waitlisted
    deriving (Bounded, Enum, Eq, Read, Show)

data Registrant = Registrant
    { rUuid    :: E.UUID
    , rInfo    :: Maybe RegisterInfo
    , rState   :: Maybe RegisterState
    , rScanned :: Bool
    } deriving (Eq, Show)

registrantProjection :: E.UUID -> E.Projection Registrant Event
registrantProjection uuid = E.Projection
    { E.projectionSeed         = Registrant uuid Nothing Nothing False
    , E.projectionEventHandler = \registrant event -> case event of
        Cancel     -> registrant {rState = Just Cancelled}
        Confirm    -> case rState registrant of
                        Just Registered -> registrant {rState = Just Confirmed}
                        _               -> registrant
        Register i -> registrant {rInfo = Just i, rState = Just Registered}
        Waitlist _ -> registrant {rState = Just Waitlisted}
        PopWaitlist _ | Just Waitlisted <- rState registrant ->
            registrant {rState = Just Registered}
        Scan _ -> registrant {rScanned = True}
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
$(A.deriveJSON A.options ''ScanInfo)
$(A.deriveJSON A.options ''Event)
$(A.deriveJSON A.options ''Hackathon)
$(A.deriveJSON A.options ''RegisterState)
$(A.deriveJSON A.options ''Registrant)

--------------------------------------------------------------------------------

parseRegisterState :: String -> Either String RegisterState
parseRegisterState str = case readMaybe str of
    Just rs -> return rs
    Nothing -> Left $
        "Can't parse register state, try one of: " ++
        L.intercalate ", " (map show [minBound :: RegisterState .. maxBound])

registrantRegisteredAt :: Registrant -> Maybe Time.UTCTime
registrantRegisteredAt registrant = riRegisteredAt <$> rInfo registrant
