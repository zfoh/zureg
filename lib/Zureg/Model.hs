-- | Basic datatypes and operations in our event sourcing.
{-# LANGUAGE TemplateHaskell #-}
module Zureg.Model
    ( TShirtCut (..)
    , TShirtSize (..)
    , TrackInterest (..)
    , ContributorLevel (..)
    , Project (..)
    , RegisterInfo (..)
    , Event (..)

    , Registrant (..)
    , registrantProjection
    ) where

import qualified Data.Aeson.TH.Extended as A
import qualified Data.Text              as T
import qualified Eventful               as E

--------------------------------------------------------------------------------
-- Events

data TShirtCut = Female | Male deriving (Bounded, Enum, Eq, Show)

data TShirtSize = S | M | L | XL | XXL deriving (Bounded, Enum, Eq, Show)

data TrackInterest = TrackInterest
    { tiBeginner     :: !Bool
    , tiIntermediate :: !Bool
    , tiGhcDevOps    :: !Bool
    } deriving (Show)

data ContributorLevel = ContributorLevel
    { clBeginner     :: !Bool
    , clIntermediate :: !Bool
    , clAdvanced     :: !Bool
    } deriving (Show)

data Project = Project
    { pName             :: !T.Text
    , pWebsite          :: !T.Text
    , pShortDescription :: !T.Text
    , pContributorLevel :: !ContributorLevel
    } deriving (Show)

data RegisterInfo = RegisterInfo
    { riName                :: !T.Text
    , riEmail               :: !T.Text
    , riAffiliation         :: !(Maybe T.Text)
    , riAskMeAbout          :: !(Maybe T.Text)
    , tiTrackInterest       :: !TrackInterest
    , riTShirt              :: !(Maybe (TShirtCut, TShirtSize))
    , riMentor              :: !Bool
    , riProject             :: !(Maybe Project)
    } deriving (Show)

data Event
    = Register RegisterInfo
    | Cancel
    deriving (Show)

--------------------------------------------------------------------------------
-- State

data Registrant = Registrant
    { rUuid      :: E.UUID
    , rInfo      :: Maybe RegisterInfo
    , rCancelled :: Bool
    } deriving (Show)

registrantProjection :: E.UUID -> E.Projection Registrant Event
registrantProjection uuid = E.Projection
    { E.projectionSeed         = Registrant uuid Nothing False
    , E.projectionEventHandler = \registrant event -> case event of
        Cancel     -> registrant {rCancelled = True}
        Register i -> registrant {rInfo = Just i}
    }

$(A.deriveJSON A.options ''TShirtSize)
$(A.deriveJSON A.options ''TShirtCut)
$(A.deriveJSON A.options ''TrackInterest)
$(A.deriveJSON A.options ''ContributorLevel)
$(A.deriveJSON A.options ''Project)
$(A.deriveJSON A.options ''RegisterInfo)
$(A.deriveJSON A.options ''Event)
$(A.deriveJSON A.options ''Registrant)
